use std::collections::HashSet;

use crate::{
    field_offset,
    runtime::{error::type_error, object_descriptor::ObjectKind},
    set_uninit,
};

use super::{
    abstract_operations::{define_property_or_throw, has_own_property, is_extensible},
    builtin_function::BuiltinFunction,
    collections::InlineArray,
    gc::{HeapObject, HeapVisitor},
    object_descriptor::ObjectDescriptor,
    object_value::ObjectValue,
    scope_names::ScopeNames,
    string_value::FlatString,
    Context, EvalResult, Handle, HeapPtr, PropertyDescriptor, PropertyKey, Realm, Value,
};

#[repr(C)]
pub struct GlobalNames {
    descriptor: HeapPtr<ObjectDescriptor>,
    /// Number of functions
    num_functions: usize,
    /// Scopes names for this global scope, containing all lexical names.
    scope_names: HeapPtr<ScopeNames>,
    /// Array of global names. The first `num_functions` are global var scoped functions, the rest
    /// are global vars. All names are interned strings.
    names: InlineArray<HeapPtr<FlatString>>,
}

impl GlobalNames {
    pub fn new(
        cx: Context,
        vars: HashSet<Handle<FlatString>>,
        funcs: HashSet<Handle<FlatString>>,
        scope_names: Handle<ScopeNames>,
    ) -> Handle<GlobalNames> {
        let num_funcs = funcs.len();
        let num_names = vars.len() + num_funcs;

        let size = Self::calculate_size_in_bytes(num_names);
        let mut global_names = cx.alloc_uninit_with_size::<GlobalNames>(size);

        set_uninit!(global_names.descriptor, cx.base_descriptors.get(ObjectKind::GlobalNames));
        set_uninit!(global_names.scope_names, *scope_names);
        global_names.num_functions = funcs.len();

        // Place function names first in the names array
        global_names.names.init_with_uninit(num_names);
        for (i, name) in funcs.iter().chain(vars.iter()).enumerate() {
            global_names.names.set_unchecked(i, **name);
        }

        global_names.to_handle()
    }

    fn calculate_size_in_bytes(num_names: usize) -> usize {
        let names_offset = field_offset!(GlobalNames, names);
        names_offset + InlineArray::<HeapPtr<FlatString>>::calculate_size_in_bytes(num_names)
    }

    pub fn scope_names(&self) -> Handle<ScopeNames> {
        self.scope_names.to_handle()
    }
}

impl HeapObject for HeapPtr<GlobalNames> {
    fn byte_size(&self) -> usize {
        GlobalNames::calculate_size_in_bytes(self.names.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer(&mut self.scope_names);

        for name in self.names.as_mut_slice() {
            visitor.visit_pointer(name);
        }
    }
}

pub fn create_global_declaration_instantiation_intrinsic(
    cx: Context,
    realm: Handle<Realm>,
) -> Handle<Value> {
    BuiltinFunction::create(
        cx,
        global_declaration_instantiation_runtime,
        1,
        cx.names.empty_string(),
        realm,
        None,
        None,
    )
    .into()
}

/// GlobalDeclarationInstantiation in the rust runtime, called from the script init function.
pub fn global_declaration_instantiation_runtime(
    cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
) -> EvalResult<Handle<Value>> {
    let global_names = arguments.first().unwrap().cast::<GlobalNames>();
    let realm = cx.current_realm();

    global_declaration_instantiation(cx, realm, global_names)?;

    Ok(cx.undefined())
}

/// Initialize the global object with the provided var scoped names.
///
/// This includes both vars and var-scoped functions. Both will be initialized as undefined here,
/// and may be overwritten later when the corresponding declaration is evaluated.
fn global_declaration_instantiation(
    cx: Context,
    realm: Handle<Realm>,
    global_names: Handle<GlobalNames>,
) -> EvalResult<()> {
    let global_object = realm.global_object();

    // Check whether any lexical names conflict with existing global names
    let mut lexical_names = vec![];
    for name_ptr in global_names.scope_names().name_ptrs() {
        lexical_names.push(name_ptr.to_handle());
    }
    realm.can_declare_lexical_names(cx, &lexical_names)?;

    // Check whether any var names conflict with existing global names
    realm.can_declare_var_names(cx, global_names.names.as_slice())?;

    // Reuse handle between iterations
    let mut name_handle = Handle::<FlatString>::empty(cx);

    // First check if functions and then variables can be declared in the global object
    for i in 0..global_names.names.len() {
        let name = global_names.names.get_unchecked(i);
        name_handle.replace(*name);

        // Safe since already an interned string
        let name_key = name_handle.cast::<PropertyKey>();

        if i < global_names.num_functions {
            if !can_declare_global_function(cx, global_object, name_key)? {
                return type_error(cx, &format!("cannot declare global function {}", *name_handle));
            }
        } else {
            if !can_declare_global_var(cx, global_object, name_key)? {
                return type_error(cx, &format!("cannot declare global var {}", *name_handle));
            }
        }
    }

    // Then declare the functions and variables in the global object
    for i in 0..global_names.names.len() {
        let name = global_names.names.get_unchecked(i);
        name_handle.replace(*name);

        let name_key = name_handle.cast::<PropertyKey>();

        if i < global_names.num_functions {
            create_global_function_binding(
                cx,
                global_object,
                name_key,
                /* can_delete */ false,
            )?;
        } else {
            create_global_var_binding(cx, global_object, name_key, /* can_delete */ false)?;
        }
    }

    Ok(())
}

/// HasRestrictedGlobalProperty (https://tc39.es/ecma262/#sec-hasrestrictedglobalproperty)
pub fn has_restricted_global_property(
    cx: Context,
    global_object: Handle<ObjectValue>,
    name_key: Handle<PropertyKey>,
) -> EvalResult<bool> {
    let existing_prop = global_object.get_own_property(cx, name_key)?;

    match existing_prop {
        None => Ok(false),
        Some(existing_prop) => Ok(!existing_prop.is_configurable()),
    }
}

/// CanDeclareGlobalVar (https://tc39.es/ecma262/#sec-candeclareglobalvar)
pub fn can_declare_global_var(
    cx: Context,
    global_object: Handle<ObjectValue>,
    name_key: Handle<PropertyKey>,
) -> EvalResult<bool> {
    if has_own_property(cx, global_object, name_key)? {
        return Ok(true);
    }

    is_extensible(cx, global_object)
}

/// CanDeclareGlobalFunction (https://tc39.es/ecma262/#sec-candeclareglobalfunction)
pub fn can_declare_global_function(
    cx: Context,
    global_object: Handle<ObjectValue>,
    name_key: Handle<PropertyKey>,
) -> EvalResult<bool> {
    let existing_prop = global_object.get_own_property(cx, name_key)?;

    match existing_prop {
        None => is_extensible(cx, global_object),
        Some(existing_prop) => {
            if existing_prop.is_configurable() {
                return Ok(true);
            }

            let result = existing_prop.is_data_descriptor()
                && existing_prop.is_writable()
                && existing_prop.is_enumerable();

            Ok(result)
        }
    }
}

/// CreateGlobalVarBinding (https://tc39.es/ecma262/#sec-createglobalvarbinding)
pub fn create_global_var_binding(
    cx: Context,
    global_object: Handle<ObjectValue>,
    name_key: Handle<PropertyKey>,
    can_delete: bool,
) -> EvalResult<()> {
    let has_property = has_own_property(cx, global_object, name_key)?;
    let is_extensible = is_extensible(cx, global_object)?;

    if !has_property && is_extensible {
        // Inlined ObjectEnvironment::CreateMutableBinding from spec
        let prop_desc = PropertyDescriptor::data(cx.undefined(), true, true, can_delete);
        define_property_or_throw(cx, global_object, name_key, prop_desc)?;

        // No need for initialize_binding here since binding must already be undefined
    }

    Ok(())
}

/// CreateGlobalFunctionBinding (https://tc39.es/ecma262/#sec-createglobalfunctionbinding)
pub fn create_global_function_binding(
    cx: Context,
    global_object: Handle<ObjectValue>,
    name_key: Handle<PropertyKey>,
    can_delete: bool,
) -> EvalResult<()> {
    let existing_prop = global_object.get_own_property(cx, name_key)?;

    let is_writable = match existing_prop {
        None => true,
        Some(existing_prop) => existing_prop.is_configurable(),
    };

    // Property will be set later by a StoreGlobal, for now write undefined
    let prop_desc = if is_writable {
        PropertyDescriptor::data(cx.undefined(), true, true, can_delete)
    } else {
        PropertyDescriptor::data_value_only(cx.undefined())
    };

    define_property_or_throw(cx, global_object, name_key, prop_desc)?;

    Ok(())
}
