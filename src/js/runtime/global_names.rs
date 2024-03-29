use std::collections::HashSet;

use crate::{
    field_offset,
    js::runtime::{error::type_error_, object_descriptor::ObjectKind},
    maybe, set_uninit,
};

use super::{
    abstract_operations::{define_property_or_throw, has_own_property, is_extensible},
    collections::InlineArray,
    gc::{HeapObject, HeapVisitor},
    object_descriptor::ObjectDescriptor,
    object_value::ObjectValue,
    scope::Scope,
    string_value::FlatString,
    Context, EvalResult, Handle, HeapPtr, PropertyDescriptor, PropertyKey,
};

#[repr(C)]
pub struct GlobalNames {
    descriptor: HeapPtr<ObjectDescriptor>,
    /// Number of functions
    num_functions: usize,
    /// Array of global names. The first `num_functions` are global var scoped functions, the rest
    /// are global vars. All names are interned strings.
    names: InlineArray<HeapPtr<FlatString>>,
}

impl GlobalNames {
    pub fn new(
        cx: Context,
        vars: HashSet<Handle<FlatString>>,
        funcs: HashSet<Handle<FlatString>>,
    ) -> Handle<GlobalNames> {
        let num_funcs = funcs.len();
        let num_names = vars.len() + num_funcs;

        let size = Self::calculate_size_in_bytes(num_names);
        let mut global_names = cx.alloc_uninit_with_size::<GlobalNames>(size);

        set_uninit!(global_names.descriptor, cx.base_descriptors.get(ObjectKind::GlobalNames));
        global_names.num_functions = 0;

        // Place function names first in the names array
        global_names.names.init_with_uninit(num_names);
        for (i, name) in funcs.iter().chain(vars.iter()).enumerate() {
            global_names.names.set_unchecked(i, name.get_());
        }

        global_names.to_handle()
    }

    fn calculate_size_in_bytes(num_names: usize) -> usize {
        let names_offset = field_offset!(GlobalNames, names);
        names_offset + InlineArray::<HeapPtr<FlatString>>::calculate_size_in_bytes(num_names)
    }

    pub fn len(&self) -> usize {
        self.names.len()
    }

    pub fn get(&self, index: usize) -> HeapPtr<FlatString> {
        self.names.as_slice()[index]
    }
}

impl HeapObject for HeapPtr<GlobalNames> {
    fn byte_size(&self) -> usize {
        GlobalNames::calculate_size_in_bytes(self.names.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);

        for name in self.names.as_mut_slice() {
            visitor.visit_pointer(name);
        }
    }
}

/// Initialize the global object with the provided var scoped names.
///
/// This includes both vars and var-scoped functions. Both will be initialized as undefined here,
/// and may be overwritten later when the corresponding declaration is evaluated.
pub fn global_init(
    cx: Context,
    global_scope: Handle<Scope>,
    global_names: Handle<GlobalNames>,
    can_delete: bool,
) -> EvalResult<()> {
    let global_object = global_scope.object();

    // Reuse handle between iterations
    let mut name_handle = Handle::<FlatString>::empty(cx);

    // First check if functions and then variables can be declared in the global object
    for i in 0..global_names.names.len() {
        let name = global_names.names.get_unchecked(i);
        name_handle.replace(*name);

        // Safe since already an interned string
        let name_key = name_handle.cast::<PropertyKey>();

        if i < global_names.num_functions {
            if !maybe!(can_declare_global_function(cx, global_object, name_key)) {
                return type_error_(
                    cx,
                    &format!("cannot declare global function {}", name_handle.get_()),
                );
            }
        } else {
            if !maybe!(can_declare_global_var(cx, global_object, name_key)) {
                return type_error_(
                    cx,
                    &format!("cannot declare global var {}", name_handle.get_()),
                );
            }
        }
    }

    // Then declare the functions and variables in the global object
    for i in 0..global_names.names.len() {
        let name = global_names.names.get_unchecked(i);
        name_handle.replace(*name);

        let name_key = name_handle.cast::<PropertyKey>();

        if i < global_names.num_functions {
            maybe!(create_global_function_binding(cx, global_object, name_key, can_delete,));
        } else {
            maybe!(create_global_var_binding(cx, global_object, name_key, can_delete,));
        }
    }

    ().into()
}

// 9.1.1.4.15 CanDeclareGlobalVar
pub fn can_declare_global_var(
    cx: Context,
    global_object: Handle<ObjectValue>,
    name_key: Handle<PropertyKey>,
) -> EvalResult<bool> {
    if maybe!(has_own_property(cx, global_object, name_key)) {
        return true.into();
    }

    is_extensible(cx, global_object)
}

// 9.1.1.4.16 CanDeclareGlobalFunction
pub fn can_declare_global_function(
    cx: Context,
    global_object: Handle<ObjectValue>,
    name_key: Handle<PropertyKey>,
) -> EvalResult<bool> {
    let existing_prop = maybe!(global_object.get_own_property(cx, name_key));

    match existing_prop {
        None => is_extensible(cx, global_object),
        Some(existing_prop) => {
            if existing_prop.is_configurable() {
                return true.into();
            }

            let result = existing_prop.is_data_descriptor()
                && existing_prop.is_writable()
                && existing_prop.is_enumerable();

            result.into()
        }
    }
}

// 9.1.1.4.17 CreateGlobalVarBinding
fn create_global_var_binding(
    cx: Context,
    global_object: Handle<ObjectValue>,
    name_key: Handle<PropertyKey>,
    can_delete: bool,
) -> EvalResult<()> {
    let has_property = maybe!(has_own_property(cx, global_object, name_key));
    let is_extensible = maybe!(is_extensible(cx, global_object));

    if !has_property && is_extensible {
        // Inlined ObjectEnvironment::CreateMutableBinding from spec
        let prop_desc = PropertyDescriptor::data(cx.undefined(), true, true, can_delete);
        maybe!(define_property_or_throw(cx, global_object, name_key, prop_desc));

        // No need for initialize_binding here since binding must already be undefined
    }

    ().into()
}

// 9.1.1.4.18 CreateGlobalFunctionBinding
fn create_global_function_binding(
    cx: Context,
    global_object: Handle<ObjectValue>,
    name_key: Handle<PropertyKey>,
    can_delete: bool,
) -> EvalResult<()> {
    let existing_prop = maybe!(global_object.get_own_property(cx, name_key));

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

    maybe!(define_property_or_throw(cx, global_object, name_key, prop_desc));

    ().into()
}
