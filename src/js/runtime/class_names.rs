use crate::{
    field_offset,
    js::runtime::{
        abstract_operations::define_property_or_throw, bytecode::function::Closure,
        error::type_error, function::build_function_name, get, intrinsics::intrinsics::Intrinsic,
        object_descriptor::ObjectKind, object_value::ObjectValue,
        ordinary_object::object_create_with_optional_proto, type_utilities::is_constructor_value,
        PropertyDescriptor, PropertyKey,
    },
    maybe, must, set_uninit,
};

use super::{
    bytecode::function::BytecodeFunction,
    collections::InlineArray,
    gc::{HeapObject, HeapVisitor},
    object_descriptor::ObjectDescriptor,
    string_value::FlatString,
    Context, EvalResult, Handle, HeapPtr, Value,
};

/// A collection of information about a class that is used in a NewClass instruction.
#[repr(C)]
pub struct ClassNames {
    descriptor: HeapPtr<ObjectDescriptor>,
    /// Number of arguments in the contiguous sequence of registers passed to the NewClass
    /// instruction.
    num_arguments: usize,
    /// Location of the home object if it should be stored in the scope chain.
    home_object: Option<HomeObjectLocation>,
    /// Location of the static home object if it should be stored in the scope chain.
    static_home_object: Option<HomeObjectLocation>,
    /// Array of global names. The first `num_functions` are global var scoped functions, the rest
    /// are global vars. All names are interned strings.
    methods: InlineArray<HeapMethod>,
}

#[repr(C)]
pub struct Method {
    /// Name of this method, if statically known. If no name is provided then the name must be
    /// computed, and will be passed with the methods to a NewClass instruction.
    pub name: Option<Handle<FlatString>>,
    /// Whether this function is static
    pub is_static: bool,
    /// Whether this is a getter function
    pub is_getter: bool,
    /// Whether this is a setter function
    pub is_setter: bool,
    /// Whether this is a private function
    pub is_private: bool,
}

#[repr(C)]
struct HeapMethod {
    name: Option<HeapPtr<FlatString>>,
    is_static: bool,
    is_getter: bool,
    is_setter: bool,
    is_private: bool,
}

/// The location where a home object should be stored in the scope chain. The scope index and depth
/// are treated as the operands for a `StoreToScope` instruction.
#[repr(C)]
pub struct HomeObjectLocation {
    pub scope_index: u32,
    pub parent_depth: u32,
}

impl ClassNames {
    pub fn new(
        cx: Context,
        methods: &[Method],
        home_object: Option<HomeObjectLocation>,
        static_home_object: Option<HomeObjectLocation>,
    ) -> Handle<ClassNames> {
        let num_methods = methods.len();
        let size = Self::calculate_size_in_bytes(num_methods);
        let mut class_names = cx.alloc_uninit_with_size::<ClassNames>(size);

        set_uninit!(class_names.descriptor, cx.base_descriptors.get(ObjectKind::ClassNames));
        set_uninit!(class_names.home_object, home_object);
        set_uninit!(class_names.static_home_object, static_home_object);

        // Calculate number of arguments needed for the NewClass instruction
        let mut num_arguments = 0;

        class_names.methods.init_with_uninit(num_methods);
        for (i, method) in methods.iter().enumerate() {
            class_names.methods.set_unchecked(i, method.to_heap());

            // Each method requires one argument for the method closure, and another if it has a
            // computed name.
            if method.name.is_none() {
                num_arguments += 2;
            } else {
                num_arguments += 1;
            }
        }

        set_uninit!(class_names.num_arguments, num_arguments);

        class_names.to_handle()
    }

    const METHODS_OFFSET: usize = field_offset!(ClassNames, methods);

    fn calculate_size_in_bytes(num_methods: usize) -> usize {
        Self::METHODS_OFFSET + InlineArray::<Method>::calculate_size_in_bytes(num_methods)
    }

    pub fn num_arguments(&self) -> usize {
        self.num_arguments
    }

    pub fn num_methods(&self) -> usize {
        self.methods.len()
    }

    pub fn get_method(&self, index: usize) -> Method {
        Method::from_heap(&self.methods.as_slice()[index])
    }
}

impl HeapObject for HeapPtr<ClassNames> {
    fn byte_size(&self) -> usize {
        ClassNames::calculate_size_in_bytes(self.methods.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);

        for method in self.methods.as_mut_slice() {
            visitor.visit_pointer_opt(&mut method.name);
        }
    }
}

impl Method {
    #[inline]
    fn to_heap(&self) -> HeapMethod {
        HeapMethod {
            name: self.name.map(|n| n.get_()),
            is_static: self.is_static,
            is_getter: self.is_getter,
            is_setter: self.is_setter,
            is_private: self.is_private,
        }
    }

    #[inline]
    fn from_heap(heap_method: &HeapMethod) -> Method {
        Method {
            name: heap_method.name.map(|n| n.to_handle()),
            is_static: heap_method.is_static,
            is_getter: heap_method.is_getter,
            is_setter: heap_method.is_setter,
            is_private: heap_method.is_private,
        }
    }
}

/// ClassDefinitionEvaluation (https://tc39.es/ecma262/#sec-runtime-semantics-classdefinitionevaluation)
pub fn new_class(
    mut cx: Context,
    class_names: Handle<ClassNames>,
    constructor_function: Handle<BytecodeFunction>,
    super_class: Option<Handle<Value>>,
    method_arguments: &[Handle<Value>],
) -> EvalResult<Handle<Closure>> {
    let (proto_parent, constructor_parent) = if let Some(super_class) = super_class {
        if super_class.is_null() {
            let constructor_parent = cx.get_intrinsic(Intrinsic::FunctionPrototype);
            (None, constructor_parent)
        } else if !is_constructor_value(super_class) {
            return type_error(cx, "super class must be a constructor");
        } else {
            let super_class = super_class.as_object();
            let proto_parent = maybe!(get(cx, super_class, cx.names.prototype()));

            if proto_parent.is_object() {
                (Some(proto_parent.as_object()), super_class)
            } else if proto_parent.is_null() {
                (None, super_class)
            } else {
                return type_error(cx, "super class prototype must be an object or null");
            }
        }
    } else {
        let proto_parent = cx.get_intrinsic(Intrinsic::ObjectPrototype);
        let constructor_parent = cx.get_intrinsic(Intrinsic::FunctionPrototype);
        (Some(proto_parent), constructor_parent)
    };

    // Create the prototype and constructor for the class
    let prototype = object_create_with_optional_proto::<ObjectValue>(
        cx,
        ObjectKind::OrdinaryObject,
        proto_parent,
    )
    .to_handle();

    let scope = cx.vm().scope().to_handle();
    let constructor = Closure::new_with_proto(cx, constructor_function, scope, constructor_parent);

    // Define a `constructor` property on the prototype
    let desc = PropertyDescriptor::data(constructor.into(), true, false, true);
    must!(define_property_or_throw(cx, prototype, cx.names.constructor(), desc));

    // Define a `prototype` property on the constructor
    let desc = PropertyDescriptor::data(prototype.into(), false, false, false);
    maybe!(define_property_or_throw(cx, constructor.into(), cx.names.prototype(), desc));

    // Store the prototype as the home object if needed
    if let Some(home_object) = &class_names.home_object {
        cx.vm().store_to_scope_at_depth(
            home_object.scope_index as usize,
            home_object.parent_depth as usize,
            prototype.get_().into(),
        );
    }

    // Store the constructor as the home object if needed
    if let Some(home_object) = &class_names.static_home_object {
        cx.vm().store_to_scope_at_depth(
            home_object.scope_index as usize,
            home_object.parent_depth as usize,
            constructor.cast::<ObjectValue>().get_().into(),
        );
    }

    let mut arg_index = 0;

    // Define all methods on the prototype and constructor
    for method_index in 0..class_names.num_methods() {
        let method = class_names.get_method(method_index);

        let name: Handle<PropertyKey>;
        let mut closure: Handle<Closure>;

        if let Some(method_name) = method.name {
            name = PropertyKey::string(cx, method_name.as_string()).to_handle(cx);
            closure = method_arguments[arg_index].cast::<Closure>();
            arg_index += 1;
        } else {
            // If the method name is computed it was passed as the next argument. Name is guaranteed
            // to be the result of a ToProperty instruction.
            name = method_arguments[arg_index].cast::<PropertyKey>();
            closure = method_arguments[arg_index + 1].cast::<Closure>();
            arg_index += 2;

            // Method name was not known when closure was created, so set it on closure now
            let prefix = if method.is_getter {
                Some("get")
            } else if method.is_setter {
                Some("set")
            } else {
                None
            };

            let closure_name = build_function_name(cx, name, prefix);
            closure.set_lazy_function_name(cx, closure_name);
        }

        // Static methods are defined on the constructor, while non-static methods are defined
        // on the prototype.
        let target = if method.is_static {
            constructor.into()
        } else {
            prototype
        };

        // Define the method on the prototype or constructor
        let desc = if method.is_getter {
            PropertyDescriptor::get_only(Some(closure.into()), false, true)
        } else if method.is_setter {
            PropertyDescriptor::set_only(Some(closure.into()), false, true)
        } else {
            PropertyDescriptor::data(closure.into(), !method.is_private, false, true)
        };

        maybe!(define_property_or_throw(cx, target, name, desc));
    }

    Ok(constructor)
}
