use crate::{
    field_offset,
    js::runtime::{
        abstract_operations::{create_method_property, define_property_or_throw},
        bytecode::function::Closure,
        function::{build_function_name, define_method_property},
        object_descriptor::ObjectKind,
        ordinary_object::ordinary_object_create,
        PropertyDescriptor, PropertyKey,
    },
    maybe, set_uninit,
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
}

#[repr(C)]
struct HeapMethod {
    name: Option<HeapPtr<FlatString>>,
    is_static: bool,
    is_getter: bool,
    is_setter: bool,
}

impl ClassNames {
    pub fn new(cx: Context, methods: &[Method]) -> Handle<ClassNames> {
        let num_methods = methods.len();
        let size = Self::calculate_size_in_bytes(num_methods);
        let mut class_names = cx.alloc_uninit_with_size::<ClassNames>(size);

        set_uninit!(class_names.descriptor, cx.base_descriptors.get(ObjectKind::ClassNames));

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
        }
    }

    #[inline]
    fn from_heap(heap_method: &HeapMethod) -> Method {
        Method {
            name: heap_method.name.map(|n| n.to_handle()),
            is_static: heap_method.is_static,
            is_getter: heap_method.is_getter,
            is_setter: heap_method.is_setter,
        }
    }
}

/// 15.7.14 ClassDefinitionEvaluation
pub fn new_class(
    mut cx: Context,
    class_names: Handle<ClassNames>,
    constructor_function: Handle<BytecodeFunction>,
    method_arguments: &[Handle<Value>],
) -> EvalResult<Handle<Closure>> {
    // Create the prototype and constructor for the class
    let prototype = ordinary_object_create(cx);

    let scope = cx.vm().scope().to_handle();
    let constructor = Closure::new(cx, constructor_function, scope);

    // Define a `constructor` property on the prototype
    create_method_property(cx, prototype.into(), cx.names.constructor(), constructor.into());

    // Define a `prototype` property on the constructor
    let desc = PropertyDescriptor::data(prototype.into(), false, false, false);
    maybe!(define_property_or_throw(cx, constructor.into(), cx.names.prototype(), desc));

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
        if method.is_getter {
            let desc = PropertyDescriptor::get_only(Some(closure.into()), false, true);
            maybe!(define_property_or_throw(cx, target, name, desc));
        } else if method.is_setter {
            let desc = PropertyDescriptor::set_only(Some(closure.into()), false, true);
            maybe!(define_property_or_throw(cx, target, name, desc));
        } else {
            maybe!(define_method_property(cx, target, name, closure.into(), false));
        }
    }

    constructor.into()
}
