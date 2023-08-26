use std::{collections::HashSet, mem::size_of};

use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    extend_object,
    js::{
        parser::ast,
        runtime::{
            builtin_function::BuiltinFunction, eval::pattern::id_string_value,
            ordinary_object::object_create_with_optional_proto,
        },
    },
    maybe, must, set_uninit,
};

use super::{
    abstract_operations::{
        create_data_property_or_throw, define_property_or_throw, has_own_property, set,
    },
    environment::environment::{DynEnvironment, HeapDynEnvironment},
    function::{get_argument, Function},
    gc::{Handle, HeapObject, HeapVisitor},
    get,
    intrinsics::intrinsics::Intrinsic,
    object_descriptor::{ObjectDescriptor, ObjectKind},
    object_value::{ObjectValue, VirtualObject},
    ordinary_object::{
        object_create, ordinary_define_own_property, ordinary_delete, ordinary_get,
        ordinary_get_own_property, ordinary_set,
    },
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
    string_value::StringValue,
    type_utilities::same_object_value_handles,
    Context, EvalResult, HeapPtr, Value,
};

// An unmapped arguments that is identical to an ordinary object, but has an arguments object
// descriptor. This emulates an ordinary object with a [[ParameterMap]] slot described in spec.
extend_object! {
    pub struct UnmappedArgumentsObject {}
}

impl UnmappedArgumentsObject {
    pub fn new(cx: Context) -> Handle<ObjectValue> {
        let object = object_create::<ObjectValue>(
            cx,
            ObjectKind::UnmappedArgumentsObject,
            Intrinsic::ObjectPrototype,
        );

        object.to_handle()
    }
}

// A mapped arguments exotic argument, as specified in:
// 10.4.4 Arguments Exotic Objects
extend_object! {
    pub struct MappedArgumentsObject {
        parameter_map: HeapPtr<ObjectValue>,
    }
}

impl MappedArgumentsObject {
    pub fn new(cx: Context, parameter_map: Handle<ObjectValue>) -> Handle<MappedArgumentsObject> {
        let mut object = object_create::<MappedArgumentsObject>(
            cx,
            ObjectKind::MappedArgumentsObject,
            Intrinsic::ObjectPrototype,
        );

        set_uninit!(object.parameter_map, parameter_map.get_());

        object.to_handle()
    }

    fn parameter_map(&self) -> Handle<ObjectValue> {
        self.parameter_map.to_handle()
    }
}

#[wrap_ordinary_object]
impl VirtualObject for Handle<MappedArgumentsObject> {
    // 10.4.4.1 [[GetOwnProperty]]
    fn get_own_property(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
    ) -> EvalResult<Option<PropertyDescriptor>> {
        let mut desc = ordinary_get_own_property(cx, self.object(), key);
        if let Some(desc) = &mut desc {
            let parameter_map = self.parameter_map();
            if must!(has_own_property(cx, parameter_map, key)) {
                desc.value = Some(must!(get(cx, parameter_map, key)));
            }
        } else {
            return None.into();
        }

        desc.into()
    }

    // 10.4.4.2 [[DefineOwnProperty]]
    fn define_own_property(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        let mut parameter_map = self.parameter_map();
        let is_mapped = must!(has_own_property(cx, parameter_map, key));

        let mut new_arg_desc = desc.clone();

        if is_mapped && desc.is_data_descriptor() {
            if let Some(false) = desc.is_writable {
                if desc.value.is_none() {
                    new_arg_desc.value = Some(must!(get(cx, parameter_map, key)));
                }
            }
        }

        if !must!(ordinary_define_own_property(cx, self.object(), key, new_arg_desc)) {
            return false.into();
        }

        if is_mapped {
            if desc.is_accessor_descriptor() {
                must!(parameter_map.delete(cx, key));
            } else {
                if let Some(value) = desc.value {
                    must!(set(cx, parameter_map, key, value, false));
                }

                if let Some(false) = desc.is_writable {
                    must!(parameter_map.delete(cx, key));
                }
            }
        }

        true.into()
    }

    // 10.4.4.3 [[Get]]
    fn get(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
        receiver: Handle<Value>,
    ) -> EvalResult<Handle<Value>> {
        let parameter_map = self.parameter_map();
        if must!(has_own_property(cx, parameter_map, key)) {
            get(cx, parameter_map, key)
        } else {
            ordinary_get(cx, self.object(), key, receiver)
        }
    }

    // 10.4.4.4 [[Set]]
    fn set(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        value: Handle<Value>,
        receiver: Handle<Value>,
    ) -> EvalResult<bool> {
        let parameter_map = self.parameter_map();
        let is_mapped = if receiver.is_object()
            && same_object_value_handles(self.object(), receiver.as_object())
        {
            must!(has_own_property(cx, parameter_map, key))
        } else {
            false
        };

        if is_mapped {
            must!(set(cx, parameter_map, key, value, false));
        }

        ordinary_set(cx, self.object(), key, value, receiver)
    }

    // 10.4.4.5 [[Delete]]
    fn delete(&mut self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool> {
        let mut parameter_map = self.parameter_map();
        let is_mapped = must!(has_own_property(cx, parameter_map, key));

        let result = maybe!(ordinary_delete(cx, self.object(), key));

        if result && is_mapped {
            must!(parameter_map.delete(cx, key));
        }

        result.into()
    }
}

// 10.4.4.6 CreateUnmappedArgumentsObject
pub fn create_unmapped_arguments_object(cx: Context, arguments: &[Handle<Value>]) -> Handle<Value> {
    let object = UnmappedArgumentsObject::new(cx).into();

    // Set length property
    let length_value = Value::smi(arguments.len() as i32).to_handle(cx);
    let length_desc = PropertyDescriptor::data(length_value, true, false, true);
    must!(define_property_or_throw(cx, object, cx.names.length(), length_desc));

    // Property key is shared between iterations
    let mut index_key = PropertyKey::uninit().to_handle(cx);

    // Set indexed argument properties
    for (i, argument) in arguments.iter().enumerate() {
        index_key.replace(PropertyKey::array_index(cx, i as u32));
        must!(create_data_property_or_throw(cx, object, index_key, *argument));
    }

    // Set @@iterator to Array.prototype.values
    let iterator_key = cx.well_known_symbols.iterator();
    let iterator_value = cx.get_intrinsic(Intrinsic::ArrayPrototypeValues);
    let iterator_desc = PropertyDescriptor::data(iterator_value.into(), true, false, true);
    must!(define_property_or_throw(cx, object, iterator_key, iterator_desc));

    // Set callee to throw a type error when accessed
    let throw_type_error = cx.get_intrinsic(Intrinsic::ThrowTypeError);
    let callee_desc =
        PropertyDescriptor::accessor(Some(throw_type_error), Some(throw_type_error), false, false);
    must!(define_property_or_throw(cx, object, cx.names.callee(), callee_desc));

    object.into()
}

// 10.4.4.7 CreateMappedArgumentsObject
pub fn create_mapped_arguments_object(
    cx: Context,
    func: Handle<Function>,
    param_nodes: &[ast::FunctionParam],
    arguments: &[Handle<Value>],
    env: DynEnvironment,
) -> Handle<Value> {
    let mut parameter_map =
        object_create_with_optional_proto::<ObjectValue>(cx, ObjectKind::OrdinaryObject, None)
            .to_handle();
    let object = MappedArgumentsObject::new(cx, parameter_map);

    // Gather parameter names. All parameters are guaranteed to be simple identifiers in order for
    // a mapped arguments object to be created.
    let parameter_names = param_nodes
        .iter()
        .map(|param| match param {
            ast::FunctionParam::Pattern(ast::Pattern::Id(id)) => id_string_value(cx, id),
            _ => unreachable!("parameter must be simple identifier"),
        })
        .collect::<Vec<_>>();

    // Property key is shared between iterations
    let mut index_key = PropertyKey::uninit().to_handle(cx);

    // Set indexed argument properties
    for (i, argument) in arguments.iter().enumerate() {
        index_key.replace(PropertyKey::array_index(cx, i as u32));
        must!(create_data_property_or_throw(cx, object.into(), index_key, *argument));
    }

    // Set length property
    let length_value = Value::smi(arguments.len() as i32).to_handle(cx);
    let length_desc = PropertyDescriptor::data(length_value, true, false, true);
    must!(define_property_or_throw(cx, object.into(), cx.names.length(), length_desc));

    // Parameter names must be deduped by placing in a HashSet. Make sure to flatten all strings
    // before placing them in the HashSet.
    let mut mapped_names = HashSet::new();
    let parameter_names = parameter_names
        .iter()
        .map(|name_string| name_string.flatten())
        .collect::<Vec<_>>();

    for (i, parameter_name) in parameter_names.iter().enumerate().rev() {
        // Only add accessor mapping for last parameter of each name, for which an argument was
        // actually provided.
        if !mapped_names.insert(parameter_name) || i >= arguments.len() {
            continue;
        }

        let arg_accessor_environment =
            ArgAccessorClosureEnvironment::new(cx, parameter_name.as_string(), env);

        let mut getter =
            BuiltinFunction::create(cx, arg_getter, 0, cx.names.empty_string(), None, None, None);
        getter.set_closure_environment(arg_accessor_environment);

        let mut setter =
            BuiltinFunction::create(cx, arg_setter, 1, cx.names.empty_string(), None, None, None);
        setter.set_closure_environment(arg_accessor_environment);

        let desc =
            PropertyDescriptor::accessor(Some(getter.into()), Some(setter.into()), false, true);

        index_key.replace(PropertyKey::array_index(cx, i as u32));
        must!(parameter_map.define_own_property(cx, index_key, desc));
    }

    // Set @@iterator to Array.prototype.values
    let iterator_key = cx.well_known_symbols.iterator();
    let iterator_value = cx.get_intrinsic(Intrinsic::ArrayPrototypeValues);
    let iterator_desc = PropertyDescriptor::data(iterator_value.into(), true, false, true);
    must!(define_property_or_throw(cx, object.into(), iterator_key, iterator_desc));

    // Set callee property to the enclosing function
    let callee_desc = PropertyDescriptor::data(func.into(), true, false, true);
    must!(define_property_or_throw(cx, object.into(), cx.names.callee(), callee_desc));

    object.into()
}

#[repr(C)]
pub struct ArgAccessorClosureEnvironment {
    descriptor: HeapPtr<ObjectDescriptor>,
    name: HeapPtr<StringValue>,
    env: HeapDynEnvironment,
}

impl ArgAccessorClosureEnvironment {
    fn new(
        cx: Context,
        name: Handle<StringValue>,
        env: DynEnvironment,
    ) -> Handle<ArgAccessorClosureEnvironment> {
        let mut arg_env = cx.alloc_uninit::<ArgAccessorClosureEnvironment>();

        set_uninit!(
            arg_env.descriptor,
            cx.base_descriptors
                .get(ObjectKind::ArgAccessorClosureEnvironment)
        );
        set_uninit!(arg_env.name, name.get_());
        set_uninit!(arg_env.env, env.to_heap());

        arg_env.to_handle()
    }

    fn name(&self) -> Handle<StringValue> {
        self.name.to_handle()
    }

    fn env(&self) -> DynEnvironment {
        DynEnvironment::from_heap(&self.env)
    }
}

// 10.4.4.7.1 MakeArgGetter
fn arg_getter(
    cx: Context,
    _: Handle<Value>,
    _: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let closure_environment_ptr = cx.get_closure_environment_ptr::<ArgAccessorClosureEnvironment>();
    let name = closure_environment_ptr.name();
    let env = closure_environment_ptr.env();

    env.get_binding_value(cx, name, false)
}

// 10.4.4.7.2 MakeArgSetter
fn arg_setter(
    cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let closure_environment_ptr = cx.get_closure_environment_ptr::<ArgAccessorClosureEnvironment>();
    let name = closure_environment_ptr.name();
    let mut env = closure_environment_ptr.env();

    let value = get_argument(cx, arguments, 0);

    must!(env.set_mutable_binding(cx, name, value, false));

    cx.undefined().into()
}

impl HeapObject for HeapPtr<MappedArgumentsObject> {
    fn byte_size(&self) -> usize {
        size_of::<MappedArgumentsObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
        visitor.visit_pointer(&mut self.parameter_map);
    }
}

impl HeapObject for HeapPtr<UnmappedArgumentsObject> {
    fn byte_size(&self) -> usize {
        size_of::<UnmappedArgumentsObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
    }
}

impl HeapObject for HeapPtr<ArgAccessorClosureEnvironment> {
    fn byte_size(&self) -> usize {
        size_of::<ArgAccessorClosureEnvironment>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer(&mut self.name);
        self.env.visit_pointers(visitor);
    }
}
