use std::collections::HashSet;

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
    gc::{GcDeref, Handle, HandleValue},
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
    type_utilities::same_object_value,
    Context, EvalResult, Gc, HeapPtr, Value,
};

// An unmapped arguments that is identical to an ordinary object, but has an arguments object
// descriptor. This emulates an ordinary object with a [[ParameterMap]] slot described in spec.
pub struct UnmappedArgumentsObject;

impl UnmappedArgumentsObject {
    pub fn new(cx: &mut Context) -> Handle<ObjectValue> {
        let object = object_create::<ObjectValue>(
            cx,
            ObjectKind::UnmappedArgumentsObject,
            Intrinsic::ObjectPrototype,
        );

        Handle::from_heap(object)
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
    pub fn new(
        cx: &mut Context,
        parameter_map: Handle<ObjectValue>,
    ) -> Handle<MappedArgumentsObject> {
        let mut object = object_create::<MappedArgumentsObject>(
            cx,
            ObjectKind::MappedArgumentsObject,
            Intrinsic::ObjectPrototype,
        );

        set_uninit!(object.parameter_map, parameter_map.get_());

        Handle::from_heap(object)
    }

    fn parameter_map(&self) -> Handle<ObjectValue> {
        Handle::from_heap(self.parameter_map)
    }
}

#[wrap_ordinary_object]
impl VirtualObject for Handle<MappedArgumentsObject> {
    // 10.4.4.1 [[GetOwnProperty]]
    fn get_own_property(
        &self,
        cx: &mut Context,
        key: PropertyKey,
    ) -> EvalResult<Option<PropertyDescriptor>> {
        let mut desc = ordinary_get_own_property(self.object(), key);
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
        cx: &mut Context,
        key: PropertyKey,
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
        cx: &mut Context,
        key: PropertyKey,
        receiver: HandleValue,
    ) -> EvalResult<HandleValue> {
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
        cx: &mut Context,
        key: PropertyKey,
        value: HandleValue,
        receiver: HandleValue,
    ) -> EvalResult<bool> {
        let parameter_map = self.parameter_map();
        let is_mapped =
            if receiver.is_object() && same_object_value(self.object(), receiver.as_object()) {
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
    fn delete(&mut self, cx: &mut Context, key: PropertyKey) -> EvalResult<bool> {
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
pub fn create_unmapped_arguments_object(
    cx: &mut Context,
    arguments: &[HandleValue],
) -> HandleValue {
    let object = UnmappedArgumentsObject::new(cx).into();

    // Set length property
    let length_desc =
        PropertyDescriptor::data(Value::smi(arguments.len() as i32), true, false, true);
    must!(define_property_or_throw(cx, object, cx.names.length(), length_desc));

    // Set indexed argument properties
    for (i, argument) in arguments.iter().enumerate() {
        let index_key = PropertyKey::array_index(cx, i as u32);
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
    cx: &mut Context,
    func: Handle<Function>,
    param_nodes: &[ast::FunctionParam],
    arguments: &[HandleValue],
    env: DynEnvironment,
) -> HandleValue {
    let mut parameter_map = {
        let object =
            object_create_with_optional_proto::<ObjectValue>(cx, ObjectKind::OrdinaryObject, None);
        Handle::from_heap(object)
    };
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

    // Set indexed argument properties
    for (i, argument) in arguments.iter().enumerate() {
        let index_key = PropertyKey::array_index(cx, i as u32);
        must!(create_data_property_or_throw(cx, object.into(), index_key, *argument));
    }

    // Set length property
    let length_desc =
        PropertyDescriptor::data(Value::smi(arguments.len() as i32), true, false, true);
    must!(define_property_or_throw(cx, object.into(), cx.names.length(), length_desc));

    let mut mapped_names = HashSet::new();
    for (i, parameter_name) in parameter_names.iter().enumerate().rev() {
        // Only add accessor mapping for last parameter of each name, for which an argument was
        // actually provided.
        if !mapped_names.insert(parameter_name) || i >= arguments.len() {
            continue;
        }

        let arg_accessor_environment = ArgAccessorEnvironment::new(cx, *parameter_name, env);

        let mut getter =
            BuiltinFunction::create(cx, arg_getter, 0, cx.names.empty_string(), None, None, None);
        getter.set_closure_environment(arg_accessor_environment);

        let mut setter =
            BuiltinFunction::create(cx, arg_setter, 1, cx.names.empty_string(), None, None, None);
        setter.set_closure_environment(arg_accessor_environment);

        let desc =
            PropertyDescriptor::accessor(Some(getter.into()), Some(setter.into()), false, true);

        let key = PropertyKey::array_index(cx, i as u32);
        must!(parameter_map.define_own_property(cx, key, desc));
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
struct ArgAccessorEnvironment {
    descriptor: HeapPtr<ObjectDescriptor>,
    name: HeapPtr<StringValue>,
    env: HeapDynEnvironment,
}

impl GcDeref for ArgAccessorEnvironment {}

impl ArgAccessorEnvironment {
    fn new(
        cx: &mut Context,
        name: Handle<StringValue>,
        env: DynEnvironment,
    ) -> Handle<ArgAccessorEnvironment> {
        let mut arg_env = cx.heap.alloc_uninit::<ArgAccessorEnvironment>();

        set_uninit!(
            arg_env.descriptor,
            cx.base_descriptors
                .get(ObjectKind::ArgAccessorClosureEnvironment)
        );
        set_uninit!(arg_env.name, name.get_());
        set_uninit!(arg_env.env, env.to_heap());

        Handle::from_heap(arg_env)
    }

    fn name(&self) -> Handle<StringValue> {
        Handle::from_heap(self.name)
    }

    fn env(&self) -> DynEnvironment {
        DynEnvironment::from_heap(&self.env)
    }
}

// 10.4.4.7.1 MakeArgGetter
fn arg_getter(
    cx: &mut Context,
    _: HandleValue,
    _: &[HandleValue],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<HandleValue> {
    let closure_environment_ptr = cx.get_closure_environment_ptr::<ArgAccessorEnvironment>();
    let name = closure_environment_ptr.name();
    let env = closure_environment_ptr.env();

    env.get_binding_value(cx, name, false)
}

// 10.4.4.7.2 MakeArgSetter
fn arg_setter(
    cx: &mut Context,
    _: HandleValue,
    arguments: &[HandleValue],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<HandleValue> {
    let closure_environment_ptr = cx.get_closure_environment_ptr::<ArgAccessorEnvironment>();
    let name = closure_environment_ptr.name();
    let mut env = closure_environment_ptr.env();

    let value = get_argument(arguments, 0);

    must!(env.set_mutable_binding(cx, name, value, false));

    Value::undefined().into()
}
