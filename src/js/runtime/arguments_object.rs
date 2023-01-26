use std::collections::HashSet;

use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::{
        parser::ast,
        runtime::{builtin_function::BuiltinFunction, eval::pattern::id_string_value},
    },
    maybe, must,
};

use super::{
    abstract_operations::{
        create_data_property_or_throw, define_property_or_throw, has_own_property, set,
    },
    environment::{environment::Environment, private_environment::PrivateNameId},
    function::{get_argument, Function},
    gc::GcDeref,
    get,
    intrinsics::intrinsics::Intrinsic,
    object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
    ordinary_object::{
        ordinary_define_own_property, ordinary_delete, ordinary_get, ordinary_get_own_property,
        ordinary_object_create, ordinary_object_create_optional_proto, ordinary_set,
        OrdinaryObject,
    },
    property::{PrivateProperty, Property},
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
    type_utilities::same_object_value,
    value::StringValue,
    Context, EvalResult, Gc, Value,
};

// An unmapped arguments that is identical to an ordinary object, but has the is_arguments_object
// method overridden. This emulates an ordinary object with a [[ParameterMap]] slot described in spec.
#[repr(C)]
pub struct UnmappedArgumentsObject {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
}

impl GcDeref for UnmappedArgumentsObject {}

impl_gc_into!(UnmappedArgumentsObject, ObjectValue);

impl UnmappedArgumentsObject {
    const VTABLE: *const () = extract_object_vtable::<UnmappedArgumentsObject>();

    pub fn new(cx: &mut Context) -> Gc<UnmappedArgumentsObject> {
        let proto = cx.current_realm().get_intrinsic(Intrinsic::ObjectPrototype);
        let object = ordinary_object_create(proto);

        cx.heap
            .alloc(UnmappedArgumentsObject { _vtable: Self::VTABLE, object })
    }

    #[inline]
    fn object(&self) -> &OrdinaryObject {
        &self.object
    }

    #[inline]
    fn object_mut(&mut self) -> &mut OrdinaryObject {
        &mut self.object
    }
}

#[wrap_ordinary_object]
impl Object for UnmappedArgumentsObject {
    fn is_arguments_object(&self) -> bool {
        true
    }
}

// A mapped arguments exotic argument, as specified in:
// 10.4.4 Arguments Exotic Objects
#[repr(C)]
pub struct MappedArgumentsObject {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
    parameter_map: Gc<ObjectValue>,
}

impl GcDeref for MappedArgumentsObject {}

impl_gc_into!(MappedArgumentsObject, ObjectValue);

impl MappedArgumentsObject {
    const VTABLE: *const () = extract_object_vtable::<MappedArgumentsObject>();

    pub fn new(cx: &mut Context, parameter_map: Gc<ObjectValue>) -> Gc<MappedArgumentsObject> {
        let proto = cx.current_realm().get_intrinsic(Intrinsic::ObjectPrototype);
        let object = ordinary_object_create(proto);

        cx.heap
            .alloc(MappedArgumentsObject { _vtable: Self::VTABLE, object, parameter_map })
    }

    #[inline]
    fn object(&self) -> &OrdinaryObject {
        &self.object
    }

    #[inline]
    fn object_mut(&mut self) -> &mut OrdinaryObject {
        &mut self.object
    }
}

#[wrap_ordinary_object]
impl Object for MappedArgumentsObject {
    // 10.4.4.1 [[GetOwnProperty]]
    fn get_own_property(
        &self,
        cx: &mut Context,
        key: &PropertyKey,
    ) -> EvalResult<Option<PropertyDescriptor>> {
        let mut desc = ordinary_get_own_property(self.object(), key);
        if let Some(desc) = &mut desc {
            if must!(has_own_property(cx, self.parameter_map, key)) {
                desc.value = Some(must!(get(cx, self.parameter_map, key)));
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
        key: &PropertyKey,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        let is_mapped = must!(has_own_property(cx, self.parameter_map, key));

        let mut new_arg_desc = desc.clone();

        if is_mapped && desc.is_data_descriptor() {
            if let Some(false) = desc.is_writable {
                if desc.value.is_none() {
                    new_arg_desc.value = Some(must!(get(cx, self.parameter_map, key)));
                }
            }
        }

        if !must!(ordinary_define_own_property(cx, self.into(), key, new_arg_desc)) {
            return false.into();
        }

        if is_mapped {
            if desc.is_accessor_descriptor() {
                must!(self.parameter_map.delete(cx, key));
            } else {
                if let Some(value) = desc.value {
                    must!(set(cx, self.parameter_map, key, value, false));
                }

                if let Some(false) = desc.is_writable {
                    must!(self.parameter_map.delete(cx, key));
                }
            }
        }

        true.into()
    }

    // 10.4.4.3 [[Get]]
    fn get(&self, cx: &mut Context, key: &PropertyKey, receiver: Value) -> EvalResult<Value> {
        if must!(has_own_property(cx, self.parameter_map, key)) {
            get(cx, self.parameter_map, key)
        } else {
            ordinary_get(cx, self.into(), key, receiver)
        }
    }

    // 10.4.4.4 [[Set]]
    fn set(
        &mut self,
        cx: &mut Context,
        key: &PropertyKey,
        value: Value,
        receiver: Value,
    ) -> EvalResult<bool> {
        let is_mapped =
            if receiver.is_object() && same_object_value(self.into(), receiver.as_object()) {
                must!(has_own_property(cx, self.parameter_map, key))
            } else {
                false
            };

        if is_mapped {
            must!(set(cx, self.parameter_map, key, value, false));
        }

        ordinary_set(cx, self.into(), key, value, receiver)
    }

    // 10.4.4.5 [[Delete]]
    fn delete(&mut self, cx: &mut Context, key: &PropertyKey) -> EvalResult<bool> {
        let is_mapped = must!(has_own_property(cx, self.parameter_map, key));

        let result = maybe!(ordinary_delete(cx, self.into(), key));

        if result && is_mapped {
            must!(self.parameter_map.delete(cx, key));
        }

        result.into()
    }

    fn is_arguments_object(&self) -> bool {
        true
    }
}

// 10.4.4.6 CreateUnmappedArgumentsObject
pub fn create_unmapped_arguments_object(cx: &mut Context, arguments: &[Value]) -> Value {
    let object: Gc<ObjectValue> = UnmappedArgumentsObject::new(cx).into();

    // Set length property
    let length_desc =
        PropertyDescriptor::data(Value::smi(arguments.len() as i32), true, false, true);
    must!(define_property_or_throw(cx, object, &cx.names.length(), length_desc));

    // Set indexed argument properties
    for (i, argument) in arguments.iter().enumerate() {
        let index_key = PropertyKey::array_index(i as u32);
        must!(create_data_property_or_throw(cx, object, &index_key, *argument));
    }

    // Set @@iterator to Array.prototype.values
    let iterator_key = PropertyKey::symbol(cx.well_known_symbols.iterator);
    let iterator_value = cx
        .current_realm()
        .get_intrinsic(Intrinsic::ArrayPrototypeValues);
    let iterator_desc = PropertyDescriptor::data(iterator_value.into(), true, false, true);
    must!(define_property_or_throw(cx, object, &iterator_key, iterator_desc));

    // Set callee to throw a type error when accessed
    let throw_type_error = cx.current_realm().get_intrinsic(Intrinsic::ThrowTypeError);
    let callee_desc =
        PropertyDescriptor::accessor(Some(throw_type_error), Some(throw_type_error), false, false);
    must!(define_property_or_throw(cx, object, &cx.names.callee(), callee_desc));

    object.into()
}

// 10.4.4.7 CreateMappedArgumentsObject
pub fn create_mapped_arguments_object(
    cx: &mut Context,
    func: Gc<Function>,
    param_nodes: &[ast::FunctionParam],
    arguments: &[Value],
    env: Gc<dyn Environment>,
) -> Value {
    let parameter_map = cx.heap.alloc(ordinary_object_create_optional_proto(None));
    let mut object = MappedArgumentsObject::new(cx, parameter_map.into());

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
        let index_key = PropertyKey::array_index(i as u32);
        must!(create_data_property_or_throw(cx, object.into(), &index_key, *argument));
    }

    // Set length property
    let length_desc =
        PropertyDescriptor::data(Value::smi(arguments.len() as i32), true, false, true);
    must!(define_property_or_throw(cx, object.into(), &cx.names.length(), length_desc));

    let mut mapped_names = HashSet::new();
    for (i, parameter_name) in parameter_names.iter().enumerate().rev() {
        // Only add accessor mapping for last parameter of each name, for which an argument was
        // actually provided.
        if !mapped_names.insert(parameter_name) || i >= arguments.len() {
            continue;
        }

        let arg_accessor_environment = cx
            .heap
            .alloc(ArgAccessorEnvironment { name: *parameter_name, env });

        let mut getter =
            BuiltinFunction::create(cx, arg_getter, 0, &cx.names.empty_string(), None, None, None);
        getter.set_closure_environment(arg_accessor_environment);

        let mut setter =
            BuiltinFunction::create(cx, arg_setter, 1, &cx.names.empty_string(), None, None, None);
        setter.set_closure_environment(arg_accessor_environment);

        let desc =
            PropertyDescriptor::accessor(Some(getter.into()), Some(setter.into()), false, true);

        must!(object.parameter_map.define_own_property(
            cx,
            &PropertyKey::array_index(i as u32),
            desc
        ));
    }

    // Set @@iterator to Array.prototype.values
    let iterator_key = PropertyKey::symbol(cx.well_known_symbols.iterator);
    let iterator_value = cx
        .current_realm()
        .get_intrinsic(Intrinsic::ArrayPrototypeValues);
    let iterator_desc = PropertyDescriptor::data(iterator_value.into(), true, false, true);
    must!(define_property_or_throw(cx, object.into(), &iterator_key, iterator_desc));

    // Set callee property to the enclosing function
    let callee_desc = PropertyDescriptor::data(func.into(), true, false, true);
    must!(define_property_or_throw(cx, object.into(), &cx.names.callee(), callee_desc));

    object.into()
}

struct ArgAccessorEnvironment {
    name: Gc<StringValue>,
    env: Gc<dyn Environment>,
}

impl GcDeref for ArgAccessorEnvironment {}

// 10.4.4.7.1 MakeArgGetter
fn arg_getter(
    cx: &mut Context,
    _: Value,
    _: &[Value],
    _: Option<Gc<ObjectValue>>,
) -> EvalResult<Value> {
    let closure_environment = cx.get_closure_environment::<ArgAccessorEnvironment>();
    let name = closure_environment.name;
    let env = closure_environment.env;

    env.get_binding_value(cx, name, false)
}

// 10.4.4.7.2 MakeArgSetter
fn arg_setter(
    cx: &mut Context,
    _: Value,
    arguments: &[Value],
    _: Option<Gc<ObjectValue>>,
) -> EvalResult<Value> {
    let closure_environment = cx.get_closure_environment::<ArgAccessorEnvironment>();
    let name = closure_environment.name;
    let mut env = closure_environment.env;
    let value = get_argument(arguments, 0);

    must!(env.set_mutable_binding(cx, name, value, false));

    Value::undefined().into()
}