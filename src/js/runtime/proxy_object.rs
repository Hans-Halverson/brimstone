use std::collections::HashSet;

use crate::{impl_gc_into, js::runtime::type_utilities::same_opt_object_value, maybe, must};

use super::{
    abstract_operations::{
        call_object, construct, get_method, is_extensible as is_extensible_, length_of_array_like,
    },
    array_object::create_array_from_list,
    environment::private_environment::PrivateNameId,
    error::type_error_,
    gc::GcDeref,
    get,
    intrinsics::intrinsics::Intrinsic,
    object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
    ordinary_object::{is_compatible_property_descriptor, ordinary_object_create, OrdinaryObject},
    property::{PrivateProperty, Property},
    property_descriptor::{from_property_descriptor, to_property_descriptor, PropertyDescriptor},
    property_key::PropertyKey,
    type_utilities::{is_callable_object, is_constructor_object, same_value, to_boolean},
    Context, EvalResult, Gc, Realm, Value,
};

// 10.5 Proxy Object
pub struct ProxyObject {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
    proxy_handler: Option<Gc<ObjectValue>>,
    proxy_target: Option<Gc<ObjectValue>>,
    is_callable: bool,
    is_constructor: bool,
}

impl GcDeref for ProxyObject {}

impl_gc_into!(ProxyObject, ObjectValue);

impl ProxyObject {
    const VTABLE: *const () = extract_object_vtable::<ProxyObject>();

    pub fn new(
        cx: &mut Context,
        proxy_target: Gc<ObjectValue>,
        proxy_handler: Gc<ObjectValue>,
        is_callable: bool,
        is_constructor: bool,
    ) -> ProxyObject {
        let object_proto = cx.current_realm().get_intrinsic(Intrinsic::ObjectPrototype);
        let object = ordinary_object_create(object_proto);

        ProxyObject {
            _vtable: ProxyObject::VTABLE,
            object,
            proxy_handler: Some(proxy_handler),
            proxy_target: Some(proxy_target),
            is_callable,
            is_constructor,
        }
    }

    pub fn handler(&self) -> Option<Gc<ObjectValue>> {
        self.proxy_handler
    }

    pub fn target(&self) -> Option<Gc<ObjectValue>> {
        self.proxy_target
    }

    pub fn revoke(&mut self) {
        self.proxy_handler = None;
        self.proxy_target = None;
    }
}

impl Object for ProxyObject {
    // 10.5.1 [[GetPrototypeOf]]
    fn get_prototype_of(&self, cx: &mut Context) -> EvalResult<Option<Gc<ObjectValue>>> {
        if self.proxy_handler.is_none() {
            return type_error_(cx, "operation attempted on revoked proxy");
        }

        let handler = self.proxy_handler.unwrap().into();
        let target = self.proxy_target.unwrap();

        let trap = maybe!(get_method(cx, handler, &cx.names.get_prototype_of()));

        if trap.is_none() {
            return target.get_prototype_of(cx);
        }

        let handler_proto = maybe!(call_object(cx, trap.unwrap(), handler, &[target.into()]));
        let handler_proto = if handler_proto.is_object() {
            Some(handler_proto.as_object())
        } else if handler_proto.is_null() {
            None
        } else {
            return type_error_(cx, "proxy getPrototypeOf handler must return object or null");
        };

        if maybe!(is_extensible_(cx, target)) {
            return handler_proto.into();
        }

        let target_proto = maybe!(target.get_prototype_of(cx));

        if !same_opt_object_value(handler_proto, target_proto) {
            return type_error_(
                cx,
                "proxy getPrototypeOf handler didn't return the target object's prototype",
            );
        }

        handler_proto.into()
    }

    // 10.5.2 [[SetPrototypeOf]]
    fn set_prototype_of(
        &mut self,
        cx: &mut Context,
        proto: Option<Gc<ObjectValue>>,
    ) -> EvalResult<bool> {
        if self.proxy_handler.is_none() {
            return type_error_(cx, "operation attempted on revoked proxy");
        }

        let handler = self.proxy_handler.unwrap().into();
        let mut target = self.proxy_target.unwrap();

        let trap = maybe!(get_method(cx, handler, &cx.names.set_prototype_of()));

        if trap.is_none() {
            return target.set_prototype_of(cx, proto);
        }

        let proto_value = match proto {
            None => Value::null(),
            Some(object_value) => object_value.into(),
        };

        let trap_arguments = [target.into(), proto_value];
        let trap_result = maybe!(call_object(cx, trap.unwrap(), handler, &trap_arguments));

        if !to_boolean(trap_result) {
            return false.into();
        }

        if maybe!(is_extensible_(cx, target)) {
            return true.into();
        }

        let target_proto = maybe!(target.get_prototype_of(cx));

        if !same_opt_object_value(proto, target_proto) {
            return type_error_(cx, "proxy setPrototypeOf handler returned true, even though the target's prototype is immutable because the target is non-extensible");
        }

        true.into()
    }

    // 10.5.3 [[IsExtensible]]
    fn is_extensible(&self, cx: &mut Context) -> EvalResult<bool> {
        if self.proxy_handler.is_none() {
            return type_error_(cx, "operation attempted on revoked proxy");
        }

        let handler = self.proxy_handler.unwrap().into();
        let target = self.proxy_target.unwrap();

        let trap = maybe!(get_method(cx, handler, &cx.names.is_extensible()));

        if trap.is_none() {
            return is_extensible_(cx, target);
        }

        let trap_result = maybe!(call_object(cx, trap.unwrap(), handler, &[target.into()]));
        let trap_result = to_boolean(trap_result);

        let target_result = maybe!(is_extensible_(cx, target));

        if trap_result != target_result {
            return type_error_(cx, "proxy must report same extensiblitity as target");
        }

        trap_result.into()
    }

    // 10.5.4 [[PreventExtensions]]
    fn prevent_extensions(&mut self, cx: &mut Context) -> EvalResult<bool> {
        if self.proxy_handler.is_none() {
            return type_error_(cx, "operation attempted on revoked proxy");
        }

        let handler = self.proxy_handler.unwrap().into();
        let mut target = self.proxy_target.unwrap();

        let trap = maybe!(get_method(cx, handler, &cx.names.prevent_extensions()));

        if trap.is_none() {
            return target.prevent_extensions(cx);
        }

        let trap_result = maybe!(call_object(cx, trap.unwrap(), handler, &[target.into()]));
        let trap_result = to_boolean(trap_result);

        if trap_result {
            if maybe!(is_extensible_(cx, target)) {
                return type_error_(
                    cx,
                    "proxy can't report an extensible object as non-extensible",
                );
            }
        }

        trap_result.into()
    }

    // 10.5.5 [[GetOwnProperty]]
    fn get_own_property(
        &self,
        cx: &mut Context,
        key: &PropertyKey,
    ) -> EvalResult<Option<PropertyDescriptor>> {
        if self.proxy_handler.is_none() {
            return type_error_(cx, "operation attempted on revoked proxy");
        }

        let handler = self.proxy_handler.unwrap().into();
        let target = self.proxy_target.unwrap();

        let trap = maybe!(get_method(cx, handler, &cx.names.get_own_property_descriptor()));

        if trap.is_none() {
            return target.get_own_property(cx, key);
        }

        let trap_arguments = [target.into(), key.to_value(cx)];
        let trap_result = maybe!(call_object(cx, trap.unwrap(), handler, &trap_arguments));

        if trap_result.is_undefined() {
            let target_desc = maybe!(target.get_own_property(cx, key));
            if target_desc.is_none() {
                return None.into();
            }

            if let Some(false) = target_desc.unwrap().is_configurable {
                return type_error_(
                    cx,
                    &format!(
                        "proxy can't report a non-configurable own property '{}' as non-existent",
                        key
                    ),
                );
            }

            if !maybe!(is_extensible_(cx, target)) {
                return type_error_(cx, &format!("proxy can't report an existing own property '{}' as non-existent on a non-extensible object", key));
            }

            return None.into();
        } else if !trap_result.is_object() {
            return type_error_(
                cx,
                "proxy getOwnPropertyDescriptor must return an object or undefined",
            );
        }

        let target_desc = maybe!(target.get_own_property(cx, key));
        let is_target_extensible = maybe!(is_extensible_(cx, target));

        let mut result_desc = maybe!(to_property_descriptor(cx, trap_result));
        result_desc.complete_property_descriptor();

        if !is_compatible_property_descriptor(cx, is_target_extensible, result_desc, target_desc) {
            return type_error_(
                cx,
                &format!("proxy can't report an incompatible property descriptor for '{}'", key),
            );
        }

        if let Some(false) = result_desc.is_configurable {
            if let None | Some(PropertyDescriptor { is_configurable: Some(true), .. }) = target_desc
            {
                return type_error_(cx, &format!("proxy can't report existing configurable property '{}' as non-configurable", key));
            } else if let Some(false) = result_desc.is_writable {
                if let Some(true) = target_desc.unwrap().is_writable {
                    return type_error_(cx, &format!("proxy can't report a non-configurable, non-writable property '{}' as writable", key));
                }
            }
        }

        Some(result_desc).into()
    }

    // 10.5.6 [[DefineOwnProperty]]
    fn define_own_property(
        &mut self,
        cx: &mut Context,
        key: &PropertyKey,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        if self.proxy_handler.is_none() {
            return type_error_(cx, "operation attempted on revoked proxy");
        }

        let handler = self.proxy_handler.unwrap().into();
        let mut target = self.proxy_target.unwrap();

        let trap = maybe!(get_method(cx, handler, &cx.names.define_property()));

        if trap.is_none() {
            return target.define_own_property(cx, key, desc);
        }

        let desc_value = from_property_descriptor(cx, desc);
        let trap_arguments = [target.into(), key.to_value(cx), desc_value.into()];
        let trap_result = maybe!(call_object(cx, trap.unwrap(), handler, &trap_arguments));

        if !to_boolean(trap_result) {
            return false.into();
        }

        let target_desc = maybe!(target.get_own_property(cx, key));
        let is_target_extensible = maybe!(is_extensible_(cx, target));

        let mut is_setting_non_configurable = false;
        if let Some(false) = desc.is_configurable {
            is_setting_non_configurable = true;
        }

        if target_desc.is_none() {
            if !is_target_extensible {
                return type_error_(
                    cx,
                    &format!(
                        "proxy can't define a new property '{}' on a non-extensible object",
                        key
                    ),
                );
            } else if is_setting_non_configurable {
                return type_error_(
                    cx,
                    &format!(
                        "proxy can't define a non-existent property '{}' as non-configurable",
                        key
                    ),
                );
            }
        } else {
            if !is_compatible_property_descriptor(cx, is_target_extensible, desc, target_desc) {
                return type_error_(
                    cx,
                    &format!(
                        "proxy can't report an incompatible property descriptor for '{}'",
                        key
                    ),
                );
            }

            let target_desc = target_desc.unwrap();
            if is_setting_non_configurable {
                if let Some(true) = target_desc.is_configurable {
                    return type_error_(cx, &format!("proxy can't define an existing configurable property '{}' as non-configurable", key));
                }
            }

            if target_desc.is_data_descriptor() {
                if let Some(false) = target_desc.is_configurable {
                    if let Some(true) = target_desc.is_writable {
                        if let Some(false) = desc.is_writable {
                            return type_error_(cx, &format!("proxy can't define an existing non-configurable writable property '{}' as non-writable", key));
                        }
                    }
                }
            }
        }

        true.into()
    }

    // 10.5.7 [[HasProperty]]
    fn has_property(&self, cx: &mut Context, key: &PropertyKey) -> EvalResult<bool> {
        if self.proxy_handler.is_none() {
            return type_error_(cx, "operation attempted on revoked proxy");
        }

        let handler = self.proxy_handler.unwrap().into();
        let target = self.proxy_target.unwrap();

        let trap = maybe!(get_method(cx, handler, &cx.names.has()));

        if trap.is_none() {
            return target.has_property(cx, key);
        }

        let trap_arguments = [target.into(), key.to_value(cx)];
        let trap_result = maybe!(call_object(cx, trap.unwrap(), handler, &trap_arguments));
        let trap_result = to_boolean(trap_result);

        if !trap_result {
            let target_desc = maybe!(target.get_own_property(cx, key));
            if let Some(desc) = target_desc {
                if let Some(false) = desc.is_configurable {
                    return type_error_(
                        cx,
                        &format!(
                            "proxy can't report a non-configurable own property '{}' as non-existent",
                            key
                        ),
                    );
                }

                if !maybe!(is_extensible_(cx, target)) {
                    return type_error_(cx, &format!("proxy can't report an existing own property '{}' as non-existent on a non-extensible object", key));
                }
            }
        }

        trap_result.into()
    }

    // 10.5.8 [[Get]]
    fn get(&self, cx: &mut Context, key: &PropertyKey, receiver: Value) -> EvalResult<Value> {
        if self.proxy_handler.is_none() {
            return type_error_(cx, "operation attempted on revoked proxy");
        }

        let handler = self.proxy_handler.unwrap().into();
        let target = self.proxy_target.unwrap();

        let trap = maybe!(get_method(cx, handler, &cx.names.get()));

        if trap.is_none() {
            return target.get(cx, key, receiver);
        }

        let trap_arguments = [target.into(), key.to_value(cx), receiver];
        let trap_result = maybe!(call_object(cx, trap.unwrap(), handler, &trap_arguments));

        let target_desc = maybe!(target.get_own_property(cx, key));
        if let Some(target_desc) = target_desc {
            if let Some(false) = target_desc.is_configurable {
                if target_desc.is_data_descriptor() {
                    if let Some(false) = target_desc.is_writable {
                        if !same_value(trap_result, target_desc.value.unwrap()) {
                            return type_error_(cx, &format!("proxy must report the same value for the non-writable, non-configurable property '{}'", key));
                        }
                    }
                } else if target_desc.is_accessor_descriptor() {
                    if target_desc.get.is_none() && !trap_result.is_undefined() {
                        return type_error_(cx, &format!("proxy must report undefined for a non-configurable accessor property '{}' without a getter", key));
                    }
                }
            }
        }

        trap_result.into()
    }

    // 10.5.9 [[Set]]
    fn set(
        &mut self,
        cx: &mut Context,
        key: &PropertyKey,
        value: Value,
        receiver: Value,
    ) -> EvalResult<bool> {
        if self.proxy_handler.is_none() {
            return type_error_(cx, "operation attempted on revoked proxy");
        }

        let handler = self.proxy_handler.unwrap().into();
        let mut target = self.proxy_target.unwrap();

        let trap = maybe!(get_method(cx, handler, &cx.names.set_()));

        if trap.is_none() {
            return target.set(cx, key, value, receiver);
        }

        let trap_arguments = [target.into(), key.to_value(cx), value, receiver];
        let trap_result = maybe!(call_object(cx, trap.unwrap(), handler, &trap_arguments));

        if !to_boolean(trap_result) {
            return false.into();
        }

        let target_desc = maybe!(target.get_own_property(cx, key));
        if let Some(target_desc) = target_desc {
            if let Some(false) = target_desc.is_configurable {
                if target_desc.is_data_descriptor() {
                    if let Some(false) = target_desc.is_writable {
                        if !same_value(value, target_desc.value.unwrap()) {
                            return type_error_(cx, &format!("proxy can't successfully set a non-writable, non-configurable property '{}'", key));
                        }
                    }
                } else if target_desc.is_accessor_descriptor() && target_desc.set.is_none() {
                    return type_error_(cx, &format!("proxy can't succesfully set an accessor property '{}' without a setter", key));
                }
            }
        }

        true.into()
    }

    // 10.5.10 [[Delete]]
    fn delete(&mut self, cx: &mut Context, key: &PropertyKey) -> EvalResult<bool> {
        if self.proxy_handler.is_none() {
            return type_error_(cx, "operation attempted on revoked proxy");
        }

        let handler = self.proxy_handler.unwrap().into();
        let mut target = self.proxy_target.unwrap();

        let trap = maybe!(get_method(cx, handler, &cx.names.delete_property()));

        if trap.is_none() {
            return target.delete(cx, key);
        }

        let trap_arguments = [target.into(), key.to_value(cx)];
        let trap_result = maybe!(call_object(cx, trap.unwrap(), handler, &trap_arguments));

        if !to_boolean(trap_result) {
            return false.into();
        }

        let target_desc = maybe!(target.get_own_property(cx, key));
        if let Some(target_desc) = target_desc {
            if let Some(false) = target_desc.is_configurable {
                return type_error_(
                    cx,
                    &format!("property '{}' is non-configurable and can't be deleted", key),
                );
            }
        } else {
            return true.into();
        }

        if !maybe!(is_extensible_(cx, target)) {
            return type_error_(
                cx,
                &format!("proxy can't delete property '{}' on a non-extensible object", key),
            );
        }

        true.into()
    }

    // 10.5.11 [[OwnPropertyKeys]]
    fn own_property_keys(&self, cx: &mut Context) -> EvalResult<Vec<Value>> {
        if self.proxy_handler.is_none() {
            return type_error_(cx, "operation attempted on revoked proxy");
        }

        let handler = self.proxy_handler.unwrap().into();
        let target = self.proxy_target.unwrap();

        let trap = maybe!(get_method(cx, handler, &cx.names.own_keys()));

        if trap.is_none() {
            return target.own_property_keys(cx);
        }

        let trap_result = maybe!(call_object(cx, trap.unwrap(), handler, &[target.into()]));

        // Inlined CreateListFromArrayLike with type filtering and duplicate detection
        if !trap_result.is_object() {
            return type_error_(cx, "proxy ownKeys must return an array");
        }

        let trap_result_object = trap_result.as_object();
        let length = maybe!(length_of_array_like(cx, trap_result_object));

        let mut trap_result_keys = Vec::with_capacity(length as usize);
        let mut unchecked_result_keys = HashSet::new();

        for i in 0..length {
            let key = PropertyKey::array_index(cx, i as u32);
            let next = maybe!(get(cx, trap_result_object, &key));
            trap_result_keys.push(next);

            if !next.is_string() && !next.is_symbol() {
                return type_error_(
                    cx,
                    &format!(
                        "proxy ownKeys must return an array with only string and symbol objects"
                    ),
                );
            }

            if !unchecked_result_keys.insert(must!(PropertyKey::from_value(cx, next))) {
                return type_error_(
                    cx,
                    &format!("proxy ownKeys can't report property '{}' more than once", key),
                );
            }
        }

        let is_extensible_target = maybe!(is_extensible_(cx, target));
        let target_keys = maybe!(target.own_property_keys(cx));

        let mut target_configurable_keys = vec![];
        let mut target_non_configurable_keys = vec![];

        for key in target_keys {
            let property_key = must!(PropertyKey::from_value(cx, key));
            let desc = maybe!(target.get_own_property(cx, &property_key));

            if let Some(PropertyDescriptor { is_configurable: Some(false), .. }) = desc {
                target_non_configurable_keys.push(property_key);
            } else {
                target_configurable_keys.push(property_key);
            }
        }

        if is_extensible_target && target_non_configurable_keys.is_empty() {
            return trap_result_keys.into();
        }

        for key in target_non_configurable_keys {
            if !unchecked_result_keys.remove(&key) {
                return type_error_(
                    cx,
                    &format!("proxy can't skip a non-configurable property '{}'", key),
                );
            }
        }

        if is_extensible_target {
            return trap_result_keys.into();
        }

        for key in target_configurable_keys {
            if !unchecked_result_keys.remove(&key) {
                return type_error_(cx, &format!("proxy can't report an existing own property '{}' as non-existent on a non-extensible object", key));
            }
        }

        if !unchecked_result_keys.is_empty() {
            let key = unchecked_result_keys.iter().next().unwrap();
            return type_error_(
                cx,
                &format!("proxy can't report a new property '{}' on a non-extensible object", key),
            );
        }

        trap_result_keys.into()
    }

    // 10.5.12 [[Call]]
    fn call(
        &self,
        cx: &mut Context,
        this_argument: Value,
        arguments: &[Value],
    ) -> EvalResult<Value> {
        if self.proxy_handler.is_none() {
            return type_error_(cx, "operation attempted on revoked proxy");
        }

        let handler = self.proxy_handler.unwrap().into();
        let target = self.proxy_target.unwrap();

        let trap = maybe!(get_method(cx, handler, &cx.names.apply()));

        if trap.is_none() {
            return call_object(cx, target, this_argument, arguments);
        }

        let arguments_array = create_array_from_list(cx, arguments);
        let trap_arguments = [target.into(), this_argument, arguments_array.into()];
        call_object(cx, trap.unwrap(), handler, &trap_arguments)
    }

    // 10.5.13 [[Construct]]
    fn construct(
        &self,
        cx: &mut Context,
        arguments: &[Value],
        new_target: Gc<ObjectValue>,
    ) -> EvalResult<Gc<ObjectValue>> {
        if self.proxy_handler.is_none() {
            return type_error_(cx, "operation attempted on revoked proxy");
        }

        let handler = self.proxy_handler.unwrap().into();
        let target = self.proxy_target.unwrap();

        let trap = maybe!(get_method(cx, handler, &cx.names.construct()));

        if trap.is_none() {
            return construct(cx, target, arguments, Some(new_target));
        }

        let arguments_array = create_array_from_list(cx, arguments);
        let trap_arguments = [target.into(), arguments_array.into(), new_target.into()];
        let new_object = maybe!(call_object(cx, trap.unwrap(), handler, &trap_arguments));

        if !new_object.is_object() {
            return type_error_(cx, "proxy constructor must return an object");
        }

        new_object.as_object().into()
    }

    // Property accessors and mutators cannot be called on proxy objects
    fn get_property(&self, _: &PropertyKey) -> Option<&Property> {
        unreachable!("property accessor and mutators not called on proxy objects")
    }

    fn get_property_mut(&mut self, _: &PropertyKey) -> Option<&mut Property> {
        unreachable!("property accessor and mutators not called on proxy objects")
    }

    fn set_property(&mut self, _: &PropertyKey, _: Property) {
        unreachable!("property accessor and mutators not called on proxy objects")
    }

    fn remove_property(&mut self, _: &PropertyKey) {
        unreachable!("property accessor and mutators not called on proxy objects")
    }

    // Private property methods defer to wrapped object
    fn private_element_find(&mut self, private_id: PrivateNameId) -> Option<&mut PrivateProperty> {
        self.object.private_element_find(private_id)
    }

    fn private_field_add(
        &mut self,
        cx: &mut Context,
        private_id: PrivateNameId,
        value: Value,
    ) -> EvalResult<()> {
        self.object.private_field_add(cx, private_id, value)
    }

    fn private_method_or_accessor_add(
        &mut self,
        cx: &mut Context,
        private_id: PrivateNameId,
        private_method: PrivateProperty,
    ) -> EvalResult<()> {
        self.object
            .private_method_or_accessor_add(cx, private_id, private_method)
    }

    fn is_proxy(&self) -> bool {
        true
    }

    fn is_callable(&self) -> bool {
        self.is_callable
    }

    fn is_constructor(&self) -> bool {
        self.is_constructor
    }

    fn get_realm(&self, cx: &mut Context) -> EvalResult<Gc<Realm>> {
        if self.proxy_handler.is_none() {
            return type_error_(cx, "operation attempted on revoked proxy");
        }

        self.proxy_target.unwrap().get_realm(cx)
    }
}

// 10.5.14 ProxyCreate
pub fn proxy_create(
    cx: &mut Context,
    target: Value,
    handler: Value,
) -> EvalResult<Gc<ProxyObject>> {
    if !target.is_object() {
        return type_error_(cx, "proxy target must be an object");
    }

    if !handler.is_object() {
        return type_error_(cx, "proxy handler must be an object");
    }

    let target_object = target.as_object();
    let handler_object = handler.as_object();

    let is_callable = is_callable_object(target_object);
    let is_constructor = is_constructor_object(target_object);

    let proxy = ProxyObject::new(cx, target_object, handler_object, is_callable, is_constructor);

    cx.heap.alloc(proxy).into()
}
