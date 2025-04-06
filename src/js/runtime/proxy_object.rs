use std::{collections::HashSet, mem::size_of};

use crate::{extend_object, must, set_uninit};

use super::{
    abstract_operations::{
        call_object, construct, get_method, is_extensible as is_extensible_, length_of_array_like,
    },
    array_object::create_array_from_list,
    error::type_error,
    gc::{Handle, HeapObject, HeapPtr, HeapVisitor},
    get,
    intrinsics::intrinsics::Intrinsic,
    object_descriptor::ObjectKind,
    object_value::{ObjectValue, VirtualObject},
    ordinary_object::{is_compatible_property_descriptor, object_create},
    property_descriptor::{from_property_descriptor, to_property_descriptor, PropertyDescriptor},
    property_key::PropertyKey,
    rust_vtables::extract_virtual_object_vtable,
    type_utilities::{
        is_callable_object, is_constructor_object_value, same_opt_object_value_handles, same_value,
        to_boolean,
    },
    Context, EvalResult, Realm, Value,
};

// Proxy Object (https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots)
extend_object! {
    pub struct ProxyObject {
        proxy_handler: Option<HeapPtr<ObjectValue>>,
        proxy_target: Option<HeapPtr<ObjectValue>>,
        is_callable: bool,
        is_constructor: bool,
    }
}

impl ProxyObject {
    pub const VIRTUAL_OBJECT_VTABLE: *const () = extract_virtual_object_vtable::<Self>();

    pub fn new(
        cx: Context,
        proxy_target: Handle<ObjectValue>,
        proxy_handler: Handle<ObjectValue>,
        is_callable: bool,
        is_constructor: bool,
    ) -> Handle<ProxyObject> {
        let mut object =
            object_create::<ProxyObject>(cx, ObjectKind::Proxy, Intrinsic::ObjectPrototype);

        set_uninit!(object.proxy_handler, Some(*proxy_handler));
        set_uninit!(object.proxy_target, Some(*proxy_target));
        set_uninit!(object.is_callable, is_callable);
        set_uninit!(object.is_constructor, is_constructor);

        object.to_handle()
    }

    #[inline]
    pub fn handler(&self) -> Option<Handle<ObjectValue>> {
        self.proxy_handler.map(|o| o.to_handle())
    }

    #[inline]
    pub fn target(&self) -> Option<Handle<ObjectValue>> {
        self.proxy_target.map(|o| o.to_handle())
    }

    #[inline]
    pub fn is_callable(&self) -> bool {
        self.is_callable
    }

    #[inline]
    pub fn is_constructor(&self) -> bool {
        self.is_constructor
    }

    pub fn is_revoked(&self) -> bool {
        self.proxy_handler.is_none()
    }

    pub fn revoke(&mut self) {
        self.proxy_handler = None;
        self.proxy_target = None;
    }
}

impl VirtualObject for Handle<ProxyObject> {
    /// [[GetOwnProperty]] (https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-getownproperty-p)
    fn get_own_property(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
    ) -> EvalResult<Option<PropertyDescriptor>> {
        if self.is_revoked() {
            return type_error(cx, "operation attempted on revoked proxy");
        }

        let handler = self.handler().unwrap().into();
        let target = self.target().unwrap();

        let trap = get_method(cx, handler, cx.names.get_own_property_descriptor())?;

        if trap.is_none() {
            return target.get_own_property(cx, key);
        }

        let trap_arguments = [target.into(), key.to_value(cx)];
        let trap_result = call_object(cx, trap.unwrap(), handler, &trap_arguments)?;

        if trap_result.is_undefined() {
            let target_desc = target.get_own_property(cx, key)?;
            if target_desc.is_none() {
                return Ok(None);
            }

            if let Some(false) = target_desc.unwrap().is_configurable {
                return type_error(
                    cx,
                    &format!(
                        "proxy can't report a non-configurable own property '{}' as non-existent",
                        key
                    ),
                );
            }

            if !is_extensible_(cx, target)? {
                return type_error(cx, &format!("proxy can't report an existing own property '{}' as non-existent on a non-extensible object", key));
            }

            return Ok(None);
        } else if !trap_result.is_object() {
            return type_error(
                cx,
                "proxy getOwnPropertyDescriptor must return an object or undefined",
            );
        }

        let target_desc = target.get_own_property(cx, key)?;
        let is_target_extensible = is_extensible_(cx, target)?;

        let mut result_desc = to_property_descriptor(cx, trap_result)?;
        result_desc.complete_property_descriptor(cx);

        if !is_compatible_property_descriptor(cx, is_target_extensible, result_desc, target_desc) {
            return type_error(
                cx,
                &format!("proxy can't report an incompatible property descriptor for '{}'", key),
            );
        }

        if let Some(false) = result_desc.is_configurable {
            if let None | Some(PropertyDescriptor { is_configurable: Some(true), .. }) = target_desc
            {
                return type_error(cx, &format!("proxy can't report existing configurable property '{}' as non-configurable", key));
            } else if let Some(false) = result_desc.is_writable {
                if let Some(true) = target_desc.unwrap().is_writable {
                    return type_error(cx, &format!("proxy can't report a non-configurable, non-writable property '{}' as writable", key));
                }
            }
        }

        Ok(Some(result_desc))
    }

    /// [[DefineOwnProperty]] (https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-defineownproperty-p-desc)
    fn define_own_property(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        if self.is_revoked() {
            return type_error(cx, "operation attempted on revoked proxy");
        }

        let handler = self.handler().unwrap().into();
        let mut target = self.target().unwrap();

        let trap = get_method(cx, handler, cx.names.define_property())?;

        if trap.is_none() {
            return target.define_own_property(cx, key, desc);
        }

        let desc_value = from_property_descriptor(cx, desc);
        let trap_arguments = [target.into(), key.to_value(cx), desc_value.into()];
        let trap_result = call_object(cx, trap.unwrap(), handler, &trap_arguments)?;

        if !to_boolean(*trap_result) {
            return Ok(false);
        }

        let target_desc = target.get_own_property(cx, key)?;
        let is_target_extensible = is_extensible_(cx, target)?;

        let mut is_setting_non_configurable = false;
        if let Some(false) = desc.is_configurable {
            is_setting_non_configurable = true;
        }

        if target_desc.is_none() {
            if !is_target_extensible {
                return type_error(
                    cx,
                    &format!(
                        "proxy can't define a new property '{}' on a non-extensible object",
                        key
                    ),
                );
            } else if is_setting_non_configurable {
                return type_error(
                    cx,
                    &format!(
                        "proxy can't define a non-existent property '{}' as non-configurable",
                        key
                    ),
                );
            }
        } else {
            if !is_compatible_property_descriptor(cx, is_target_extensible, desc, target_desc) {
                return type_error(
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
                    return type_error(cx, &format!("proxy can't define an existing configurable property '{}' as non-configurable", key));
                }
            }

            if target_desc.is_data_descriptor() {
                if let Some(false) = target_desc.is_configurable {
                    if let Some(true) = target_desc.is_writable {
                        if let Some(false) = desc.is_writable {
                            return type_error(cx, &format!("proxy can't define an existing non-configurable writable property '{}' as non-writable", key));
                        }
                    }
                }
            }
        }

        Ok(true)
    }

    /// [[HasProperty]] (https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-hasproperty-p)
    fn has_property(&self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool> {
        if self.is_revoked() {
            return type_error(cx, "operation attempted on revoked proxy");
        }

        let handler = self.handler().unwrap().into();
        let target = self.target().unwrap();

        let trap = get_method(cx, handler, cx.names.has())?;

        if trap.is_none() {
            return target.has_property(cx, key);
        }

        let trap_arguments = [target.into(), key.to_value(cx)];
        let trap_result = call_object(cx, trap.unwrap(), handler, &trap_arguments)?;
        let trap_result = to_boolean(*trap_result);

        if !trap_result {
            let target_desc = target.get_own_property(cx, key)?;
            if let Some(desc) = target_desc {
                if let Some(false) = desc.is_configurable {
                    return type_error(
                        cx,
                        &format!(
                            "proxy can't report a non-configurable own property '{}' as non-existent",
                            key
                        ),
                    );
                }

                if !is_extensible_(cx, target)? {
                    return type_error(cx, &format!("proxy can't report an existing own property '{}' as non-existent on a non-extensible object", key));
                }
            }
        }

        Ok(trap_result)
    }

    /// [[Get]] (https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-get-p-receiver)
    fn get(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
        receiver: Handle<Value>,
    ) -> EvalResult<Handle<Value>> {
        if self.is_revoked() {
            return type_error(cx, "operation attempted on revoked proxy");
        }

        let handler = self.handler().unwrap().into();
        let target = self.target().unwrap();

        let trap = get_method(cx, handler, cx.names.get())?;

        if trap.is_none() {
            return target.get(cx, key, receiver);
        }

        let trap_arguments = [target.into(), key.to_value(cx), receiver];
        let trap_result = call_object(cx, trap.unwrap(), handler, &trap_arguments)?;

        let target_desc = target.get_own_property(cx, key)?;
        if let Some(target_desc) = target_desc {
            if let Some(false) = target_desc.is_configurable {
                if target_desc.is_data_descriptor() {
                    if let Some(false) = target_desc.is_writable {
                        if !same_value(trap_result, target_desc.value.unwrap()) {
                            return type_error(cx, &format!("proxy must report the same value for the non-writable, non-configurable property '{}'", key));
                        }
                    }
                } else if target_desc.is_accessor_descriptor()
                    && target_desc.get.is_none()
                    && !trap_result.is_undefined()
                {
                    return type_error(cx, &format!("proxy must report undefined for a non-configurable accessor property '{}' without a getter", key));
                }
            }
        }

        Ok(trap_result)
    }

    /// [[Set]] (https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-set-p-v-receiver)
    fn set(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        value: Handle<Value>,
        receiver: Handle<Value>,
    ) -> EvalResult<bool> {
        if self.is_revoked() {
            return type_error(cx, "operation attempted on revoked proxy");
        }

        let handler = self.handler().unwrap().into();
        let mut target = self.target().unwrap();

        let trap = get_method(cx, handler, cx.names.set_())?;

        if trap.is_none() {
            return target.set(cx, key, value, receiver);
        }

        let trap_arguments = [target.into(), key.to_value(cx), value, receiver];
        let trap_result = call_object(cx, trap.unwrap(), handler, &trap_arguments)?;

        if !to_boolean(*trap_result) {
            return Ok(false);
        }

        let target_desc = target.get_own_property(cx, key)?;
        if let Some(target_desc) = target_desc {
            if let Some(false) = target_desc.is_configurable {
                if target_desc.is_data_descriptor() {
                    if let Some(false) = target_desc.is_writable {
                        if !same_value(value, target_desc.value.unwrap()) {
                            return type_error(cx, &format!("proxy can't successfully set a non-writable, non-configurable property '{}'", key));
                        }
                    }
                } else if target_desc.is_accessor_descriptor() && target_desc.set.is_none() {
                    return type_error(cx, &format!("proxy can't succesfully set an accessor property '{}' without a setter", key));
                }
            }
        }

        Ok(true)
    }

    /// [[Delete]] (https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-delete-p)
    fn delete(&mut self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool> {
        if self.is_revoked() {
            return type_error(cx, "operation attempted on revoked proxy");
        }

        let handler = self.handler().unwrap().into();
        let mut target = self.target().unwrap();

        let trap = get_method(cx, handler, cx.names.delete_property())?;

        if trap.is_none() {
            return target.delete(cx, key);
        }

        let trap_arguments = [target.into(), key.to_value(cx)];
        let trap_result = call_object(cx, trap.unwrap(), handler, &trap_arguments)?;

        if !to_boolean(*trap_result) {
            return Ok(false);
        }

        let target_desc = target.get_own_property(cx, key)?;
        if let Some(target_desc) = target_desc {
            if let Some(false) = target_desc.is_configurable {
                return type_error(
                    cx,
                    &format!("property '{}' is non-configurable and can't be deleted", key),
                );
            }
        } else {
            return Ok(true);
        }

        if !is_extensible_(cx, target)? {
            return type_error(
                cx,
                &format!("proxy can't delete property '{}' on a non-extensible object", key),
            );
        }

        Ok(true)
    }

    /// [[OwnPropertyKeys]] (https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-ownpropertykeys)
    fn own_property_keys(&self, cx: Context) -> EvalResult<Vec<Handle<Value>>> {
        if self.is_revoked() {
            return type_error(cx, "operation attempted on revoked proxy");
        }

        let handler = self.handler().unwrap().into();
        let target = self.target().unwrap();

        let trap = get_method(cx, handler, cx.names.own_keys())?;

        if trap.is_none() {
            return target.own_property_keys(cx);
        }

        let trap_result = call_object(cx, trap.unwrap(), handler, &[target.into()])?;

        // Inlined CreateListFromArrayLike with type filtering and duplicate detection
        if !trap_result.is_object() {
            return type_error(cx, "proxy ownKeys must return an array");
        }

        let trap_result_object = trap_result.as_object();
        let length = length_of_array_like(cx, trap_result_object)?;

        let mut trap_result_keys = Vec::with_capacity(length as usize);
        let mut unchecked_result_keys = HashSet::new();

        for i in 0..length {
            let key = PropertyKey::array_index(cx, i as u32).to_handle(cx);
            let next = get(cx, trap_result_object, key)?;
            trap_result_keys.push(next);

            if !next.is_string() && !next.is_symbol() {
                return type_error(
                    cx,
                    "proxy ownKeys must return an array with only string and symbol objects",
                );
            }

            let next_key = must!(PropertyKey::from_value(cx, next)).to_handle(cx);
            if !unchecked_result_keys.insert(next_key) {
                return type_error(
                    cx,
                    &format!("proxy ownKeys can't report property '{}' more than once", key),
                );
            }
        }

        let is_extensible_target = is_extensible_(cx, target)?;
        let target_keys = target.own_property_keys(cx)?;

        let mut target_configurable_keys = vec![];
        let mut target_non_configurable_keys = vec![];

        for key in target_keys {
            let property_key = must!(PropertyKey::from_value(cx, key)).to_handle(cx);
            let desc = target.get_own_property(cx, property_key)?;

            if let Some(PropertyDescriptor { is_configurable: Some(false), .. }) = desc {
                target_non_configurable_keys.push(property_key);
            } else {
                target_configurable_keys.push(property_key);
            }
        }

        if is_extensible_target && target_non_configurable_keys.is_empty() {
            return Ok(trap_result_keys);
        }

        for key in target_non_configurable_keys {
            if !unchecked_result_keys.remove(&key) {
                return type_error(
                    cx,
                    &format!("proxy can't skip a non-configurable property '{}'", key),
                );
            }
        }

        if is_extensible_target {
            return Ok(trap_result_keys);
        }

        for key in target_configurable_keys {
            if !unchecked_result_keys.remove(&key) {
                return type_error(cx, &format!("proxy can't report an existing own property '{}' as non-existent on a non-extensible object", key));
            }
        }

        if !unchecked_result_keys.is_empty() {
            let key = unchecked_result_keys.iter().next().unwrap();
            return type_error(
                cx,
                &format!("proxy can't report a new property '{}' on a non-extensible object", key),
            );
        }

        Ok(trap_result_keys)
    }

    /// [[Call]] (https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-call-thisargument-argumentslist)
    fn call(
        &self,
        cx: Context,
        this_argument: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        if self.is_revoked() {
            return type_error(cx, "operation attempted on revoked proxy");
        }

        let handler = self.handler().unwrap().into();
        let target = self.target().unwrap();

        let trap = get_method(cx, handler, cx.names.apply())?;

        if trap.is_none() {
            return call_object(cx, target, this_argument, arguments);
        }

        let arguments_array = create_array_from_list(cx, arguments);
        let trap_arguments = [target.into(), this_argument, arguments_array.into()];
        call_object(cx, trap.unwrap(), handler, &trap_arguments)
    }

    /// [[Construct]] (https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-construct-argumentslist-newtarget)
    fn construct(
        &self,
        cx: Context,
        arguments: &[Handle<Value>],
        new_target: Handle<ObjectValue>,
    ) -> EvalResult<Handle<ObjectValue>> {
        if self.is_revoked() {
            return type_error(cx, "operation attempted on revoked proxy");
        }

        let handler = self.handler().unwrap().into();
        let target = self.target().unwrap();

        let trap = get_method(cx, handler, cx.names.construct())?;

        if trap.is_none() {
            return construct(cx, target, arguments, Some(new_target));
        }

        let arguments_array = create_array_from_list(cx, arguments);
        let trap_arguments = [target.into(), arguments_array.into(), new_target.into()];
        let new_object = call_object(cx, trap.unwrap(), handler, &trap_arguments)?;

        if !new_object.is_object() {
            return type_error(cx, "proxy constructor must return an object");
        }

        Ok(new_object.as_object())
    }

    fn get_realm(&self, cx: Context) -> EvalResult<HeapPtr<Realm>> {
        if self.is_revoked() {
            return type_error(cx, "operation attempted on revoked proxy");
        }

        self.target().unwrap().get_realm(cx)
    }
}

impl ProxyObject {
    /// [[GetPrototypeOf]] (https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-getprototypeof)
    pub fn get_prototype_of(&self, cx: Context) -> EvalResult<Option<Handle<ObjectValue>>> {
        if self.is_revoked() {
            return type_error(cx, "operation attempted on revoked proxy");
        }

        let handler = self.handler().unwrap().into();
        let target = self.target().unwrap();

        // Allocations happen after this point, so can no longer use self

        let trap = get_method(cx, handler, cx.names.get_prototype_of())?;

        if trap.is_none() {
            return target.get_prototype_of(cx);
        }

        let handler_proto = call_object(cx, trap.unwrap(), handler, &[target.into()])?;
        let handler_proto = if handler_proto.is_object() {
            Some(handler_proto.as_object())
        } else if handler_proto.is_null() {
            None
        } else {
            return type_error(cx, "proxy getPrototypeOf handler must return object or null");
        };

        if is_extensible_(cx, target)? {
            return Ok(handler_proto);
        }

        let target_proto = target.get_prototype_of(cx)?;

        if !same_opt_object_value_handles(handler_proto, target_proto) {
            return type_error(
                cx,
                "proxy getPrototypeOf handler didn't return the target object's prototype",
            );
        }

        Ok(handler_proto)
    }

    /// [[SetPrototypeOf]] (https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-setprototypeof-v)
    pub fn set_prototype_of(
        &mut self,
        cx: Context,
        proto: Option<Handle<ObjectValue>>,
    ) -> EvalResult<bool> {
        if self.is_revoked() {
            return type_error(cx, "operation attempted on revoked proxy");
        }

        let handler = self.handler().unwrap().into();
        let mut target = self.target().unwrap();

        // Allocations happen after this point, so can no longer use self

        let trap = get_method(cx, handler, cx.names.set_prototype_of())?;

        if trap.is_none() {
            return target.set_prototype_of(cx, proto);
        }

        let proto_value = match proto {
            None => cx.null(),
            Some(object_value) => object_value.into(),
        };

        let trap_arguments = [target.into(), proto_value];
        let trap_result = call_object(cx, trap.unwrap(), handler, &trap_arguments)?;

        if !to_boolean(*trap_result) {
            return Ok(false);
        }

        if is_extensible_(cx, target)? {
            return Ok(true);
        }

        let target_proto = target.get_prototype_of(cx)?;

        if !same_opt_object_value_handles(proto, target_proto) {
            return type_error(cx, "proxy setPrototypeOf handler returned true, even though the target's prototype is immutable because the target is non-extensible");
        }

        Ok(true)
    }

    /// [[IsExtensible]] (https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-isextensible)
    pub fn is_extensible(&self, cx: Context) -> EvalResult<bool> {
        if self.is_revoked() {
            return type_error(cx, "operation attempted on revoked proxy");
        }

        let handler = self.handler().unwrap().into();
        let target = self.target().unwrap();

        // Allocations happen after this point, so can no longer use self

        let trap = get_method(cx, handler, cx.names.is_extensible())?;

        if trap.is_none() {
            return is_extensible_(cx, target);
        }

        let trap_result = call_object(cx, trap.unwrap(), handler, &[target.into()])?;
        let trap_result = to_boolean(*trap_result);

        let target_result = is_extensible_(cx, target)?;

        if trap_result != target_result {
            return type_error(cx, "proxy must report same extensiblitity as target");
        }

        Ok(trap_result)
    }

    /// [[PreventExtensions]] (https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-preventextensions)
    pub fn prevent_extensions(&mut self, cx: Context) -> EvalResult<bool> {
        if self.is_revoked() {
            return type_error(cx, "operation attempted on revoked proxy");
        }

        let handler = self.handler().unwrap().into();
        let mut target = self.target().unwrap();

        // Allocations happen after this point, so can no longer use self

        let trap = get_method(cx, handler, cx.names.prevent_extensions())?;

        if trap.is_none() {
            return target.prevent_extensions(cx);
        }

        let trap_result = call_object(cx, trap.unwrap(), handler, &[target.into()])?;
        let trap_result = to_boolean(*trap_result);

        if trap_result && is_extensible_(cx, target)? {
            return type_error(cx, "proxy can't report an extensible object as non-extensible");
        }

        Ok(trap_result)
    }
}

/// ProxyCreate (https://tc39.es/ecma262/#sec-proxycreate)
pub fn proxy_create(
    cx: Context,
    target: Handle<Value>,
    handler: Handle<Value>,
) -> EvalResult<Handle<ProxyObject>> {
    if !target.is_object() {
        return type_error(cx, "proxy target must be an object");
    }

    if !handler.is_object() {
        return type_error(cx, "proxy handler must be an object");
    }

    let target_object = target.as_object();
    let handler_object = handler.as_object();

    let is_callable = is_callable_object(target_object);
    let is_constructor = is_constructor_object_value(target_object);

    let proxy = ProxyObject::new(cx, target_object, handler_object, is_callable, is_constructor);

    Ok(proxy)
}

impl HeapObject for HeapPtr<ProxyObject> {
    fn byte_size(&self) -> usize {
        size_of::<ProxyObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        visitor.visit_pointer_opt(&mut self.proxy_handler);
        visitor.visit_pointer_opt(&mut self.proxy_target);
    }
}
