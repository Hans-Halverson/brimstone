use crate::{js::runtime::abstract_operations::create_data_property_or_throw, maybe, must};

use super::{
    abstract_operations::{get, has_property},
    completion::EvalResult,
    error::type_error,
    gc::Handle,
    object_value::ObjectValue,
    ordinary_object::ordinary_object_create,
    type_utilities::{is_callable, to_boolean},
    Context, Value,
};

// 6.2.5 Property Descriptor
// Direct translation of spec. Leaves room for optimization in the future.
#[derive(Clone, Copy)]
pub struct PropertyDescriptor {
    /// The [[Value]] field. None if [[Value]] field is not present.
    pub value: Option<Handle<Value>>,
    /// The [[Writable]] field. None if [[Writable]] field is not present.
    pub is_writable: Option<bool>,
    /// The [[Enumerable]] field. None if [[Enumerable]] field is not present.
    pub is_enumerable: Option<bool>,
    /// The [[Configurable]] field. None if [[Configurable]] field is not present.
    pub is_configurable: Option<bool>,
    /// Whether the [[Get]] field is present.
    pub has_get: bool,
    /// Whether the [[Set]] field is present.
    pub has_set: bool,
    /// The [[Get]] field. Default value of None if [[Get]] field is not present.
    pub get: Option<Handle<ObjectValue>>,
    /// The [[Set]] field. Default value of None if [[Set]] field is not present.
    pub set: Option<Handle<ObjectValue>>,
}

impl PropertyDescriptor {
    pub fn data(
        value: Handle<Value>,
        is_writable: bool,
        is_enumerable: bool,
        is_configurable: bool,
    ) -> PropertyDescriptor {
        PropertyDescriptor {
            value: Some(value),
            is_writable: Some(is_writable),
            is_enumerable: Some(is_enumerable),
            is_configurable: Some(is_configurable),
            has_get: false,
            has_set: false,
            get: None,
            set: None,
        }
    }

    pub fn data_value_only(value: Handle<Value>) -> PropertyDescriptor {
        PropertyDescriptor {
            value: Some(value),
            is_writable: None,
            is_enumerable: None,
            is_configurable: None,
            has_get: false,
            has_set: false,
            get: None,
            set: None,
        }
    }

    pub fn accessor(
        get: Option<Handle<ObjectValue>>,
        set: Option<Handle<ObjectValue>>,
        is_enumerable: bool,
        is_configurable: bool,
    ) -> PropertyDescriptor {
        PropertyDescriptor {
            get,
            set,
            is_enumerable: Some(is_enumerable),
            is_configurable: Some(is_configurable),
            has_get: true,
            has_set: true,
            value: None,
            is_writable: None,
        }
    }

    pub fn get_only(
        get: Option<Handle<ObjectValue>>,
        is_enumerable: bool,
        is_configurable: bool,
    ) -> PropertyDescriptor {
        PropertyDescriptor {
            get,
            set: None,
            is_enumerable: Some(is_enumerable),
            is_configurable: Some(is_configurable),
            has_get: true,
            has_set: false,
            value: None,
            is_writable: None,
        }
    }

    pub fn set_only(
        set: Option<Handle<ObjectValue>>,
        is_enumerable: bool,
        is_configurable: bool,
    ) -> PropertyDescriptor {
        PropertyDescriptor {
            get: None,
            set,
            is_enumerable: Some(is_enumerable),
            is_configurable: Some(is_configurable),
            has_get: false,
            has_set: true,
            value: None,
            is_writable: None,
        }
    }

    pub fn attributes(
        is_writable: Option<bool>,
        is_enumerable: Option<bool>,
        is_configurable: Option<bool>,
    ) -> PropertyDescriptor {
        PropertyDescriptor {
            value: None,
            is_writable,
            is_enumerable,
            is_configurable,
            has_get: false,
            has_set: false,
            get: None,
            set: None,
        }
    }

    pub fn is_writable(&self) -> bool {
        match self.is_writable {
            None => false,
            Some(is_writable) => is_writable,
        }
    }

    pub fn is_enumerable(&self) -> bool {
        match self.is_enumerable {
            None => false,
            Some(is_enumerable) => is_enumerable,
        }
    }

    pub fn is_configurable(&self) -> bool {
        match self.is_configurable {
            None => false,
            Some(is_configurable) => is_configurable,
        }
    }

    pub fn has_no_fields(&self) -> bool {
        self.value.is_none()
            && !self.has_get
            && !self.has_set
            && self.is_writable.is_none()
            && self.is_enumerable.is_none()
            && self.is_configurable.is_none()
    }

    // 6.2.5.1 IsAccessorDescriptor
    pub fn is_accessor_descriptor(&self) -> bool {
        self.has_get || self.has_set
    }

    // 6.2.5.1 IsDataDescriptor
    pub fn is_data_descriptor(&self) -> bool {
        self.value.is_some() || self.is_writable.is_some()
    }

    // 6.2.5.3 IsGenericDescriptor
    pub fn is_generic_descriptor(&self) -> bool {
        !self.is_data_descriptor() && !self.is_accessor_descriptor()
    }

    // 6.2.5.6 CompletePropertyDescriptor
    pub fn complete_property_descriptor(&mut self, cx: Context) {
        if self.is_generic_descriptor() || self.is_data_descriptor() {
            if self.value.is_none() {
                self.value = Some(cx.undefined());
            }

            if self.is_writable.is_none() {
                self.is_writable = Some(false);
            }
        } else {
            // Intentionally ignore get and set, as they are already "undefined"
        }

        if self.is_enumerable.is_none() {
            self.is_enumerable = Some(false);
        }

        if self.is_configurable.is_none() {
            self.is_configurable = Some(false);
        }
    }
}

// 6.2.5.4 FromPropertyDescriptor
pub fn from_property_descriptor(cx: Context, desc: PropertyDescriptor) -> Handle<ObjectValue> {
    let object = ordinary_object_create(cx);

    if let Some(value) = desc.value {
        must!(create_data_property_or_throw(cx, object.into(), cx.names.value(), value,));
    }

    if let Some(is_writable) = desc.is_writable {
        let is_writable_value = cx.bool(is_writable);
        must!(create_data_property_or_throw(
            cx,
            object.into(),
            cx.names.writable(),
            is_writable_value,
        ));
    }

    if desc.has_get {
        let get_value = if let Some(get) = desc.get {
            get.into()
        } else {
            cx.undefined()
        };

        must!(create_data_property_or_throw(cx, object.into(), cx.names.get(), get_value));
    }

    if desc.has_set {
        let set_value = if let Some(set) = desc.set {
            set.into()
        } else {
            cx.undefined()
        };

        must!(create_data_property_or_throw(cx, object.into(), cx.names.set_(), set_value));
    }

    if let Some(is_enumerable) = desc.is_enumerable {
        let is_enumerable_value = cx.bool(is_enumerable);
        must!(create_data_property_or_throw(
            cx,
            object.into(),
            cx.names.enumerable(),
            is_enumerable_value,
        ));
    }

    if let Some(is_configurable) = desc.is_configurable {
        let is_configurable_value = cx.bool(is_configurable);
        must!(create_data_property_or_throw(
            cx,
            object.into(),
            cx.names.configurable(),
            is_configurable_value,
        ));
    }

    object
}

// 6.2.5.5 ToPropertyDescriptor
pub fn to_property_descriptor(cx: Context, value: Handle<Value>) -> EvalResult<PropertyDescriptor> {
    if !value.is_object() {
        return type_error(cx, "property descriptor must be an object");
    }

    let object = value.as_object();

    let mut desc = PropertyDescriptor {
        value: None,
        is_writable: None,
        is_enumerable: None,
        is_configurable: None,
        has_get: false,
        has_set: false,
        get: None,
        set: None,
    };

    if maybe!(has_property(cx, object, cx.names.enumerable())) {
        let enumerable = maybe!(get(cx, object, cx.names.enumerable()));
        desc.is_enumerable = Some(to_boolean(enumerable.get()));
    }

    if maybe!(has_property(cx, object, cx.names.configurable())) {
        let configurable = maybe!(get(cx, object, cx.names.configurable()));
        desc.is_configurable = Some(to_boolean(configurable.get()));
    }

    if maybe!(has_property(cx, object, cx.names.value())) {
        desc.value = Some(maybe!(get(cx, object, cx.names.value())));
    }

    if maybe!(has_property(cx, object, cx.names.writable())) {
        let writable = maybe!(get(cx, object, cx.names.writable()));
        desc.is_writable = Some(to_boolean(writable.get()));
    }

    if maybe!(has_property(cx, object, cx.names.get())) {
        let get = maybe!(get(cx, object, cx.names.get()));
        let is_function = is_callable(get);
        if !is_function && !get.is_undefined() {
            return type_error(cx, "getter is not callable");
        }

        desc.has_get = true;
        desc.get = if is_function {
            Some(get.as_object())
        } else {
            None
        };
    }

    if maybe!(has_property(cx, object, cx.names.set_())) {
        let set = maybe!(get(cx, object, cx.names.set_()));
        let is_function = is_callable(set);
        if !is_function && !set.is_undefined() {
            return type_error(cx, "setter is not callable");
        }

        desc.has_set = true;
        desc.set = if is_function {
            Some(set.as_object())
        } else {
            None
        };
    }

    if (desc.has_get || desc.has_set) && (desc.value.is_some() || desc.is_writable.is_some()) {
        return type_error(cx, "property desriptor must be data or accesor");
    }

    desc.into()
}
