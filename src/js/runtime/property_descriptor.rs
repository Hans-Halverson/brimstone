use crate::{js::runtime::abstract_operations::create_data_property_or_throw, maybe_, must_};

use super::{
    abstract_operations::{get, has_property},
    completion::AbstractResult,
    error::type_error_,
    gc::Gc,
    object_value::ObjectValue,
    ordinary_object::{ordinary_object_create, OrdinaryObject},
    type_utilities::{is_callable, to_boolean},
    value::Value,
    Context,
};

// 6.2.5 Property Attributes
// Direct translation of spec. Leaves room for optimization in the future.
#[derive(Clone, Copy)]
pub struct PropertyDescriptor {
    pub value: Option<Value>,
    pub is_writable: Option<bool>,
    pub is_enumerable: Option<bool>,
    pub is_configurable: Option<bool>,
    pub get: Option<Gc<ObjectValue>>,
    pub set: Option<Gc<ObjectValue>>,
}

impl PropertyDescriptor {
    pub fn data(
        value: Value,
        is_writable: bool,
        is_enumerable: bool,
        is_configurable: bool,
    ) -> PropertyDescriptor {
        PropertyDescriptor {
            value: Some(value),
            is_writable: Some(is_writable),
            is_enumerable: Some(is_enumerable),
            is_configurable: Some(is_configurable),
            get: None,
            set: None,
        }
    }

    pub fn accessor(
        get: Option<Gc<ObjectValue>>,
        set: Option<Gc<ObjectValue>>,
        is_enumerable: bool,
        is_configurable: bool,
    ) -> PropertyDescriptor {
        PropertyDescriptor {
            get,
            set,
            is_enumerable: Some(is_enumerable),
            is_configurable: Some(is_configurable),
            value: None,
            is_writable: None,
        }
    }

    pub fn data_value_only(value: Value) -> PropertyDescriptor {
        PropertyDescriptor {
            value: Some(value),
            is_writable: None,
            is_enumerable: None,
            is_configurable: None,
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
            && self.get.is_none()
            && self.set.is_none()
            && self.is_writable.is_none()
            && self.is_enumerable.is_none()
            && self.is_configurable.is_none()
    }

    // 6.2.5.1 IsAccessorDescriptor
    pub fn is_accessor_descriptor(&self) -> bool {
        self.get.is_some() || self.set.is_some()
    }

    // 6.2.5.1 IsAccessorDescriptor
    pub fn is_data_descriptor(&self) -> bool {
        self.value.is_some() || self.is_writable.is_some()
    }

    // 6.2.5.3 IsGenericDescriptor
    pub fn is_generic_descriptor(&self) -> bool {
        !self.is_data_descriptor() && !self.is_generic_descriptor()
    }

    // 6.2.5.6 CompletePropertyDescriptor
    pub fn complete_property_descriptor(&mut self) {
        if self.is_generic_descriptor() || self.is_data_descriptor() {
            if self.value.is_none() {
                self.value = Some(Value::undefined());
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
pub fn from_property_descriptor(cx: &mut Context, desc: PropertyDescriptor) -> Gc<OrdinaryObject> {
    let object = ordinary_object_create(cx, "%Object.prototype%");

    if let Some(value) = desc.value {
        must_!(create_data_property_or_throw(
            cx,
            object.into(),
            "value",
            value
        ));
    }

    if let Some(is_writable) = desc.is_writable {
        must_!(create_data_property_or_throw(
            cx,
            object.into(),
            "writable",
            is_writable.into()
        ));
    }

    if let Some(get) = desc.get {
        must_!(create_data_property_or_throw(
            cx,
            object.into(),
            "get",
            get.into()
        ));
    }

    if let Some(set) = desc.set {
        must_!(create_data_property_or_throw(
            cx,
            object.into(),
            "set",
            set.into()
        ));
    }

    if let Some(is_enumerable) = desc.is_enumerable {
        must_!(create_data_property_or_throw(
            cx,
            object.into(),
            "enumerable",
            is_enumerable.into()
        ));
    }

    if let Some(is_configurable) = desc.is_configurable {
        must_!(create_data_property_or_throw(
            cx,
            object.into(),
            "configurable",
            is_configurable.into()
        ));
    }

    object
}

// 6.2.5.5 ToPropertyDescriptor
pub fn to_property_descriptor(
    cx: &mut Context,
    object: Gc<ObjectValue>,
) -> AbstractResult<PropertyDescriptor> {
    let mut desc = PropertyDescriptor {
        value: None,
        is_writable: None,
        is_enumerable: None,
        is_configurable: None,
        get: None,
        set: None,
    };

    if maybe_!(has_property(object, "enumerable")) {
        let enumerable = maybe_!(get(cx, object, "enumerable"));
        desc.is_enumerable = Some(to_boolean(enumerable));
    }

    if maybe_!(has_property(object, "configurable")) {
        let configurable = maybe_!(get(cx, object, "configurable"));
        desc.is_configurable = Some(to_boolean(configurable));
    }

    if maybe_!(has_property(object, "value")) {
        desc.value = Some(maybe_!(get(cx, object, "value")));
    }

    if maybe_!(has_property(object, "writable")) {
        let writable = maybe_!(get(cx, object, "writable"));
        desc.is_writable = Some(to_boolean(writable));
    }

    if maybe_!(has_property(object, "get")) {
        let get = maybe_!(get(cx, object, "get"));
        let is_function = is_callable(get);
        if !is_function && !get.is_undefined() {
            return type_error_(cx, "getter is not callable");
        }
        desc.get = if is_function {
            Some(get.as_object())
        } else {
            None
        };
    }

    if maybe_!(has_property(object, "set")) {
        let set = maybe_!(get(cx, object, "set"));
        let is_function = is_callable(set);
        if !is_function && !set.is_undefined() {
            return type_error_(cx, "setter is not callable");
        }
        desc.set = if is_function {
            Some(set.as_object())
        } else {
            None
        };
    }

    if (desc.get.is_some() || desc.set.is_some())
        && (desc.value.is_some() || desc.is_writable.is_some())
    {
        return type_error_(cx, "property desriptor must be data or accesor");
    }

    desc.into()
}
