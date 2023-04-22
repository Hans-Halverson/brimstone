use bitflags::bitflags;

use super::{
    object_value::ObjectValue,
    value::{AccessorValue, Value},
    Context, Gc,
};

// A property value. If a data property then the value is the value itself. If an accessor property
// then the value is an accessor property, which is a pointer to the get and set function pair.
//
// 6.2.9 PrivateElements are represented as properties, with bitflags noting the private property kind.
#[derive(Clone)]
pub struct Property {
    value: Value,
    flags: PropertyFlags,
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct PropertyFlags: u8 {
        const IS_WRITABLE = 1 << 0;
        const IS_ENUMERABLE = 1 << 1;
        const IS_CONFIGURABLE = 1 << 2;
        const IS_PRIVATE_FIELD = 1 << 3;
        const IS_PRIVATE_METHOD = 1 << 4;
        const IS_PRIVATE_ACCESSOR = 1 << 5;
    }
}

pub const DENSE_ARRAY_PROPERTY_FLAGS: PropertyFlags = PropertyFlags::IS_WRITABLE
    .union(PropertyFlags::IS_ENUMERABLE)
    .union(PropertyFlags::IS_CONFIGURABLE);

impl Property {
    #[inline]
    pub fn data(
        value: Value,
        is_writable: bool,
        is_enumerable: bool,
        is_configurable: bool,
    ) -> Property {
        let mut flags = PropertyFlags::empty();

        if is_writable {
            flags |= PropertyFlags::IS_WRITABLE;
        }

        if is_enumerable {
            flags |= PropertyFlags::IS_ENUMERABLE;
        }

        if is_configurable {
            flags |= PropertyFlags::IS_CONFIGURABLE;
        }

        Property { value, flags }
    }

    #[inline]
    pub fn accessor(accessor_value: Value, is_enumerable: bool, is_configurable: bool) -> Property {
        let mut flags = PropertyFlags::empty();

        if is_enumerable {
            flags |= PropertyFlags::IS_ENUMERABLE;
        }

        if is_configurable {
            flags |= PropertyFlags::IS_CONFIGURABLE;
        }

        Property { value: accessor_value, flags }
    }

    pub fn private_field(value: Value) -> Property {
        Property { value, flags: PropertyFlags::IS_PRIVATE_FIELD }
    }

    pub fn private_method(method: Gc<ObjectValue>) -> Property {
        Property {
            value: method.into(),
            flags: PropertyFlags::IS_PRIVATE_METHOD,
        }
    }

    pub fn private_getter(cx: &mut Context, getter: Gc<ObjectValue>) -> Property {
        let accessor_value = AccessorValue::new(cx, Some(getter), None);
        Property {
            value: accessor_value.into(),
            flags: PropertyFlags::IS_PRIVATE_ACCESSOR,
        }
    }

    pub fn private_setter(cx: &mut Context, setter: Gc<ObjectValue>) -> Property {
        let accessor_value = AccessorValue::new(cx, None, Some(setter));
        Property {
            value: accessor_value.into(),
            flags: PropertyFlags::IS_PRIVATE_ACCESSOR,
        }
    }

    pub fn value(&self) -> Value {
        self.value
    }

    pub fn is_enumerable(&self) -> bool {
        self.flags.contains(PropertyFlags::IS_ENUMERABLE)
    }

    pub fn is_configurable(&self) -> bool {
        self.flags.contains(PropertyFlags::IS_CONFIGURABLE)
    }

    pub fn is_writable(&self) -> bool {
        self.flags.contains(PropertyFlags::IS_WRITABLE)
    }

    pub fn is_private_field(&self) -> bool {
        self.flags.contains(PropertyFlags::IS_PRIVATE_FIELD)
    }

    pub fn is_private_method(&self) -> bool {
        self.flags.contains(PropertyFlags::IS_PRIVATE_METHOD)
    }

    pub fn is_private_accessor(&self) -> bool {
        self.flags.contains(PropertyFlags::IS_PRIVATE_ACCESSOR)
    }

    pub fn is_allowed_as_dense_array_property(&self) -> bool {
        self.flags.contains(DENSE_ARRAY_PROPERTY_FLAGS)
    }

    pub fn set_value(&mut self, value: Value) {
        self.value = value;
    }

    pub fn set_is_enumerable(&mut self, is_enumerable: bool) {
        if is_enumerable {
            self.flags.insert(PropertyFlags::IS_ENUMERABLE)
        } else {
            self.flags.remove(PropertyFlags::IS_ENUMERABLE)
        }
    }

    pub fn set_is_configurable(&mut self, is_configurable: bool) {
        if is_configurable {
            self.flags.insert(PropertyFlags::IS_CONFIGURABLE)
        } else {
            self.flags.remove(PropertyFlags::IS_CONFIGURABLE)
        }
    }

    pub fn set_is_writable(&mut self, is_writable: bool) {
        if is_writable {
            self.flags.insert(PropertyFlags::IS_WRITABLE)
        } else {
            self.flags.remove(PropertyFlags::IS_WRITABLE)
        }
    }

    // Combine a getter and setter accessor. Either this is a getter and the input is a setter,
    // or vice versa. No other inputs are allowed.
    pub fn add_accessor(&mut self, other: Property) {
        let mut this_accessor = self.value.as_accessor();
        let other_accessor = other.value.as_accessor();

        if this_accessor.get.is_none() {
            this_accessor.get = other_accessor.get;
        } else {
            this_accessor.set = other_accessor.set;
        }
    }
}
