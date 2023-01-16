use super::{
    object_value::ObjectValue,
    value::{AccessorValue, Value},
    Context, Gc,
};

// A property value. If a data property then the value is the value itself. If an accessor property
// then the value is an accessor property, which is a pointer to the get and set function pair.
#[derive(Clone)]
pub struct Property {
    value: Value,
    is_writable: bool,
    is_enumerable: bool,
    is_configurable: bool,
}

impl Property {
    pub const fn data(
        value: Value,
        is_writable: bool,
        is_enumerable: bool,
        is_configurable: bool,
    ) -> Property {
        Property { value, is_writable, is_enumerable, is_configurable }
    }

    pub const fn accessor(
        accessor_value: Value,
        is_enumerable: bool,
        is_configurable: bool,
    ) -> Property {
        Property {
            value: accessor_value,
            is_enumerable,
            is_configurable,
            is_writable: false,
        }
    }

    pub fn value(&self) -> Value {
        self.value
    }

    pub fn is_enumerable(&self) -> bool {
        self.is_enumerable
    }

    pub fn is_configurable(&self) -> bool {
        self.is_configurable
    }

    pub fn is_writable(&self) -> bool {
        self.is_writable
    }

    pub fn set_value(&mut self, value: Value) {
        self.value = value;
    }

    pub fn set_is_enumerable(&mut self, is_enumerable: bool) {
        self.is_enumerable = is_enumerable
    }

    pub fn set_is_configurable(&mut self, is_configurable: bool) {
        self.is_configurable = is_configurable
    }

    pub fn set_is_writable(&mut self, is_writable: bool) {
        self.is_writable = is_writable
    }
}

// 6.2.9 PrivateElement Record
pub struct PrivateProperty {
    kind: PrivatePropertyKind,
    value: Value,
}

impl PrivateProperty {
    pub fn field(value: Value) -> PrivateProperty {
        PrivateProperty { kind: PrivatePropertyKind::Field, value }
    }

    pub fn method(method: Gc<ObjectValue>) -> PrivateProperty {
        PrivateProperty { kind: PrivatePropertyKind::Method, value: method.into() }
    }

    pub fn getter(cx: &mut Context, getter: Gc<ObjectValue>) -> PrivateProperty {
        let accessor_value = cx
            .heap
            .alloc(AccessorValue { get: Some(getter), set: None });
        PrivateProperty {
            kind: PrivatePropertyKind::Accessor,
            value: accessor_value.into(),
        }
    }

    pub fn setter(cx: &mut Context, setter: Gc<ObjectValue>) -> PrivateProperty {
        let accessor_value = cx
            .heap
            .alloc(AccessorValue { get: None, set: Some(setter) });
        PrivateProperty {
            kind: PrivatePropertyKind::Accessor,
            value: accessor_value.into(),
        }
    }

    pub fn kind(&self) -> PrivatePropertyKind {
        self.kind
    }

    pub fn value(&self) -> Value {
        self.value
    }

    pub fn set_value(&mut self, value: Value) {
        self.value = value
    }

    // Combine a getter and setter accessor. Either this is a getter and the input is a setter,
    // or vice versa. No other inputs are allowed.
    pub fn add_accessor(&mut self, other: PrivateProperty) {
        let mut this_accessor = self.value.as_accessor();
        let other_accessor = other.value.as_accessor();

        if this_accessor.get.is_none() {
            this_accessor.get = other_accessor.get;
        } else {
            this_accessor.set = other_accessor.set;
        }
    }
}

impl Clone for PrivateProperty {
    fn clone(&self) -> PrivateProperty {
        PrivateProperty { kind: self.kind, value: self.value }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum PrivatePropertyKind {
    Field,
    Method,
    Accessor,
}
