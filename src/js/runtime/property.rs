use super::value::Value;

// A property value. If a data property then the value is the value itself. If an accessor property
// then the value is an accessor property, which is a pointer to the get and set function pair.
pub struct Property {
    value: Value,
    is_enumerable: bool,
    is_configurable: bool,
    is_writable: bool,
}

impl Property {
    pub fn data(
        value: Value,
        is_enumerable: bool,
        is_configurable: bool,
        is_writable: bool,
    ) -> Property {
        Property {
            value,
            is_enumerable,
            is_configurable,
            is_writable,
        }
    }

    pub fn accessor(accessor_value: Value, is_enumerable: bool, is_configurable: bool) -> Property {
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
