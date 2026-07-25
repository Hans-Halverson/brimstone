use bitflags::bitflags;

use crate::runtime::{
    Context,
    accessor::Accessor,
    alloc_error::AllocResult,
    gc::{Handle, HeapVisitor},
    object_value::ObjectValue,
    value::Value,
};

/// A property value. If a data property then the value is the value itself. If an accessor property
/// then the value is an accessor property, which is a pointer to the get and set function pair.
///
/// Property structs contain handles and can only be stored on the stack.
///
/// PrivateElements (https://tc39.es/ecma262/#sec-privateelement-specification-type) are represented
/// as properties, with bitflags noting the private property kind.
#[derive(Copy, Clone)]
pub struct Property {
    value: Handle<Value>,
    flags: PropertyFlags,
}

/// A property that is stored on the heap. Contains direct references to heap items.
#[derive(Clone)]
pub struct HeapProperty {
    value: Value,
    flags: PropertyFlags,
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct PropertyFlags: u8 {
        const IS_WRITABLE = 1 << 0;
        const IS_ENUMERABLE = 1 << 1;
        const IS_CONFIGURABLE = 1 << 2;
        const IS_ACCESSOR = 1 << 3;
        const IS_PRIVATE_FIELD = 1 << 4;
        const IS_PRIVATE_METHOD = 1 << 5;
        const IS_PRIVATE_ACCESSOR = 1 << 6;
    }
}

impl PropertyFlags {
    /// Return these flags with the writable attribute added.
    #[inline]
    pub const fn writable(self) -> PropertyFlags {
        self.union(PropertyFlags::IS_WRITABLE)
    }

    /// Return these flags with the enumerable attribute added.
    #[inline]
    pub const fn enumerable(self) -> PropertyFlags {
        self.union(PropertyFlags::IS_ENUMERABLE)
    }

    /// Return these flags with the configurable attribute added.
    #[inline]
    pub const fn configurable(self) -> PropertyFlags {
        self.union(PropertyFlags::IS_CONFIGURABLE)
    }

    /// Return these flags with the accessor attribute added.
    #[inline]
    pub const fn accessor(self) -> PropertyFlags {
        self.union(PropertyFlags::IS_ACCESSOR)
    }

    #[inline]
    pub fn from_data_attributes(
        is_writable: bool,
        is_enumerable: bool,
        is_configurable: bool,
    ) -> PropertyFlags {
        let mut flags = PropertyFlags::empty();
        flags.set(PropertyFlags::IS_WRITABLE, is_writable);
        flags.set(PropertyFlags::IS_ENUMERABLE, is_enumerable);
        flags.set(PropertyFlags::IS_CONFIGURABLE, is_configurable);
        flags
    }

    #[inline]
    pub fn from_accessor_attributes(is_enumerable: bool, is_configurable: bool) -> PropertyFlags {
        let mut flags = PropertyFlags::IS_ACCESSOR;
        flags.set(PropertyFlags::IS_ENUMERABLE, is_enumerable);
        flags.set(PropertyFlags::IS_CONFIGURABLE, is_configurable);
        flags
    }

    #[inline]
    pub fn is_writable(&self) -> bool {
        self.contains(PropertyFlags::IS_WRITABLE)
    }

    #[inline]
    pub fn is_enumerable(&self) -> bool {
        self.contains(PropertyFlags::IS_ENUMERABLE)
    }

    #[inline]
    pub fn is_configurable(&self) -> bool {
        self.contains(PropertyFlags::IS_CONFIGURABLE)
    }

    #[inline]
    pub fn is_accessor(&self) -> bool {
        self.contains(PropertyFlags::IS_ACCESSOR)
    }

    #[inline]
    pub fn is_private(&self) -> bool {
        self.intersects(
            PropertyFlags::IS_PRIVATE_FIELD
                | PropertyFlags::IS_PRIVATE_METHOD
                | PropertyFlags::IS_PRIVATE_ACCESSOR,
        )
    }
}

pub const DEFAULT_DATA_PROPERTY_FLAGS: PropertyFlags = PropertyFlags::IS_WRITABLE
    .union(PropertyFlags::IS_ENUMERABLE)
    .union(PropertyFlags::IS_CONFIGURABLE);

pub const DEFAULT_ACCESSOR_PROPERTY_FLAGS: PropertyFlags = PropertyFlags::IS_ACCESSOR
    .union(PropertyFlags::IS_ENUMERABLE)
    .union(PropertyFlags::IS_CONFIGURABLE);

pub const DENSE_ARRAY_PROPERTY_FLAGS: PropertyFlags = DEFAULT_DATA_PROPERTY_FLAGS;

impl Property {
    #[inline]
    pub fn data(value: Handle<Value>, flags: PropertyFlags) -> Property {
        Property { value, flags }
    }

    /// An ordinary data property: writable, enumerable, and configurable.
    #[inline]
    pub fn default_data(value: Handle<Value>) -> Property {
        Property::data(value, DEFAULT_DATA_PROPERTY_FLAGS)
    }

    /// A writable, configurable, non-enumerable data property.
    #[inline]
    pub fn non_enumerable_data(value: Handle<Value>) -> Property {
        Property::data(value, PropertyFlags::empty().writable().configurable())
    }

    /// A frozen data property: non-writable, non-enumerable, and non-configurable.
    #[inline]
    pub fn frozen(value: Handle<Value>) -> Property {
        Property::data(value, PropertyFlags::empty())
    }

    #[inline]
    pub fn accessor(accessor_value: Handle<Value>, flags: PropertyFlags) -> Property {
        let flags = flags | PropertyFlags::IS_ACCESSOR;
        Property { value: accessor_value, flags }
    }

    pub fn private_field(value: Handle<Value>) -> Property {
        Property { value, flags: PropertyFlags::IS_PRIVATE_FIELD }
    }

    pub fn private_method(method: Handle<ObjectValue>) -> Property {
        Property {
            value: method.into(),
            flags: PropertyFlags::IS_PRIVATE_METHOD,
        }
    }

    pub fn private_getter(cx: Context, getter: Handle<ObjectValue>) -> AllocResult<Property> {
        let accessor_value = Accessor::new(cx, Some(getter), None)?;
        Ok(Property {
            value: accessor_value.into(),
            flags: PropertyFlags::IS_PRIVATE_ACCESSOR | PropertyFlags::IS_ACCESSOR,
        })
    }

    pub fn private_setter(cx: Context, setter: Handle<ObjectValue>) -> AllocResult<Property> {
        let accessor_value = Accessor::new(cx, None, Some(setter))?;
        Ok(Property {
            value: accessor_value.into(),
            flags: PropertyFlags::IS_PRIVATE_ACCESSOR | PropertyFlags::IS_ACCESSOR,
        })
    }

    pub fn private_accessor(accessor: Handle<Accessor>) -> Property {
        Property {
            value: accessor.into(),
            flags: PropertyFlags::IS_PRIVATE_ACCESSOR | PropertyFlags::IS_ACCESSOR,
        }
    }

    /// Return the underlying raw value. For data properties this is the value itself, for accessor
    /// properties this is the accessor value.
    pub fn value(&self) -> Handle<Value> {
        self.value
    }

    /// Return the underlying value as an accessor.
    pub fn accessor_value(&self) -> Handle<Accessor> {
        Accessor::from_value_handle(self.value)
    }

    pub fn flags(&self) -> PropertyFlags {
        self.flags
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

    pub fn is_data(&self) -> bool {
        !self.flags.contains(PropertyFlags::IS_ACCESSOR)
    }

    pub fn is_accessor(&self) -> bool {
        self.flags.contains(PropertyFlags::IS_ACCESSOR)
    }

    pub fn is_allowed_as_dense_array_property(&self) -> bool {
        self.flags.contains(DENSE_ARRAY_PROPERTY_FLAGS)
    }

    pub fn set_value(&mut self, value: Handle<Value>) {
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

    pub fn set_is_accessor(&mut self, is_accessor: bool) {
        if is_accessor {
            self.flags.insert(PropertyFlags::IS_ACCESSOR)
        } else {
            self.flags.remove(PropertyFlags::IS_ACCESSOR)
        }
    }

    pub fn to_heap(self) -> HeapProperty {
        HeapProperty { value: *self.value, flags: self.flags }
    }

    pub fn from_heap(cx: Context, heap_property: &HeapProperty) -> Property {
        Property {
            value: heap_property.value.to_handle(cx),
            flags: heap_property.flags,
        }
    }
}

impl HeapProperty {
    pub fn new(value: Value, flags: PropertyFlags) -> HeapProperty {
        HeapProperty { value, flags }
    }

    pub fn value(&self) -> Value {
        self.value
    }

    pub fn flags(&self) -> PropertyFlags {
        self.flags
    }

    pub fn is_configurable(&self) -> bool {
        self.flags.contains(PropertyFlags::IS_CONFIGURABLE)
    }

    pub fn is_private(&self) -> bool {
        self.flags.is_private()
    }

    pub fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_value(&mut self.value);
    }
}
