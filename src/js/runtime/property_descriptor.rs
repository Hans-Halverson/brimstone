use crate::{
    must_a,
    runtime::{
        Context, Value,
        abstract_operations::{create_data_property_or_throw, get, has_property},
        accessor::Accessor,
        alloc_error::AllocResult,
        common_shapes::CommonShape,
        error::type_error,
        eval_result::EvalResult,
        gc::Handle,
        object_value::ObjectValue,
        ordinary_object::{ObjectBuilder, ordinary_object_create},
        property::{DEFAULT_DATA_PROPERTY_FLAGS, Property, PropertyFlags},
        type_utilities::{is_callable, to_boolean},
    },
};

/// Property Descriptor (https://tc39.es/ecma262/#sec-property-descriptor-specification-type)
#[derive(Clone, Copy, Default)]
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
    /// A data descriptor with all three attributes present.
    pub fn data(value: Handle<Value>, flags: PropertyFlags) -> PropertyDescriptor {
        PropertyDescriptor {
            value: Some(value),
            is_writable: Some(flags.is_writable()),
            is_enumerable: Some(flags.is_enumerable()),
            is_configurable: Some(flags.is_configurable()),
            ..Default::default()
        }
    }

    /// An ordinary data property: writable, enumerable, and configurable.
    pub fn default_data(value: Handle<Value>) -> PropertyDescriptor {
        PropertyDescriptor::data(value, DEFAULT_DATA_PROPERTY_FLAGS)
    }

    /// A writable, configurable, non-enumerable data property.
    pub fn non_enumerable_data(value: Handle<Value>) -> PropertyDescriptor {
        PropertyDescriptor::data(value, PropertyFlags::empty().writable().configurable())
    }

    /// A frozen data property: non-writable, non-enumerable, and non-configurable.
    pub fn frozen(value: Handle<Value>) -> PropertyDescriptor {
        PropertyDescriptor::data(value, PropertyFlags::empty())
    }

    /// An accessor descriptor with both a getter and setter.
    pub fn accessor(
        get: Option<Handle<ObjectValue>>,
        set: Option<Handle<ObjectValue>>,
        flags: PropertyFlags,
    ) -> PropertyDescriptor {
        PropertyDescriptor {
            get,
            set,
            is_enumerable: Some(flags.is_enumerable()),
            is_configurable: Some(flags.is_configurable()),
            has_get: true,
            has_set: true,
            ..Default::default()
        }
    }

    /// An accessor descriptor with only a getter.
    pub fn getter(get: Option<Handle<ObjectValue>>, flags: PropertyFlags) -> PropertyDescriptor {
        PropertyDescriptor {
            get,
            is_enumerable: Some(flags.is_enumerable()),
            is_configurable: Some(flags.is_configurable()),
            has_get: true,
            ..Default::default()
        }
    }

    /// An accessor descriptor with only a setter.
    pub fn setter(set: Option<Handle<ObjectValue>>, flags: PropertyFlags) -> PropertyDescriptor {
        PropertyDescriptor {
            set,
            is_enumerable: Some(flags.is_enumerable()),
            is_configurable: Some(flags.is_configurable()),
            has_set: true,
            ..Default::default()
        }
    }

    /// A data property descriptor with only the value set. No attributes are present.
    pub fn data_value_only(value: Handle<Value>) -> PropertyDescriptor {
        PropertyDescriptor { value: Some(value), ..Default::default() }
    }

    /// A property descriptor with only the attributes set. No value is present.
    pub fn attributes_only(
        is_writable: Option<bool>,
        is_enumerable: Option<bool>,
        is_configurable: Option<bool>,
    ) -> PropertyDescriptor {
        PropertyDescriptor {
            is_writable,
            is_enumerable,
            is_configurable,
            ..Default::default()
        }
    }

    pub fn to_property(&self, cx: Context) -> AllocResult<Property> {
        if self.is_accessor_descriptor() {
            let accessor = Accessor::new(cx, self.get, self.set)?;
            let flags = PropertyFlags::from_accessor_attributes(
                self.is_enumerable(),
                self.is_configurable(),
            );
            Ok(Property::accessor(accessor.into(), flags))
        } else {
            let flags = PropertyFlags::from_data_attributes(
                self.is_writable(),
                self.is_enumerable(),
                self.is_configurable(),
            );
            Ok(Property::data(self.value.unwrap(), flags))
        }
    }

    pub fn is_writable(&self) -> bool {
        self.is_writable.unwrap_or(false)
    }

    pub fn is_enumerable(&self) -> bool {
        self.is_enumerable.unwrap_or(false)
    }

    pub fn is_configurable(&self) -> bool {
        self.is_configurable.unwrap_or(false)
    }

    pub fn has_no_fields(&self) -> bool {
        self.value.is_none()
            && !self.has_get
            && !self.has_set
            && self.is_writable.is_none()
            && self.is_enumerable.is_none()
            && self.is_configurable.is_none()
    }

    /// IsAccessorDescriptor (https://tc39.es/ecma262/#sec-isaccessordescriptor)
    pub fn is_accessor_descriptor(&self) -> bool {
        self.has_get || self.has_set
    }

    /// IsDataDescriptor (https://tc39.es/ecma262/#sec-isdatadescriptor)
    pub fn is_data_descriptor(&self) -> bool {
        self.value.is_some() || self.is_writable.is_some()
    }

    /// IsGenericDescriptor (https://tc39.es/ecma262/#sec-isgenericdescriptor)
    pub fn is_generic_descriptor(&self) -> bool {
        !self.is_data_descriptor() && !self.is_accessor_descriptor()
    }

    /// CompletePropertyDescriptor (https://tc39.es/ecma262/#sec-completepropertydescriptor)
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

    /// Create a property descriptor from a JS object.
    ///
    /// ToPropertyDescriptor (https://tc39.es/ecma262/#sec-topropertydescriptor)
    pub fn from_object(cx: Context, value: Handle<Value>) -> EvalResult<PropertyDescriptor> {
        if !value.is_object() {
            return type_error(cx, "property descriptor must be an object");
        }

        let object = value.as_object();

        let mut desc = PropertyDescriptor::default();

        if has_property(cx, object, cx.names.enumerable())? {
            let enumerable = get(cx, object, cx.names.enumerable())?;
            desc.is_enumerable = Some(to_boolean(*enumerable));
        }

        if has_property(cx, object, cx.names.configurable())? {
            let configurable = get(cx, object, cx.names.configurable())?;
            desc.is_configurable = Some(to_boolean(*configurable));
        }

        if has_property(cx, object, cx.names.value())? {
            desc.value = Some(get(cx, object, cx.names.value())?);
        }

        if has_property(cx, object, cx.names.writable())? {
            let writable = get(cx, object, cx.names.writable())?;
            desc.is_writable = Some(to_boolean(*writable));
        }

        if has_property(cx, object, cx.names.get())? {
            let get = get(cx, object, cx.names.get())?;
            let is_function = is_callable(get);
            if !is_function && !get.is_undefined() {
                return type_error(cx, "getter must be a function");
            }

            desc.has_get = true;
            desc.get = if is_function {
                Some(get.as_object())
            } else {
                None
            };
        }

        if has_property(cx, object, cx.names.set_())? {
            let set = get(cx, object, cx.names.set_())?;
            let is_function = is_callable(set);
            if !is_function && !set.is_undefined() {
                return type_error(cx, "setter must be a function");
            }

            desc.has_set = true;
            desc.set = if is_function {
                Some(set.as_object())
            } else {
                None
            };
        }

        if (desc.has_get || desc.has_set) && (desc.value.is_some() || desc.is_writable.is_some()) {
            return type_error(cx, "property descriptor must be data or accessor");
        }

        Ok(desc)
    }

    /// Create a partial property descriptor object from the fields set in a PropertyDescriptor.
    ///
    /// FromPropertyDescriptor (https://tc39.es/ecma262/#sec-frompropertydescriptor)
    pub fn to_partial_object(&self, cx: Context) -> AllocResult<Handle<ObjectValue>> {
        let object = ordinary_object_create(cx)?;

        if let Some(value) = self.value {
            must_a!(create_data_property_or_throw(cx, object, cx.names.value(), value,));
        }

        if let Some(is_writable) = self.is_writable {
            let is_writable_value = cx.bool(is_writable);
            must_a!(create_data_property_or_throw(
                cx,
                object,
                cx.names.writable(),
                is_writable_value,
            ));
        }

        if self.has_get {
            let get_value = if let Some(get) = self.get {
                get.into()
            } else {
                cx.undefined()
            };

            must_a!(create_data_property_or_throw(cx, object, cx.names.get(), get_value));
        }

        if self.has_set {
            let set_value = if let Some(set) = self.set {
                set.into()
            } else {
                cx.undefined()
            };

            must_a!(create_data_property_or_throw(cx, object, cx.names.set_(), set_value));
        }

        if let Some(is_enumerable) = self.is_enumerable {
            let is_enumerable_value = cx.bool(is_enumerable);
            must_a!(create_data_property_or_throw(
                cx,
                object,
                cx.names.enumerable(),
                is_enumerable_value,
            ));
        }

        if let Some(is_configurable) = self.is_configurable {
            let is_configurable_value = cx.bool(is_configurable);
            must_a!(create_data_property_or_throw(
                cx,
                object,
                cx.names.configurable(),
                is_configurable_value,
            ));
        }

        Ok(object)
    }
}

/// Create a complete property descriptor object from a property.
///
/// Specializes FromPropertyDescriptor for a complete descriptor.
///
/// FromPropertyDescriptor (https://tc39.es/ecma262/#sec-frompropertydescriptor)
pub fn to_property_descriptor_object(
    cx: Context,
    property: Property,
) -> AllocResult<Handle<ObjectValue>> {
    let enumerable = cx.bool(property.is_enumerable());
    let configurable = cx.bool(property.is_configurable());

    if property.is_data() {
        let writable = cx.bool(property.is_writable());

        let mut object = ObjectBuilder::<ObjectValue>::new(cx)
            .common_shape(CommonShape::DataPropertyDescriptor)?
            .build()?
            .to_handle();
        object.init_properties(cx, &[property.value(), writable, enumerable, configurable])?;

        Ok(object)
    } else {
        let accessor = property.accessor_value();
        let get_value = if let Some(get) = accessor.get {
            get.as_value().to_handle(cx)
        } else {
            cx.undefined()
        };
        let set_value = if let Some(set) = accessor.set {
            set.as_value().to_handle(cx)
        } else {
            cx.undefined()
        };

        let mut object = ObjectBuilder::<ObjectValue>::new(cx)
            .common_shape(CommonShape::AccessorPropertyDescriptor)?
            .build()?
            .to_handle();
        object.init_properties(cx, &[get_value, set_value, enumerable, configurable])?;

        Ok(object)
    }
}
