use crate::{
    handle_scope_guard,
    runtime::{
        Context, Handle, HeapItemKind, Realm, Value,
        accessor::Accessor,
        alloc_error::AllocResult,
        builtin_function::BuiltinFunction,
        global_object::GlobalObject,
        intrinsics::{intrinsics::Intrinsic, rust_runtime::RuntimeFunction},
        object_value::ObjectValue,
        ordinary_object::{OrdinaryObject, PropertyStorage, object_create_with_proto},
        property::Property,
        property_key::PropertyKey,
    },
};

/// Installs properties on an intrinsic object (prototype, constructor, or global object).
pub struct IntrinsicBuilder {
    cx: Context,
    realm: Handle<Realm>,
    object: IntrinsicObject,
}

enum IntrinsicObject {
    Ordinary(Handle<ObjectValue>),
    Global(Handle<GlobalObject>),
}

impl IntrinsicBuilder {
    /// Wrap an already-created ordinary object.
    pub fn ordinary(cx: Context, realm: Handle<Realm>, object: Handle<ObjectValue>) -> Self {
        Self { cx, realm, object: IntrinsicObject::Ordinary(object) }
    }

    /// Wrap the global object of the given realm.
    pub fn global(cx: Context, realm: Handle<Realm>) -> Self {
        Self {
            cx,
            realm,
            object: IntrinsicObject::Global(realm.global_object()),
        }
    }

    /// Create an ordinary object with the given prototype.
    pub fn new_object(
        cx: Context,
        realm: Handle<Realm>,
        prototype: Intrinsic,
    ) -> AllocResult<Self> {
        let object = object_create_with_proto::<OrdinaryObject>(
            cx,
            HeapItemKind::OrdinaryObject,
            realm.get_intrinsic(prototype),
        )?
        .to_handle();

        Ok(Self::ordinary(cx, realm, object.as_object()))
    }

    /// Create a constructor function object with the given prototype.
    pub fn constructor(
        cx: Context,
        realm: Handle<Realm>,
        func: RuntimeFunction,
        length: u32,
        name: Handle<PropertyKey>,
        prototype: Intrinsic,
    ) -> AllocResult<Self> {
        let object =
            BuiltinFunction::intrinsic_constructor(cx, func, length, name, realm, prototype)?;
        Ok(Self::ordinary(cx, realm, object))
    }

    fn get_property(&self, key: Handle<PropertyKey>) -> Option<Property> {
        match self.object {
            IntrinsicObject::Ordinary(ref object) => {
                PropertyStorage::get_property(object, self.cx, key)
            }
            IntrinsicObject::Global(ref global_object) => {
                PropertyStorage::get_property(global_object, self.cx, key)
            }
        }
    }

    /// Install any key/property pair on the object.
    pub fn property(&mut self, key: Handle<PropertyKey>, property: Property) -> AllocResult<()> {
        match self.object {
            IntrinsicObject::Ordinary(ref mut object) => {
                PropertyStorage::set_property(object, self.cx, key, property)
            }
            IntrinsicObject::Global(ref mut global_object) => {
                PropertyStorage::set_property(global_object, self.cx, key, property)
            }
        }
    }

    /// Install a builtin method (writable, non-enumerable, configurable).
    pub fn method(
        &mut self,
        name: Handle<PropertyKey>,
        func: RuntimeFunction,
        length: u32,
    ) -> AllocResult<()> {
        handle_scope_guard!(self.cx);

        let func = BuiltinFunction::create(self.cx, func, length, name, self.realm, None)?.into();
        let property = Property::data(func, true, false, true);
        self.property(name, property)
    }

    /// Install a data property (writable, non-enumerable, configurable).
    pub fn data(&mut self, name: Handle<PropertyKey>, value: Handle<Value>) -> AllocResult<()> {
        handle_scope_guard!(self.cx);

        let property = Property::data(value, true, false, true);
        self.property(name, property)
    }

    /// Install a getter-only accessor (non-enumerable, configurable).
    pub fn getter(&mut self, name: Handle<PropertyKey>, func: RuntimeFunction) -> AllocResult<()> {
        handle_scope_guard!(self.cx);

        let getter = BuiltinFunction::create(self.cx, func, 0, name, self.realm, Some("get"))?;
        let accessor_value = Accessor::new(self.cx, Some(getter), None)?;
        let property = Property::accessor(accessor_value.into(), false, true);
        self.property(name, property)
    }

    /// Install a getter/setter accessor (non-enumerable, configurable).
    pub fn getter_setter(
        &mut self,
        name: Handle<PropertyKey>,
        getter: RuntimeFunction,
        setter: RuntimeFunction,
    ) -> AllocResult<()> {
        handle_scope_guard!(self.cx);

        let getter = BuiltinFunction::create(self.cx, getter, 0, name, self.realm, Some("get"))?;
        let setter = BuiltinFunction::create(self.cx, setter, 1, name, self.realm, Some("set"))?;
        let accessor_value = Accessor::new(self.cx, Some(getter), Some(setter))?;
        let property = Property::accessor(accessor_value.into(), false, true);
        self.property(name, property)
    }

    /// Install a frozen data property (non-writable, non-enumerable, non-configurable).
    pub fn frozen(&mut self, name: Handle<PropertyKey>, value: Handle<Value>) -> AllocResult<()> {
        handle_scope_guard!(self.cx);

        let property = Property::data(value, false, false, false);
        self.property(name, property)
    }

    /// Install the given intrinsic as a frozen prototype property.
    pub fn prototype(&mut self, prototype: Intrinsic) -> AllocResult<()> {
        let value = self.realm.get_intrinsic(prototype).into();
        self.frozen(self.cx.names.prototype(), value)
    }

    /// Install @@toStringTag with a particular string value.
    pub fn to_string_tag(&mut self, tag: Handle<PropertyKey>) -> AllocResult<()> {
        let key = self.cx.symbols.to_string_tag();
        let value = tag.as_string().into();
        self.property(key, Property::data(value, false, false, true))
    }

    /// Create a builtin function without installing it on the object.
    pub fn function(
        &self,
        func: RuntimeFunction,
        length: u32,
        name: Handle<PropertyKey>,
    ) -> AllocResult<Handle<ObjectValue>> {
        BuiltinFunction::create(self.cx, func, length, name, self.realm, None)
    }

    /// Create an alias for an existing property under a new key. All attributes of the property
    /// are preserved.
    pub fn alias(
        &mut self,
        from_key: Handle<PropertyKey>,
        to_key: Handle<PropertyKey>,
    ) -> AllocResult<()> {
        handle_scope_guard!(self.cx);

        let property = self.get_property(from_key).unwrap();
        self.property(to_key, property)
    }

    /// Finish, returning the object.
    pub fn build(self) -> AllocResult<Handle<ObjectValue>> {
        match self.object {
            IntrinsicObject::Ordinary(object) => Ok(object),
            IntrinsicObject::Global(global_object) => Ok(global_object.as_object()),
        }
    }
}

/// Installs a list of builtin methods on an IntrinsicBuilder.
/// Uses the syntax: `builtin_name RuntimeFunctionVariant (length)` for each line.
#[macro_export]
macro_rules! intrinsic_methods {
    ($cx:expr, $builder:expr, { $( $name:ident $variant:ident ( $length:expr ) ),* $(,)? }) => {
        $(
            $builder.method(
                $cx.names.$name(),
                $crate::runtime::intrinsics::rust_runtime::RuntimeFunction::$variant,
                $length,
            )?;
        )*
    };
}

/// Installs a list of getters on an IntrinsicBuilder.
/// Uses the syntax: `builtin_name RuntimeFunctionVariant` for each line.
#[macro_export]
macro_rules! intrinsic_getter_methods {
    ($cx:expr, $builder:expr, { $( $name:ident $variant:ident ),* $(,)? }) => {
        $(
            $builder.getter(
                $cx.names.$name(),
                $crate::runtime::intrinsics::rust_runtime::RuntimeFunction::$variant,
            )?;
        )*
    };
}
