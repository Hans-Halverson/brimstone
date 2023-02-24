use std::mem::{transmute, transmute_copy};
use std::ops::{Deref, DerefMut};

use crate::maybe;

use super::builtin_function::BuiltinFunction;
use super::environment::private_environment::PrivateNameId;
use super::property::{PrivateProperty, Property};
use super::property_key::PropertyKey;
use super::type_utilities::same_opt_object_value;
use super::{
    completion::EvalResult, gc::Gc, property_descriptor::PropertyDescriptor, value::Value,
};
use super::{Context, Realm};

/// Generic object type encompassing ordinary objects and all forms of exotic objects.
///
/// Every object struct must have a #[repr(C)] layout with a vtable at the beginning. This allows us
/// to use the ObjectValue as a "thin" trait object which consists of just a pointer to the data,
/// but can be implicitly derefed to a regular "fat" trait object by loading the vtable at the
/// beginning of the struct.
#[repr(C)]
pub struct ObjectValue {
    vtable: ObjectValueVtable,
}

pub type ObjectValueVtable = *const ();

pub trait Object {
    fn get_prototype_of(&self, cx: &mut Context) -> EvalResult<Option<Gc<ObjectValue>>>;

    fn set_prototype_of(
        &mut self,
        cx: &mut Context,
        proto: Option<Gc<ObjectValue>>,
    ) -> EvalResult<bool>;

    fn is_extensible(&self, cx: &mut Context) -> EvalResult<bool>;

    fn prevent_extensions(&mut self, cx: &mut Context) -> EvalResult<bool>;

    fn get_own_property(
        &self,
        cx: &mut Context,
        key: &PropertyKey,
    ) -> EvalResult<Option<PropertyDescriptor>>;

    fn define_own_property(
        &mut self,
        cx: &mut Context,
        key: &PropertyKey,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool>;

    fn has_property(&self, cx: &mut Context, key: &PropertyKey) -> EvalResult<bool>;

    fn get(&self, cx: &mut Context, key: &PropertyKey, receiver: Value) -> EvalResult<Value>;

    fn set(
        &mut self,
        cx: &mut Context,
        key: &PropertyKey,
        value: Value,
        receiver: Value,
    ) -> EvalResult<bool>;

    fn delete(&mut self, cx: &mut Context, key: &PropertyKey) -> EvalResult<bool>;

    fn own_property_keys(&self, cx: &mut Context) -> EvalResult<Vec<Value>>;

    fn call(
        &self,
        _: &mut Context,
        _this_argument: Value,
        _arguments: &[Value],
    ) -> EvalResult<Value> {
        panic!("[[Call]] not implemented for this object")
    }

    fn construct(
        &self,
        _: &mut Context,
        _arguments: &[Value],
        _new_target: Gc<ObjectValue>,
    ) -> EvalResult<Gc<ObjectValue>> {
        panic!("[[Construct]] not implemented for this object")
    }

    // Private property methods
    fn private_element_find(&mut self, private_id: PrivateNameId) -> Option<&mut PrivateProperty>;

    fn private_field_add(
        &mut self,
        cx: &mut Context,
        private_id: PrivateNameId,
        value: Value,
    ) -> EvalResult<()>;

    fn private_method_or_accessor_add(
        &mut self,
        cx: &mut Context,
        private_id: PrivateNameId,
        private_method: PrivateProperty,
    ) -> EvalResult<()>;

    // Property accessors and mutators
    fn get_property(&self, key: &PropertyKey) -> Option<&Property>;
    fn get_property_mut(&mut self, key: &PropertyKey) -> Option<&mut Property>;
    fn set_property(&mut self, key: &PropertyKey, value: Property);
    fn remove_property(&mut self, key: &PropertyKey);

    // Type utilities
    fn is_callable(&self) -> bool {
        false
    }

    fn is_constructor(&self) -> bool {
        false
    }

    fn get_realm(&self, cx: &mut Context) -> EvalResult<Gc<Realm>> {
        cx.current_realm().into()
    }

    // Type refinement functions
    fn as_builtin_function_opt(&self) -> Option<Gc<BuiltinFunction>> {
        None
    }

    fn is_array(&self) -> bool {
        false
    }

    fn is_error(&self) -> bool {
        false
    }

    fn is_bool_object(&self) -> bool {
        false
    }

    fn is_number_object(&self) -> bool {
        false
    }

    fn is_string_object(&self) -> bool {
        false
    }

    fn is_symbol_object(&self) -> bool {
        false
    }

    fn is_bigint_object(&self) -> bool {
        false
    }

    fn is_date_object(&self) -> bool {
        false
    }

    fn is_regexp_object(&self) -> bool {
        false
    }

    fn is_map_object(&self) -> bool {
        false
    }

    fn is_arguments_object(&self) -> bool {
        false
    }

    fn is_proxy(&self) -> bool {
        false
    }

    fn is_bound_function(&self) -> bool {
        false
    }
}

// 10.4.7.2 SetImmutablePrototype
pub fn set_immutable_prototype(
    cx: &mut Context,
    object: Gc<ObjectValue>,
    proto: Option<Gc<ObjectValue>>,
) -> EvalResult<bool> {
    let current_proto = maybe!(object.get_prototype_of(cx));
    same_opt_object_value(proto, current_proto).into()
}

// Allow downcasting from Gc<ObjectValue> to any Object type
impl Gc<ObjectValue> {
    pub fn cast<T: Object>(&self) -> Gc<T> {
        Gc::from_ptr(self.as_ptr() as *mut T)
    }
}

// Same layout as in std::raw, which is not exposed in stable. This definition is only used
// to properly type our custom trait object creation.
#[repr(C)]
struct ObjectTraitObject {
    data: *const ObjectValue,
    vtable: *const (),
}

impl Deref for Gc<ObjectValue> {
    type Target = dyn Object;

    fn deref(&self) -> &Self::Target {
        unsafe {
            let data = self.as_ptr() as *const ObjectValue;
            let object_value = data.read();

            let trait_object = ObjectTraitObject { data, vtable: object_value.vtable };

            transmute_copy::<ObjectTraitObject, &dyn Object>(&trait_object)
        }
    }
}

impl DerefMut for Gc<ObjectValue> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            let data = self.as_ptr() as *const ObjectValue;
            let object_value = data.read();

            let trait_object = ObjectTraitObject { data, vtable: object_value.vtable };

            transmute_copy::<ObjectTraitObject, &mut dyn Object>(&trait_object)
        }
    }
}

/// Compile time shenanigans to extract the trait object vtable for a particular type that
/// implements Object so that we can construct our own trait objects manually.
pub const fn extract_object_vtable<T: Object>() -> *const () {
    unsafe {
        let example_ptr: *const T = std::ptr::null();
        let example_trait_object: *const dyn Object = example_ptr;
        let object_trait_object =
            transmute::<*const dyn Object, ObjectTraitObject>(example_trait_object);

        object_trait_object.vtable
    }
}
