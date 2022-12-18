use std::mem::{transmute, transmute_copy};
use std::ops::{Deref, DerefMut};

use super::Context;
use super::{
    completion::AbstractResult, gc::Gc, property_descriptor::PropertyDescriptor, value::Value,
};

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
    fn get_prototype_of(&self) -> AbstractResult<Option<Gc<ObjectValue>>>;

    fn set_prototype_of(&mut self, proto: Option<Gc<ObjectValue>>) -> AbstractResult<bool>;

    fn is_extensible(&self) -> AbstractResult<bool>;

    fn prevent_extensions(&mut self) -> AbstractResult<bool>;

    fn get_own_property(&self, key: &str) -> AbstractResult<Option<PropertyDescriptor>>;

    fn define_own_property(
        &mut self,
        cx: &mut Context,
        key: &str,
        desc: PropertyDescriptor,
    ) -> AbstractResult<bool>;

    fn has_property(&self, key: &str) -> AbstractResult<bool>;

    fn get(&self, cx: &mut Context, key: &str, receiver: Value) -> AbstractResult<Value>;

    fn set(
        &mut self,
        cx: &mut Context,
        key: &str,
        value: Value,
        receiver: Value,
    ) -> AbstractResult<bool>;

    fn delete(&mut self, key: &str) -> AbstractResult<bool>;

    fn own_property_keys(&self, cx: &mut Context) -> Vec<Value>;
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

            let trait_object = ObjectTraitObject {
                data,
                vtable: object_value.vtable,
            };

            transmute_copy::<ObjectTraitObject, &dyn Object>(&trait_object)
        }
    }
}

impl DerefMut for Gc<ObjectValue> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            let data = self.as_ptr() as *const ObjectValue;
            let object_value = data.read();

            let trait_object = ObjectTraitObject {
                data,
                vtable: object_value.vtable,
            };

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
