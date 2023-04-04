use std::mem::{transmute, transmute_copy};
use std::ops::{Deref, DerefMut};

use super::builtin_function::BuiltinFunction;
use super::intrinsics::typed_array::TypedArray;
use super::object_descriptor::{ObjectDescriptor, ObjectKind};
use super::ordinary_object::OrdinaryObject;
use super::property_key::PropertyKey;
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
    descriptor: Gc<ObjectDescriptor>,
}

impl Gc<ObjectValue> {
    #[inline]
    pub fn descriptor(&self) -> Gc<ObjectDescriptor> {
        unsafe { self.as_ptr().read().descriptor }
    }

    fn descriptor_kind(&self) -> ObjectKind {
        self.descriptor().kind()
    }

    pub fn is_array(&self) -> bool {
        self.descriptor_kind() == ObjectKind::ArrayObject
    }

    pub fn is_error(&self) -> bool {
        self.descriptor_kind() == ObjectKind::ErrorObject
    }

    pub fn is_bool_object(&self) -> bool {
        self.descriptor_kind() == ObjectKind::BooleanObject
    }

    pub fn is_number_object(&self) -> bool {
        self.descriptor_kind() == ObjectKind::NumberObject
    }

    pub fn is_string_object(&self) -> bool {
        self.descriptor_kind() == ObjectKind::StringObject
    }

    pub fn is_symbol_object(&self) -> bool {
        self.descriptor_kind() == ObjectKind::SymbolObject
    }

    pub fn is_bigint_object(&self) -> bool {
        self.descriptor_kind() == ObjectKind::BigIntObject
    }

    pub fn is_date_object(&self) -> bool {
        false
    }

    pub fn is_regexp_object(&self) -> bool {
        false
    }

    pub fn is_map_object(&self) -> bool {
        self.descriptor_kind() == ObjectKind::MapObject
    }

    pub fn is_set_object(&self) -> bool {
        self.descriptor_kind() == ObjectKind::SetObject
    }

    pub fn is_array_buffer(&self) -> bool {
        self.descriptor_kind() == ObjectKind::ArrayBufferObject
    }

    pub fn is_shared_array_buffer(&self) -> bool {
        false
    }

    pub fn is_typed_array(&self) -> bool {
        let kind = self.descriptor_kind() as u8;
        (kind >= ObjectKind::Int8Array as u8) && (kind <= ObjectKind::Float64Array as u8)
    }

    pub fn is_data_view(&self) -> bool {
        self.descriptor_kind() == ObjectKind::DataViewObject
    }

    pub fn is_arguments_object(&self) -> bool {
        match self.descriptor_kind() {
            ObjectKind::MappedArgumentsObject | ObjectKind::UnmappedArgumentsObject => true,
            _ => false,
        }
    }

    pub fn is_proxy(&self) -> bool {
        self.descriptor_kind() == ObjectKind::Proxy
    }

    pub fn is_bound_function(&self) -> bool {
        self.descriptor_kind() == ObjectKind::BoundFunctionObject
    }

    pub fn is_object_prototype(&self) -> bool {
        self.descriptor_kind() == ObjectKind::ObjectPrototype
    }
}

pub type ObjectValueVtable = *const ();

pub trait HasObject {
    // Getters to cast subtype to this type
    fn object(&self) -> &OrdinaryObject;
    fn object_mut(&mut self) -> &mut OrdinaryObject;
}

pub trait Object: HasObject {
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

    fn as_typed_array(&self) -> Gc<dyn TypedArray> {
        unreachable!("as_typed_array can only be called on typed arrays")
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
            let vtable = data.read().descriptor.vtable();

            let trait_object = ObjectTraitObject { data, vtable };

            transmute_copy::<ObjectTraitObject, &dyn Object>(&trait_object)
        }
    }
}

impl DerefMut for Gc<ObjectValue> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            let data = self.as_ptr() as *const ObjectValue;
            let vtable = data.read().descriptor.vtable();

            let trait_object = ObjectTraitObject { data, vtable };

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
