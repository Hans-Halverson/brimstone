// Generate a function that casts from a value to a particular object type by comparing vtables
#[macro_export]
macro_rules! cast_from_value_fn {
    ($type:ident, $name:expr) => {
        pub fn cast_from_value(cx: &mut Context, value: Value) -> EvalResult<Gc<$type>> {
            if !value.is_object() {
                return type_error_(cx, concat!("expected object of type ", $name));
            }

            // Extract vtable from trait object and compare to known vtable for this type
            let object_value = value.as_object();
            let trait_object = unsafe {
                std::mem::transmute::<&dyn Object, (*const (), *const ())>(object_value.deref())
            };

            if trait_object.1 != $type::VTABLE {
                return type_error_(cx, concat!("expected object of type ", $name));
            }

            object_value.cast::<$type>().into()
        }
    };
}
