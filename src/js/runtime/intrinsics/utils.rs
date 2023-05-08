// Generate a function that casts from a value to a particular object type by comparing vtables
#[macro_export]
macro_rules! cast_from_value_fn {
    ($type:ident, $name:expr) => {
        pub fn cast_from_value(cx: &mut Context, value: HandleValue) -> EvalResult<Handle<$type>> {
            if !value.is_object() {
                return type_error_(cx, concat!("expected object of type ", $name));
            }

            // Check if object descriptor's kind matches the kind we are casting to
            let object_value = value.as_object();
            if object_value.descriptor().kind() != ObjectKind::$type {
                return type_error_(cx, concat!("expected object of type ", $name));
            }

            object_value.cast::<$type>().into()
        }
    };
}
