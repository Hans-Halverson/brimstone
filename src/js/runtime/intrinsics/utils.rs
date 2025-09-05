// Generate a function that casts from a value to a particular object type by comparing vtables
#[macro_export]
macro_rules! cast_from_value_fn {
    ($type:ident $(<$($generics:tt),*>)?, $name:expr) => {
        pub fn cast_from_value(
            cx: Context,
            value: Handle<Value>,
        ) -> EvalResult<Handle<$type $(<$($generics),*>)?>> {
            if !value.is_object() {
                return type_error(cx, concat!("expected object of type ", $name));
            }

            // Check if object descriptor's kind matches the kind we are casting to
            let object_value = value.as_object();
            if object_value.descriptor().kind() != HeapItemKind::$type {
                return type_error(cx, concat!("expected object of type ", $name));
            }

            Ok(object_value.cast::<$type>())
        }
    };
}
