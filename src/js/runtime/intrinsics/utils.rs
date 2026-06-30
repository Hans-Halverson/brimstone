// Generate a function that casts from a value to a particular object type by comparing vtables
#[macro_export]
macro_rules! cast_from_value_fn {
    ($type:ident $(<$($generics:tt),*>)?, $name:expr) => {
        pub fn cast_from_value(
            cx: Context,
            value: Handle<Value>,
        ) -> EvalResult<Handle<$type $(<$($generics),*>)?>> {
            if let Some(value) = value.as_opt::<$type>() {
                Ok(value)
            } else {
                type_error(cx, concat!("expected object of type ", $name))
            }
        }
    };
}
