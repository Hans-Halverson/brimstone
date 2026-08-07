/// Set an uninitialized field to a value, making sure not to drop the old value stored at that field.
#[macro_export]
macro_rules! set_uninit {
    ($field:expr, $value:expr) => {
        #[allow(clippy::macro_metavars_in_unsafe)]
        unsafe {
            std::ptr::addr_of_mut!($field).write($value)
        }
    };
}
