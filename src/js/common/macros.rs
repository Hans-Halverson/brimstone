/// Assert a condition at compile time
#[macro_export]
macro_rules! static_assert {
    ($expr:expr) => {
        const _: () = std::assert!($expr);
    };
}

#[macro_export]
macro_rules! replace_expr {
    ($_t:tt $sub:expr) => {
        $sub
    };
}

/// Count the number of arguments passed to the macro. Only use for low numbers (e.g. < 10).
#[macro_export]
macro_rules! count {
    ($($tts:tt)*) => {0usize $(+ replace_expr!($tts 1usize))*};
}
