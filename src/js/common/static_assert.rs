/// Assert a condition at compile time
#[macro_export]
macro_rules! static_assert {
    ($expr:expr) => {
        const _: () = std::assert!($expr);
    };
}
