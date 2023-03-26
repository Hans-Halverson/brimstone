/// Calculate field offset as const.
#[macro_export]
macro_rules! field_offset {
    ($base_type:ident, $field_name:ident) => {{
        const FIELD_OFFSET: usize = unsafe {
            let base_uninit = std::mem::MaybeUninit::uninit();
            let base_ptr: *const $base_type = base_uninit.as_ptr();

            let field_ptr = std::ptr::addr_of!((*base_ptr).$field_name);

            (field_ptr as *const u8).offset_from(base_ptr as *const u8) as usize
        };

        FIELD_OFFSET
    }};
}
