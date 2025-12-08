use crate::runtime::{
    alloc_error::AllocResult,
    intrinsics::{
        date_prototype::DatePrototype, global_object::init_global_annex_b_methods,
        intrinsics::Intrinsic, regexp_prototype::RegExpPrototype,
        string_prototype::StringPrototype,
    },
    Context, Handle, Realm,
};

/// Annex B methods are not guaranteed to be part of the serialized heap so we must initialize them
/// separately.
pub fn init_annex_b_methods(cx: Context, realm: Handle<Realm>) -> AllocResult<()> {
    init_global_annex_b_methods(cx, realm)?;

    let string_prototype = realm.get_intrinsic(Intrinsic::StringPrototype);
    StringPrototype::init_annex_b_methods(string_prototype, cx, realm)?;

    let date_prototype = realm.get_intrinsic(Intrinsic::DatePrototype);
    DatePrototype::init_annex_b_methods(date_prototype, cx, realm)?;

    let regexp_prototype = realm.get_intrinsic(Intrinsic::RegExpPrototype);
    RegExpPrototype::init_annex_b_methods(regexp_prototype, cx, realm)?;

    Ok(())
}
