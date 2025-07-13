use crate::runtime::{
    intrinsics::{
        date_prototype::DatePrototype, intrinsics::Intrinsic, string_prototype::StringPrototype,
    },
    Context, Handle, Realm,
};

/// Annex B methods are not guaranteed to be part of the serialized heap so we must initialize them
/// separately.
pub fn init_annex_b_methods(cx: Context, realm: Handle<Realm>) {
    let string_prototype = realm.get_intrinsic(Intrinsic::StringPrototype);
    StringPrototype::init_annex_b_methods(string_prototype, cx, realm);

    let date_prototype = realm.get_intrinsic(Intrinsic::DatePrototype);
    DatePrototype::init_annex_b_methods(date_prototype, cx, realm);
}
