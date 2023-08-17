use icu_properties::sets::{self, CodePointSetData, CodePointSetDataBorrowed};
use once_cell::sync::Lazy;

include!("../../../icu/data/mod.rs");

pub struct ICU {
    /// All code points with the ID_Start property.
    pub id_start: CodePointSetDataBorrowed<'static>,
    /// All code points with the ID_Continue property.
    pub id_continue: CodePointSetDataBorrowed<'static>,
}

pub static ICU: Lazy<ICU> = Lazy::new(|| {
    static ID_START_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_id_start(&BakedDataProvider).unwrap());
    static ID_CONTINUE_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_id_continue(&BakedDataProvider).unwrap());

    ICU {
        id_start: ID_START_SET.as_borrowed(),
        id_continue: ID_CONTINUE_SET.as_borrowed(),
    }
});
