use icu_normalizer::{ComposingNormalizer, DecomposingNormalizer};
use icu_properties::sets::{self, CodePointSetData, CodePointSetDataBorrowed};
use once_cell::sync::Lazy;

include!("../../../icu/data/mod.rs");

pub struct ICU {
    /// All code points with the ID_Start property.
    pub id_start: CodePointSetDataBorrowed<'static>,
    /// All code points with the ID_Continue property.
    pub id_continue: CodePointSetDataBorrowed<'static>,
    /// Normalizer for the NFC normalization form.
    pub nfc_normalizer: ComposingNormalizer,
    /// Normalizer for the NFD normalization form.
    pub nfd_normalizer: DecomposingNormalizer,
    /// Normalizer for the NFKC normalization form.
    pub nfkc_normalizer: ComposingNormalizer,
    /// Normalizer for the NFKD normalization form.
    pub nfkd_normalizer: DecomposingNormalizer,
}

pub static ICU: Lazy<ICU> = Lazy::new(|| {
    static ID_START_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_id_start(&BakedDataProvider).unwrap());
    static ID_CONTINUE_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_id_continue(&BakedDataProvider).unwrap());

    ICU {
        id_start: ID_START_SET.as_borrowed(),
        id_continue: ID_CONTINUE_SET.as_borrowed(),
        nfc_normalizer: ComposingNormalizer::try_new_nfc_unstable(&BakedDataProvider).unwrap(),
        nfd_normalizer: DecomposingNormalizer::try_new_nfd_unstable(&BakedDataProvider).unwrap(),
        nfkc_normalizer: ComposingNormalizer::try_new_nfkc_unstable(&BakedDataProvider).unwrap(),
        nfkd_normalizer: DecomposingNormalizer::try_new_nfkd_unstable(&BakedDataProvider).unwrap(),
    }
});
