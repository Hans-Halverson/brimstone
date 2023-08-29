// @generated
#![allow(clippy::octal_escapes)]
type DataStruct =
    <::icu_collator::provider::CollationDataV1Marker as ::icu_provider::DataMarker>::Yokeable;
pub fn lookup(locale: &icu_provider::DataLocale) -> Option<&'static DataStruct> {
    static KEYS: [&str; 3usize] = ["und", "und-u-co-emoji", "und-u-co-eor"];
    static DATA: [&DataStruct; 3usize] = [&UND, &UND_U_CO_EMOJI, &UND_U_CO_EOR];
    KEYS.binary_search_by(|k| locale.strict_cmp(k.as_bytes()).reverse())
        .ok()
        .map(|i| unsafe { *DATA.get_unchecked(i) })
}
static UND_U_CO_EMOJI: DataStruct = include!("und-u-co-emoji.rs.data");
static UND_U_CO_EOR: DataStruct = include!("und-u-co-eor.rs.data");
static UND: DataStruct = include!("und.rs.data");
