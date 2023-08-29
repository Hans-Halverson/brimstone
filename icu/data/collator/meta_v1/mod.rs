// @generated
#![allow(clippy::octal_escapes)]
type DataStruct =
    <::icu_collator::provider::CollationMetadataV1Marker as ::icu_provider::DataMarker>::Yokeable;
pub fn lookup(locale: &icu_provider::DataLocale) -> Option<&'static DataStruct> {
    static KEYS: [&str; 3usize] = ["und", "und-u-co-emoji", "und-u-co-eor"];
    static DATA: [&DataStruct; 3usize] = [&UND, &UND, &UND];
    KEYS.binary_search_by(|k| locale.strict_cmp(k.as_bytes()).reverse())
        .ok()
        .map(|i| unsafe { *DATA.get_unchecked(i) })
}
static UND: DataStruct = include!("und.rs.data");
