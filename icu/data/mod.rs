// @generated
include!("collation_diacritics_v1_marker.rs.data");
include!("collation_jamo_v1_marker.rs.data");
include!("collation_metadata_v1_marker.rs.data");
include!("collation_special_primaries_v1_marker.rs.data");
include!("collation_reordering_v1_marker.rs.data");
include!("collation_root_v1_marker.rs.data");
include!("collation_tailoring_v1_marker.rs.data");
include!("canonical_compositions_v1_marker.rs.data");
include!("canonical_decomposition_data_v1_marker.rs.data");
include!("canonical_decomposition_tables_v1_marker.rs.data");
include!("compatibility_decomposition_supplement_v1_marker.rs.data");
include!("compatibility_decomposition_tables_v1_marker.rs.data");
include!("script_name_to_value_v2_marker.rs.data");
include!("ascii_hex_digit_v1_marker.rs.data");
include!("alphabetic_v1_marker.rs.data");
include!("bidi_control_v1_marker.rs.data");
include!("bidi_mirrored_v1_marker.rs.data");
include!("case_ignorable_v1_marker.rs.data");
include!("changes_when_casefolded_v1_marker.rs.data");
include!("changes_when_casemapped_v1_marker.rs.data");
include!("changes_when_nfkc_casefolded_v1_marker.rs.data");
include!("changes_when_lowercased_v1_marker.rs.data");
include!("changes_when_titlecased_v1_marker.rs.data");
include!("changes_when_uppercased_v1_marker.rs.data");
include!("cased_v1_marker.rs.data");
include!("default_ignorable_code_point_v1_marker.rs.data");
include!("dash_v1_marker.rs.data");
include!("deprecated_v1_marker.rs.data");
include!("diacritic_v1_marker.rs.data");
include!("emoji_modifier_base_v1_marker.rs.data");
include!("emoji_component_v1_marker.rs.data");
include!("emoji_modifier_v1_marker.rs.data");
include!("emoji_presentation_v1_marker.rs.data");
include!("emoji_v1_marker.rs.data");
include!("extender_v1_marker.rs.data");
include!("extended_pictographic_v1_marker.rs.data");
include!("grapheme_base_v1_marker.rs.data");
include!("grapheme_extend_v1_marker.rs.data");
include!("hex_digit_v1_marker.rs.data");
include!("id_continue_v1_marker.rs.data");
include!("id_start_v1_marker.rs.data");
include!("ids_binary_operator_v1_marker.rs.data");
include!("ids_trinary_operator_v1_marker.rs.data");
include!("ideographic_v1_marker.rs.data");
include!("join_control_v1_marker.rs.data");
include!("logical_order_exception_v1_marker.rs.data");
include!("lowercase_v1_marker.rs.data");
include!("math_v1_marker.rs.data");
include!("noncharacter_code_point_v1_marker.rs.data");
include!("pattern_syntax_v1_marker.rs.data");
include!("pattern_white_space_v1_marker.rs.data");
include!("quotation_mark_v1_marker.rs.data");
include!("regional_indicator_v1_marker.rs.data");
include!("radical_v1_marker.rs.data");
include!("soft_dotted_v1_marker.rs.data");
include!("sentence_terminal_v1_marker.rs.data");
include!("terminal_punctuation_v1_marker.rs.data");
include!("unified_ideograph_v1_marker.rs.data");
include!("uppercase_v1_marker.rs.data");
include!("variation_selector_v1_marker.rs.data");
include!("white_space_v1_marker.rs.data");
include!("xid_continue_v1_marker.rs.data");
include!("xid_start_v1_marker.rs.data");
include!("case_map_v1_marker.rs.data");
include!("general_category_v1_marker.rs.data");
include!("script_v1_marker.rs.data");
include!("script_with_extensions_property_v1_marker.rs.data");
/// Marks a type as a data provider. You can then use macros like
/// `impl_core_helloworld_v1` to add implementations.
///
/// ```ignore
/// struct MyProvider;
/// const _: () = {
///     include!("path/to/generated/macros.rs");
///     make_provider!(MyProvider);
///     impl_core_helloworld_v1!(MyProvider);
/// }
/// ```
#[doc(hidden)]
#[macro_export]
macro_rules! __make_provider {
    ($ name : ty) => {
        #[clippy::msrv = "1.71.1"]
        impl $name {
            #[allow(dead_code)]
            pub(crate) const MUST_USE_MAKE_PROVIDER_MACRO: () = ();
        }
        icu_provider::marker::impl_data_provider_never_marker!($name);
    };
}
#[doc(inline)]
pub use __make_provider as make_provider;
#[allow(unused_macros)]
macro_rules! impl_data_provider {
    ($ provider : ty) => {
        make_provider!($provider);
        impl_collation_diacritics_v1_marker!($provider);
        impl_collation_jamo_v1_marker!($provider);
        impl_collation_metadata_v1_marker!($provider);
        impl_collation_special_primaries_v1_marker!($provider);
        impl_collation_reordering_v1_marker!($provider);
        impl_collation_root_v1_marker!($provider);
        impl_collation_tailoring_v1_marker!($provider);
        impl_canonical_compositions_v1_marker!($provider);
        impl_canonical_decomposition_data_v1_marker!($provider);
        impl_canonical_decomposition_tables_v1_marker!($provider);
        impl_compatibility_decomposition_supplement_v1_marker!($provider);
        impl_compatibility_decomposition_tables_v1_marker!($provider);
        impl_script_name_to_value_v2_marker!($provider);
        impl_ascii_hex_digit_v1_marker!($provider);
        impl_alphabetic_v1_marker!($provider);
        impl_bidi_control_v1_marker!($provider);
        impl_bidi_mirrored_v1_marker!($provider);
        impl_case_ignorable_v1_marker!($provider);
        impl_changes_when_casefolded_v1_marker!($provider);
        impl_changes_when_casemapped_v1_marker!($provider);
        impl_changes_when_nfkc_casefolded_v1_marker!($provider);
        impl_changes_when_lowercased_v1_marker!($provider);
        impl_changes_when_titlecased_v1_marker!($provider);
        impl_changes_when_uppercased_v1_marker!($provider);
        impl_cased_v1_marker!($provider);
        impl_default_ignorable_code_point_v1_marker!($provider);
        impl_dash_v1_marker!($provider);
        impl_deprecated_v1_marker!($provider);
        impl_diacritic_v1_marker!($provider);
        impl_emoji_modifier_base_v1_marker!($provider);
        impl_emoji_component_v1_marker!($provider);
        impl_emoji_modifier_v1_marker!($provider);
        impl_emoji_presentation_v1_marker!($provider);
        impl_emoji_v1_marker!($provider);
        impl_extender_v1_marker!($provider);
        impl_extended_pictographic_v1_marker!($provider);
        impl_grapheme_base_v1_marker!($provider);
        impl_grapheme_extend_v1_marker!($provider);
        impl_hex_digit_v1_marker!($provider);
        impl_id_continue_v1_marker!($provider);
        impl_id_start_v1_marker!($provider);
        impl_ids_binary_operator_v1_marker!($provider);
        impl_ids_trinary_operator_v1_marker!($provider);
        impl_ideographic_v1_marker!($provider);
        impl_join_control_v1_marker!($provider);
        impl_logical_order_exception_v1_marker!($provider);
        impl_lowercase_v1_marker!($provider);
        impl_math_v1_marker!($provider);
        impl_noncharacter_code_point_v1_marker!($provider);
        impl_pattern_syntax_v1_marker!($provider);
        impl_pattern_white_space_v1_marker!($provider);
        impl_quotation_mark_v1_marker!($provider);
        impl_regional_indicator_v1_marker!($provider);
        impl_radical_v1_marker!($provider);
        impl_soft_dotted_v1_marker!($provider);
        impl_sentence_terminal_v1_marker!($provider);
        impl_terminal_punctuation_v1_marker!($provider);
        impl_unified_ideograph_v1_marker!($provider);
        impl_uppercase_v1_marker!($provider);
        impl_variation_selector_v1_marker!($provider);
        impl_white_space_v1_marker!($provider);
        impl_xid_continue_v1_marker!($provider);
        impl_xid_start_v1_marker!($provider);
        impl_case_map_v1_marker!($provider);
        impl_general_category_v1_marker!($provider);
        impl_script_v1_marker!($provider);
        impl_script_with_extensions_property_v1_marker!($provider);
    };
}
#[allow(unused_macros)]
macro_rules! impl_any_provider {
    ($ provider : ty) => {
        #[clippy::msrv = "1.71.1"]
        impl icu_provider::any::AnyProvider for $provider {
            fn load_any(&self, marker: icu_provider::DataMarkerInfo, req: icu_provider::DataRequest) -> Result<icu_provider::AnyResponse, icu_provider::DataError> {
                match marker.path.hashed() {
                    h if h == <icu_collator::provider::CollationDiacriticsV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_collator::provider::CollationDiacriticsV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_collator::provider::CollationJamoV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_collator::provider::CollationJamoV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_collator::provider::CollationMetadataV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_collator::provider::CollationMetadataV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_collator::provider::CollationSpecialPrimariesV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_collator::provider::CollationSpecialPrimariesV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_collator::provider::CollationReorderingV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_collator::provider::CollationReorderingV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_collator::provider::CollationRootV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_collator::provider::CollationRootV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_collator::provider::CollationTailoringV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_collator::provider::CollationTailoringV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_normalizer::provider::CanonicalCompositionsV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_normalizer::provider::CanonicalCompositionsV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_normalizer::provider::CanonicalDecompositionDataV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_normalizer::provider::CanonicalDecompositionDataV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_normalizer::provider::CanonicalDecompositionTablesV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_normalizer::provider::CanonicalDecompositionTablesV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_normalizer::provider::CompatibilityDecompositionSupplementV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_normalizer::provider::CompatibilityDecompositionSupplementV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_normalizer::provider::CompatibilityDecompositionTablesV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_normalizer::provider::CompatibilityDecompositionTablesV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ScriptNameToValueV2Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::ScriptNameToValueV2Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::AsciiHexDigitV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::AsciiHexDigitV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::AlphabeticV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::AlphabeticV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::BidiControlV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::BidiControlV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::BidiMirroredV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::BidiMirroredV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::CaseIgnorableV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::CaseIgnorableV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ChangesWhenCasefoldedV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::ChangesWhenCasefoldedV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ChangesWhenCasemappedV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::ChangesWhenCasemappedV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ChangesWhenNfkcCasefoldedV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::ChangesWhenNfkcCasefoldedV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ChangesWhenLowercasedV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::ChangesWhenLowercasedV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ChangesWhenTitlecasedV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::ChangesWhenTitlecasedV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ChangesWhenUppercasedV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::ChangesWhenUppercasedV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::CasedV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::CasedV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::DefaultIgnorableCodePointV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::DefaultIgnorableCodePointV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::DashV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::DashV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::DeprecatedV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::DeprecatedV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::DiacriticV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::DiacriticV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::EmojiModifierBaseV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::EmojiModifierBaseV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::EmojiComponentV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::EmojiComponentV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::EmojiModifierV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::EmojiModifierV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::EmojiPresentationV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::EmojiPresentationV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::EmojiV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::EmojiV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ExtenderV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::ExtenderV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ExtendedPictographicV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::ExtendedPictographicV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::GraphemeBaseV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::GraphemeBaseV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::GraphemeExtendV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::GraphemeExtendV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::HexDigitV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::HexDigitV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::IdContinueV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::IdContinueV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::IdStartV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::IdStartV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::IdsBinaryOperatorV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::IdsBinaryOperatorV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::IdsTrinaryOperatorV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::IdsTrinaryOperatorV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::IdeographicV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::IdeographicV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::JoinControlV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::JoinControlV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::LogicalOrderExceptionV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::LogicalOrderExceptionV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::LowercaseV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::LowercaseV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::MathV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::MathV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::NoncharacterCodePointV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::NoncharacterCodePointV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::PatternSyntaxV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::PatternSyntaxV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::PatternWhiteSpaceV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::PatternWhiteSpaceV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::QuotationMarkV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::QuotationMarkV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::RegionalIndicatorV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::RegionalIndicatorV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::RadicalV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::RadicalV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::SoftDottedV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::SoftDottedV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::SentenceTerminalV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::SentenceTerminalV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::TerminalPunctuationV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::TerminalPunctuationV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::UnifiedIdeographV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::UnifiedIdeographV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::UppercaseV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::UppercaseV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::VariationSelectorV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::VariationSelectorV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::WhiteSpaceV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::WhiteSpaceV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::XidContinueV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::XidContinueV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::XidStartV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::XidStartV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_casemap::provider::CaseMapV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_casemap::provider::CaseMapV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::GeneralCategoryV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::GeneralCategoryV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ScriptV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::ScriptV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ScriptWithExtensionsPropertyV1Marker as icu_provider::DataMarker>::INFO.path.hashed() => icu_provider::DataProvider::<icu_properties::provider::ScriptWithExtensionsPropertyV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    _ => Err(icu_provider::DataErrorKind::MarkerNotFound.with_req(marker, req)),
                }
            }
        }
    };
}
