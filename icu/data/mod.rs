// @generated
include!("normalizer_nfd_tables_v1.rs.data");
include!("property_binary_pattern_syntax_v1.rs.data");
include!("property_binary_changes_when_lowercased_v1.rs.data");
include!("property_binary_ids_trinary_operator_v1.rs.data");
include!("property_binary_regional_indicator_v1.rs.data");
include!("property_binary_changes_when_uppercased_v1.rs.data");
include!("property_binary_changes_when_casemapped_v1.rs.data");
include!("property_enum_script_v1.rs.data");
include!("normalizer_nfkd_data_v1.rs.data");
include!("property_binary_ids_binary_operator_v1.rs.data");
include!("property_binary_radical_v1.rs.data");
include!("property_binary_extender_v1.rs.data");
include!("property_binary_emoji_component_v1.rs.data");
include!("collation_reordering_v1.rs.data");
include!("normalizer_nfkd_tables_v1.rs.data");
include!("property_binary_dash_v1.rs.data");
include!("property_enum_general_category_v1.rs.data");
include!("property_binary_emoji_presentation_v1.rs.data");
include!("collation_diacritics_v1.rs.data");
include!("property_binary_bidi_control_v1.rs.data");
include!("property_binary_white_space_v1.rs.data");
include!("property_binary_unified_ideograph_v1.rs.data");
include!("property_binary_noncharacter_code_point_v1.rs.data");
include!("property_script_with_extensions_v1.rs.data");
include!("property_binary_emoji_v1.rs.data");
include!("property_binary_bidi_mirrored_v1.rs.data");
include!("property_binary_changes_when_casefolded_v1.rs.data");
include!("collation_jamo_v1.rs.data");
include!("property_binary_quotation_mark_v1.rs.data");
include!("property_binary_deprecated_v1.rs.data");
include!("property_binary_xid_start_v1.rs.data");
include!("collation_metadata_v1.rs.data");
include!("property_binary_variation_selector_v1.rs.data");
include!("property_binary_terminal_punctuation_v1.rs.data");
include!("normalizer_nfc_v1.rs.data");
include!("property_binary_cased_v1.rs.data");
include!("collation_tailoring_v1.rs.data");
include!("property_binary_id_continue_v1.rs.data");
include!("property_binary_basic_emoji_v1.rs.data");
include!("property_binary_id_start_v1.rs.data");
include!("property_binary_uppercase_v1.rs.data");
include!("property_binary_changes_when_nfkc_casefolded_v1.rs.data");
include!("property_binary_hex_digit_v1.rs.data");
include!("property_binary_xid_continue_v1.rs.data");
include!("property_binary_soft_dotted_v1.rs.data");
include!("property_binary_ideographic_v1.rs.data");
include!("property_binary_changes_when_titlecased_v1.rs.data");
include!("property_binary_sentence_terminal_v1.rs.data");
include!("property_binary_ascii_hex_digit_v1.rs.data");
include!("collation_special_primaries_v1.rs.data");
include!("property_binary_logical_order_exception_v1.rs.data");
include!("property_binary_case_ignorable_v1.rs.data");
include!("normalizer_nfd_data_v1.rs.data");
include!("property_binary_diacritic_v1.rs.data");
include!("property_binary_grapheme_extend_v1.rs.data");
include!("property_name_parse_script_v1.rs.data");
include!("property_binary_lowercase_v1.rs.data");
include!("property_binary_emoji_modifier_base_v1.rs.data");
include!("property_binary_grapheme_base_v1.rs.data");
include!("property_binary_emoji_modifier_v1.rs.data");
include!("property_binary_join_control_v1.rs.data");
include!("property_binary_math_v1.rs.data");
include!("property_binary_pattern_white_space_v1.rs.data");
include!("collation_root_v1.rs.data");
include!("case_map_v1.rs.data");
include!("property_binary_alphabetic_v1.rs.data");
include!("property_binary_default_ignorable_code_point_v1.rs.data");
include!("property_binary_extended_pictographic_v1.rs.data");
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
        #[clippy::msrv = "1.82"]
        impl $name {
            #[allow(dead_code)]
            pub(crate) const MUST_USE_MAKE_PROVIDER_MACRO: () = ();
        }
        icu_provider::marker::impl_data_provider_never_marker!($name);
    };
}
#[doc(inline)]
pub use __make_provider as make_provider;
/// This macro requires the following crates:
/// * `icu_casemap`
/// * `icu_collator`
/// * `icu_collections`
/// * `icu_locale/compiled_data`
/// * `icu_normalizer`
/// * `icu_properties`
/// * `icu_provider`
/// * `icu_provider/baked`
/// * `zerotrie`
/// * `zerovec`
#[allow(unused_macros)]
macro_rules! impl_data_provider {
    ($ provider : ty) => {
        make_provider!($provider);
        impl_normalizer_nfd_tables_v1!($provider);
        impl_property_binary_pattern_syntax_v1!($provider);
        impl_property_binary_changes_when_lowercased_v1!($provider);
        impl_property_binary_ids_trinary_operator_v1!($provider);
        impl_property_binary_regional_indicator_v1!($provider);
        impl_property_binary_changes_when_uppercased_v1!($provider);
        impl_property_binary_changes_when_casemapped_v1!($provider);
        impl_property_enum_script_v1!($provider);
        impl_normalizer_nfkd_data_v1!($provider);
        impl_property_binary_ids_binary_operator_v1!($provider);
        impl_property_binary_radical_v1!($provider);
        impl_property_binary_extender_v1!($provider);
        impl_property_binary_emoji_component_v1!($provider);
        impl_collation_reordering_v1!($provider);
        impl_normalizer_nfkd_tables_v1!($provider);
        impl_property_binary_dash_v1!($provider);
        impl_property_enum_general_category_v1!($provider);
        impl_property_binary_emoji_presentation_v1!($provider);
        impl_collation_diacritics_v1!($provider);
        impl_property_binary_bidi_control_v1!($provider);
        impl_property_binary_white_space_v1!($provider);
        impl_property_binary_unified_ideograph_v1!($provider);
        impl_property_binary_noncharacter_code_point_v1!($provider);
        impl_property_script_with_extensions_v1!($provider);
        impl_property_binary_emoji_v1!($provider);
        impl_property_binary_bidi_mirrored_v1!($provider);
        impl_property_binary_changes_when_casefolded_v1!($provider);
        impl_collation_jamo_v1!($provider);
        impl_property_binary_quotation_mark_v1!($provider);
        impl_property_binary_deprecated_v1!($provider);
        impl_property_binary_xid_start_v1!($provider);
        impl_collation_metadata_v1!($provider);
        impl_property_binary_variation_selector_v1!($provider);
        impl_property_binary_terminal_punctuation_v1!($provider);
        impl_normalizer_nfc_v1!($provider);
        impl_property_binary_cased_v1!($provider);
        impl_collation_tailoring_v1!($provider);
        impl_property_binary_id_continue_v1!($provider);
        impl_property_binary_basic_emoji_v1!($provider);
        impl_property_binary_id_start_v1!($provider);
        impl_property_binary_uppercase_v1!($provider);
        impl_property_binary_changes_when_nfkc_casefolded_v1!($provider);
        impl_property_binary_hex_digit_v1!($provider);
        impl_property_binary_xid_continue_v1!($provider);
        impl_property_binary_soft_dotted_v1!($provider);
        impl_property_binary_ideographic_v1!($provider);
        impl_property_binary_changes_when_titlecased_v1!($provider);
        impl_property_binary_sentence_terminal_v1!($provider);
        impl_property_binary_ascii_hex_digit_v1!($provider);
        impl_collation_special_primaries_v1!($provider);
        impl_property_binary_logical_order_exception_v1!($provider);
        impl_property_binary_case_ignorable_v1!($provider);
        impl_normalizer_nfd_data_v1!($provider);
        impl_property_binary_diacritic_v1!($provider);
        impl_property_binary_grapheme_extend_v1!($provider);
        impl_property_name_parse_script_v1!($provider);
        impl_property_binary_lowercase_v1!($provider);
        impl_property_binary_emoji_modifier_base_v1!($provider);
        impl_property_binary_grapheme_base_v1!($provider);
        impl_property_binary_emoji_modifier_v1!($provider);
        impl_property_binary_join_control_v1!($provider);
        impl_property_binary_math_v1!($provider);
        impl_property_binary_pattern_white_space_v1!($provider);
        impl_collation_root_v1!($provider);
        impl_case_map_v1!($provider);
        impl_property_binary_alphabetic_v1!($provider);
        impl_property_binary_default_ignorable_code_point_v1!($provider);
        impl_property_binary_extended_pictographic_v1!($provider);
    };
}
