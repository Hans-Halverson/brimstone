// @generated
include!("macros.rs");
macro_rules! impl_data_provider {
    ($ provider : ty) => {
        make_provider!($provider);
        impl_collator_data_v1!($provider);
        impl_collator_dia_v1!($provider);
        impl_collator_jamo_v1!($provider);
        impl_collator_meta_v1!($provider);
        impl_collator_prim_v1!($provider);
        impl_collator_reord_v1!($provider);
        impl_fallback_likelysubtags_v1!($provider);
        impl_fallback_parents_v1!($provider);
        impl_fallback_supplement_co_v1!($provider);
        impl_normalizer_comp_v1!($provider);
        impl_normalizer_nfd_v1!($provider);
        impl_normalizer_nfdex_v1!($provider);
        impl_normalizer_nfkd_v1!($provider);
        impl_normalizer_nfkdex_v1!($provider);
        impl_propnames_from_sc_v1!($provider);
        impl_props_ahex_v1!($provider);
        impl_props_alpha_v1!($provider);
        impl_props_bidi_c_v1!($provider);
        impl_props_bidi_m_v1!($provider);
        impl_props_ci_v1!($provider);
        impl_props_cwcf_v1!($provider);
        impl_props_cwcm_v1!($provider);
        impl_props_cwkcf_v1!($provider);
        impl_props_cwl_v1!($provider);
        impl_props_cwt_v1!($provider);
        impl_props_cwu_v1!($provider);
        impl_props_cased_v1!($provider);
        impl_props_di_v1!($provider);
        impl_props_dash_v1!($provider);
        impl_props_dep_v1!($provider);
        impl_props_dia_v1!($provider);
        impl_props_ebase_v1!($provider);
        impl_props_ecomp_v1!($provider);
        impl_props_emod_v1!($provider);
        impl_props_epres_v1!($provider);
        impl_props_emoji_v1!($provider);
        impl_props_ext_v1!($provider);
        impl_props_extpict_v1!($provider);
        impl_props_gr_base_v1!($provider);
        impl_props_gr_ext_v1!($provider);
        impl_props_hex_v1!($provider);
        impl_props_idc_v1!($provider);
        impl_props_ids_v1!($provider);
        impl_props_idsb_v1!($provider);
        impl_props_idst_v1!($provider);
        impl_props_ideo_v1!($provider);
        impl_props_join_c_v1!($provider);
        impl_props_loe_v1!($provider);
        impl_props_lower_v1!($provider);
        impl_props_math_v1!($provider);
        impl_props_nchar_v1!($provider);
        impl_props_pat_syn_v1!($provider);
        impl_props_pat_ws_v1!($provider);
        impl_props_qmark_v1!($provider);
        impl_props_ri_v1!($provider);
        impl_props_radical_v1!($provider);
        impl_props_sd_v1!($provider);
        impl_props_sterm_v1!($provider);
        impl_props_term_v1!($provider);
        impl_props_uideo_v1!($provider);
        impl_props_upper_v1!($provider);
        impl_props_vs_v1!($provider);
        impl_props_wspace_v1!($provider);
        impl_props_xidc_v1!($provider);
        impl_props_xids_v1!($provider);
        impl_props_casemap_v1!($provider);
        impl_props_gc_v1!($provider);
        impl_props_scx_v1!($provider);
    };
}
#[allow(unused_macros)]
macro_rules! impl_any_provider {
    ($ provider : ty) => {
        #[clippy::msrv = "1.67"]
        impl icu_provider::AnyProvider for $provider {
            fn load_any(&self, key: icu_provider::DataKey, req: icu_provider::DataRequest) -> Result<icu_provider::AnyResponse, icu_provider::DataError> {
                match key.hashed() {
                    h if h == <icu_collator::provider::CollationDataV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_collator::provider::CollationDataV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_collator::provider::CollationDiacriticsV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_collator::provider::CollationDiacriticsV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_collator::provider::CollationJamoV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_collator::provider::CollationJamoV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_collator::provider::CollationMetadataV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_collator::provider::CollationMetadataV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_collator::provider::CollationSpecialPrimariesV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_collator::provider::CollationSpecialPrimariesV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_collator::provider::CollationReorderingV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_collator::provider::CollationReorderingV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_locid_transform::provider::LocaleFallbackLikelySubtagsV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_locid_transform::provider::LocaleFallbackLikelySubtagsV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_locid_transform::provider::LocaleFallbackParentsV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_locid_transform::provider::LocaleFallbackParentsV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_locid_transform::provider::CollationFallbackSupplementV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_locid_transform::provider::CollationFallbackSupplementV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_normalizer::provider::CanonicalCompositionsV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_normalizer::provider::CanonicalCompositionsV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_normalizer::provider::CanonicalDecompositionDataV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_normalizer::provider::CanonicalDecompositionDataV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_normalizer::provider::CanonicalDecompositionTablesV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_normalizer::provider::CanonicalDecompositionTablesV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_normalizer::provider::CompatibilityDecompositionSupplementV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_normalizer::provider::CompatibilityDecompositionSupplementV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_normalizer::provider::CompatibilityDecompositionTablesV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_normalizer::provider::CompatibilityDecompositionTablesV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ScriptNameToValueV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::ScriptNameToValueV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::AsciiHexDigitV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::AsciiHexDigitV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::AlphabeticV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::AlphabeticV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::BidiControlV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::BidiControlV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::BidiMirroredV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::BidiMirroredV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::CaseIgnorableV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::CaseIgnorableV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ChangesWhenCasefoldedV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::ChangesWhenCasefoldedV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ChangesWhenCasemappedV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::ChangesWhenCasemappedV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ChangesWhenNfkcCasefoldedV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::ChangesWhenNfkcCasefoldedV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ChangesWhenLowercasedV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::ChangesWhenLowercasedV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ChangesWhenTitlecasedV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::ChangesWhenTitlecasedV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ChangesWhenUppercasedV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::ChangesWhenUppercasedV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::CasedV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::CasedV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::DefaultIgnorableCodePointV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::DefaultIgnorableCodePointV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::DashV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::DashV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::DeprecatedV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::DeprecatedV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::DiacriticV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::DiacriticV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::EmojiModifierBaseV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::EmojiModifierBaseV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::EmojiComponentV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::EmojiComponentV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::EmojiModifierV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::EmojiModifierV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::EmojiPresentationV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::EmojiPresentationV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::EmojiV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::EmojiV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ExtenderV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::ExtenderV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ExtendedPictographicV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::ExtendedPictographicV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::GraphemeBaseV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::GraphemeBaseV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::GraphemeExtendV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::GraphemeExtendV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::HexDigitV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::HexDigitV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::IdContinueV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::IdContinueV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::IdStartV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::IdStartV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::IdsBinaryOperatorV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::IdsBinaryOperatorV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::IdsTrinaryOperatorV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::IdsTrinaryOperatorV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::IdeographicV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::IdeographicV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::JoinControlV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::JoinControlV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::LogicalOrderExceptionV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::LogicalOrderExceptionV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::LowercaseV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::LowercaseV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::MathV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::MathV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::NoncharacterCodePointV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::NoncharacterCodePointV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::PatternSyntaxV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::PatternSyntaxV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::PatternWhiteSpaceV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::PatternWhiteSpaceV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::QuotationMarkV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::QuotationMarkV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::RegionalIndicatorV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::RegionalIndicatorV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::RadicalV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::RadicalV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::SoftDottedV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::SoftDottedV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::SentenceTerminalV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::SentenceTerminalV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::TerminalPunctuationV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::TerminalPunctuationV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::UnifiedIdeographV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::UnifiedIdeographV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::UppercaseV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::UppercaseV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::VariationSelectorV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::VariationSelectorV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::WhiteSpaceV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::WhiteSpaceV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::XidContinueV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::XidContinueV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::XidStartV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::XidStartV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_casemap::provider::CaseMapV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_casemap::provider::CaseMapV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::GeneralCategoryV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::GeneralCategoryV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    h if h == <icu_properties::provider::ScriptWithExtensionsPropertyV1Marker as icu_provider::KeyedDataMarker>::KEY.hashed() => icu_provider::DataProvider::<icu_properties::provider::ScriptWithExtensionsPropertyV1Marker>::load(self, req).map(icu_provider::DataResponse::wrap_into_any_response),
                    _ => Err(icu_provider::DataErrorKind::MissingDataKey.with_req(key, req)),
                }
            }
        }
    };
}
#[clippy::msrv = "1.67"]
pub struct BakedDataProvider;
impl_data_provider!(BakedDataProvider);
