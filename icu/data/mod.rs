// @generated
mod collator;
mod fallback;
mod normalizer;
mod propnames;
mod props;
use ::icu_provider::prelude::*;
/// Implement [`DataProvider<M>`] on the given struct using the data
/// hardcoded in this module. This allows the struct to be used with
/// `icu`'s `_unstable` constructors.
///
/// This macro can only be called from its definition-site, i.e. right
/// after `include!`-ing the generated module.
///
/// ```compile_fail
/// struct MyDataProvider;
/// include!("/path/to/generated/mod.rs");
/// impl_data_provider(MyDataProvider);
/// ```
#[allow(unused_macros)]
macro_rules! impl_data_provider {
    ($ provider : path) => {
        impl DataProvider<::icu_collator::provider::CollationDataV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_collator::provider::CollationDataV1Marker>, DataError> {
                collator::data_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_collator::provider::CollationDataV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_collator::provider::CollationDiacriticsV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_collator::provider::CollationDiacriticsV1Marker>, DataError> {
                collator::dia_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_collator::provider::CollationDiacriticsV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_collator::provider::CollationJamoV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_collator::provider::CollationJamoV1Marker>, DataError> {
                collator::jamo_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_collator::provider::CollationJamoV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_collator::provider::CollationMetadataV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_collator::provider::CollationMetadataV1Marker>, DataError> {
                collator::meta_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_collator::provider::CollationMetadataV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_collator::provider::CollationReorderingV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_collator::provider::CollationReorderingV1Marker>, DataError> {
                collator::reord_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_collator::provider::CollationReorderingV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_collator::provider::CollationSpecialPrimariesV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_collator::provider::CollationSpecialPrimariesV1Marker>, DataError> {
                collator::prim_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_collator::provider::CollationSpecialPrimariesV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_normalizer::provider::CanonicalCompositionsV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_normalizer::provider::CanonicalCompositionsV1Marker>, DataError> {
                normalizer::comp_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_normalizer::provider::CanonicalCompositionsV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_normalizer::provider::CanonicalDecompositionDataV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_normalizer::provider::CanonicalDecompositionDataV1Marker>, DataError> {
                normalizer::nfd_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_normalizer::provider::CanonicalDecompositionDataV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_normalizer::provider::CanonicalDecompositionTablesV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_normalizer::provider::CanonicalDecompositionTablesV1Marker>, DataError> {
                normalizer::nfdex_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_normalizer::provider::CanonicalDecompositionTablesV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_normalizer::provider::CompatibilityDecompositionSupplementV1Marker> for $provider {
            fn load(
                &self,
                req: DataRequest,
            ) -> Result<DataResponse<::icu_normalizer::provider::CompatibilityDecompositionSupplementV1Marker>, DataError> {
                normalizer::nfkd_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| {
                        DataErrorKind::MissingLocale.with_req(
                            ::icu_normalizer::provider::CompatibilityDecompositionSupplementV1Marker::KEY,
                            req,
                        )
                    })
            }
        }
        impl DataProvider<::icu_normalizer::provider::CompatibilityDecompositionTablesV1Marker> for $provider {
            fn load(
                &self,
                req: DataRequest,
            ) -> Result<DataResponse<::icu_normalizer::provider::CompatibilityDecompositionTablesV1Marker>, DataError> {
                normalizer::nfkdex_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| {
                        DataErrorKind::MissingLocale.with_req(::icu_normalizer::provider::CompatibilityDecompositionTablesV1Marker::KEY, req)
                    })
            }
        }
        impl DataProvider<::icu_properties::provider::AlphabeticV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::AlphabeticV1Marker>, DataError> {
                props::alpha_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::AlphabeticV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::AsciiHexDigitV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::AsciiHexDigitV1Marker>, DataError> {
                props::ahex_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::AsciiHexDigitV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::BidiControlV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::BidiControlV1Marker>, DataError> {
                props::bidi_c_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::BidiControlV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::BidiMirroredV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::BidiMirroredV1Marker>, DataError> {
                props::bidi_m_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::BidiMirroredV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::CaseIgnorableV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::CaseIgnorableV1Marker>, DataError> {
                props::ci_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::CaseIgnorableV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::CasedV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::CasedV1Marker>, DataError> {
                props::cased_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::CasedV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::ChangesWhenCasefoldedV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::ChangesWhenCasefoldedV1Marker>, DataError> {
                props::cwcf_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::ChangesWhenCasefoldedV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::ChangesWhenCasemappedV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::ChangesWhenCasemappedV1Marker>, DataError> {
                props::cwcm_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::ChangesWhenCasemappedV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::ChangesWhenLowercasedV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::ChangesWhenLowercasedV1Marker>, DataError> {
                props::cwl_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::ChangesWhenLowercasedV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::ChangesWhenNfkcCasefoldedV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::ChangesWhenNfkcCasefoldedV1Marker>, DataError> {
                props::cwkcf_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::ChangesWhenNfkcCasefoldedV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::ChangesWhenTitlecasedV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::ChangesWhenTitlecasedV1Marker>, DataError> {
                props::cwt_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::ChangesWhenTitlecasedV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::ChangesWhenUppercasedV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::ChangesWhenUppercasedV1Marker>, DataError> {
                props::cwu_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::ChangesWhenUppercasedV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::DashV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::DashV1Marker>, DataError> {
                props::dash_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::DashV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::DefaultIgnorableCodePointV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::DefaultIgnorableCodePointV1Marker>, DataError> {
                props::di_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::DefaultIgnorableCodePointV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::DeprecatedV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::DeprecatedV1Marker>, DataError> {
                props::dep_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::DeprecatedV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::DiacriticV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::DiacriticV1Marker>, DataError> {
                props::dia_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::DiacriticV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::EmojiComponentV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::EmojiComponentV1Marker>, DataError> {
                props::ecomp_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::EmojiComponentV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::EmojiModifierBaseV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::EmojiModifierBaseV1Marker>, DataError> {
                props::ebase_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::EmojiModifierBaseV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::EmojiModifierV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::EmojiModifierV1Marker>, DataError> {
                props::emod_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::EmojiModifierV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::EmojiPresentationV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::EmojiPresentationV1Marker>, DataError> {
                props::epres_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::EmojiPresentationV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::EmojiV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::EmojiV1Marker>, DataError> {
                props::emoji_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::EmojiV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::ExtendedPictographicV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::ExtendedPictographicV1Marker>, DataError> {
                props::extpict_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::ExtendedPictographicV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::ExtenderV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::ExtenderV1Marker>, DataError> {
                props::ext_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::ExtenderV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::GeneralCategoryV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::GeneralCategoryV1Marker>, DataError> {
                props::gc_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::GeneralCategoryV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::GraphemeBaseV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::GraphemeBaseV1Marker>, DataError> {
                props::gr_base_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::GraphemeBaseV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::GraphemeExtendV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::GraphemeExtendV1Marker>, DataError> {
                props::gr_ext_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::GraphemeExtendV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::HexDigitV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::HexDigitV1Marker>, DataError> {
                props::hex_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::HexDigitV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::IdContinueV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::IdContinueV1Marker>, DataError> {
                props::idc_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::IdContinueV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::IdStartV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::IdStartV1Marker>, DataError> {
                props::ids_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::IdStartV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::IdeographicV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::IdeographicV1Marker>, DataError> {
                props::ideo_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::IdeographicV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::IdsBinaryOperatorV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::IdsBinaryOperatorV1Marker>, DataError> {
                props::idsb_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::IdsBinaryOperatorV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::IdsTrinaryOperatorV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::IdsTrinaryOperatorV1Marker>, DataError> {
                props::idst_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::IdsTrinaryOperatorV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::JoinControlV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::JoinControlV1Marker>, DataError> {
                props::join_c_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::JoinControlV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::LogicalOrderExceptionV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::LogicalOrderExceptionV1Marker>, DataError> {
                props::loe_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::LogicalOrderExceptionV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::LowercaseV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::LowercaseV1Marker>, DataError> {
                props::lower_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::LowercaseV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::MathV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::MathV1Marker>, DataError> {
                props::math_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::MathV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::NoncharacterCodePointV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::NoncharacterCodePointV1Marker>, DataError> {
                props::nchar_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::NoncharacterCodePointV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::PatternSyntaxV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::PatternSyntaxV1Marker>, DataError> {
                props::pat_syn_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::PatternSyntaxV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::PatternWhiteSpaceV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::PatternWhiteSpaceV1Marker>, DataError> {
                props::pat_ws_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::PatternWhiteSpaceV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::QuotationMarkV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::QuotationMarkV1Marker>, DataError> {
                props::qmark_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::QuotationMarkV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::RadicalV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::RadicalV1Marker>, DataError> {
                props::radical_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::RadicalV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::RegionalIndicatorV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::RegionalIndicatorV1Marker>, DataError> {
                props::ri_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::RegionalIndicatorV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::ScriptNameToValueV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::ScriptNameToValueV1Marker>, DataError> {
                propnames::from::sc_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::ScriptNameToValueV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::ScriptWithExtensionsPropertyV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::ScriptWithExtensionsPropertyV1Marker>, DataError> {
                props::scx_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::ScriptWithExtensionsPropertyV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::SentenceTerminalV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::SentenceTerminalV1Marker>, DataError> {
                props::sterm_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::SentenceTerminalV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::SoftDottedV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::SoftDottedV1Marker>, DataError> {
                props::sd_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::SoftDottedV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::TerminalPunctuationV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::TerminalPunctuationV1Marker>, DataError> {
                props::term_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::TerminalPunctuationV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::UnifiedIdeographV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::UnifiedIdeographV1Marker>, DataError> {
                props::uideo_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::UnifiedIdeographV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::UppercaseV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::UppercaseV1Marker>, DataError> {
                props::upper_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::UppercaseV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::VariationSelectorV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::VariationSelectorV1Marker>, DataError> {
                props::vs_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::VariationSelectorV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::WhiteSpaceV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::WhiteSpaceV1Marker>, DataError> {
                props::wspace_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::WhiteSpaceV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::XidContinueV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::XidContinueV1Marker>, DataError> {
                props::xidc_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::XidContinueV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_properties::provider::XidStartV1Marker> for $provider {
            fn load(&self, req: DataRequest) -> Result<DataResponse<::icu_properties::provider::XidStartV1Marker>, DataError> {
                props::xids_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| DataErrorKind::MissingLocale.with_req(::icu_properties::provider::XidStartV1Marker::KEY, req))
            }
        }
        impl DataProvider<::icu_provider_adapters::fallback::provider::CollationFallbackSupplementV1Marker> for $provider {
            fn load(
                &self,
                req: DataRequest,
            ) -> Result<DataResponse<::icu_provider_adapters::fallback::provider::CollationFallbackSupplementV1Marker>, DataError> {
                fallback::supplement::co_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| {
                        DataErrorKind::MissingLocale.with_req(
                            ::icu_provider_adapters::fallback::provider::CollationFallbackSupplementV1Marker::KEY,
                            req,
                        )
                    })
            }
        }
        impl DataProvider<::icu_provider_adapters::fallback::provider::LocaleFallbackLikelySubtagsV1Marker> for $provider {
            fn load(
                &self,
                req: DataRequest,
            ) -> Result<DataResponse<::icu_provider_adapters::fallback::provider::LocaleFallbackLikelySubtagsV1Marker>, DataError> {
                fallback::likelysubtags_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| {
                        DataErrorKind::MissingLocale.with_req(
                            ::icu_provider_adapters::fallback::provider::LocaleFallbackLikelySubtagsV1Marker::KEY,
                            req,
                        )
                    })
            }
        }
        impl DataProvider<::icu_provider_adapters::fallback::provider::LocaleFallbackParentsV1Marker> for $provider {
            fn load(
                &self,
                req: DataRequest,
            ) -> Result<DataResponse<::icu_provider_adapters::fallback::provider::LocaleFallbackParentsV1Marker>, DataError> {
                fallback::parents_v1::lookup(&req.locale)
                    .map(zerofrom::ZeroFrom::zero_from)
                    .map(DataPayload::from_owned)
                    .map(|payload| DataResponse { metadata: Default::default(), payload: Some(payload) })
                    .ok_or_else(|| {
                        DataErrorKind::MissingLocale.with_req(
                            ::icu_provider_adapters::fallback::provider::LocaleFallbackParentsV1Marker::KEY,
                            req,
                        )
                    })
            }
        }
    };
}
/// Implement [`AnyProvider`] on the given struct using the data
/// hardcoded in this module. This allows the struct to be used with
/// `icu`'s `_any` constructors.
///
/// This macro can only be called from its definition-site, i.e. right
/// after `include!`-ing the generated module.
///
/// ```compile_fail
/// struct MyAnyProvider;
/// include!("/path/to/generated/mod.rs");
/// impl_any_provider(MyAnyProvider);
/// ```
#[allow(unused_macros)]
macro_rules! impl_any_provider {
    ($ provider : path) => {
        impl AnyProvider for $provider {
            fn load_any(&self, key: DataKey, req: DataRequest) -> Result<AnyResponse, DataError> {
                const COLLATIONDATAV1MARKER: ::icu_provider::DataKeyHash = ::icu_collator::provider::CollationDataV1Marker::KEY.hashed();
                const COLLATIONDIACRITICSV1MARKER: ::icu_provider::DataKeyHash = ::icu_collator::provider::CollationDiacriticsV1Marker::KEY.hashed();
                const COLLATIONJAMOV1MARKER: ::icu_provider::DataKeyHash = ::icu_collator::provider::CollationJamoV1Marker::KEY.hashed();
                const COLLATIONMETADATAV1MARKER: ::icu_provider::DataKeyHash = ::icu_collator::provider::CollationMetadataV1Marker::KEY.hashed();
                const COLLATIONREORDERINGV1MARKER: ::icu_provider::DataKeyHash = ::icu_collator::provider::CollationReorderingV1Marker::KEY.hashed();
                const COLLATIONSPECIALPRIMARIESV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_collator::provider::CollationSpecialPrimariesV1Marker::KEY.hashed();
                const CANONICALCOMPOSITIONSV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_normalizer::provider::CanonicalCompositionsV1Marker::KEY.hashed();
                const CANONICALDECOMPOSITIONDATAV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_normalizer::provider::CanonicalDecompositionDataV1Marker::KEY.hashed();
                const CANONICALDECOMPOSITIONTABLESV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_normalizer::provider::CanonicalDecompositionTablesV1Marker::KEY.hashed();
                const COMPATIBILITYDECOMPOSITIONSUPPLEMENTV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_normalizer::provider::CompatibilityDecompositionSupplementV1Marker::KEY.hashed();
                const COMPATIBILITYDECOMPOSITIONTABLESV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_normalizer::provider::CompatibilityDecompositionTablesV1Marker::KEY.hashed();
                const ALPHABETICV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::AlphabeticV1Marker::KEY.hashed();
                const ASCIIHEXDIGITV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::AsciiHexDigitV1Marker::KEY.hashed();
                const BIDICONTROLV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::BidiControlV1Marker::KEY.hashed();
                const BIDIMIRROREDV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::BidiMirroredV1Marker::KEY.hashed();
                const CASEIGNORABLEV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::CaseIgnorableV1Marker::KEY.hashed();
                const CASEDV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::CasedV1Marker::KEY.hashed();
                const CHANGESWHENCASEFOLDEDV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_properties::provider::ChangesWhenCasefoldedV1Marker::KEY.hashed();
                const CHANGESWHENCASEMAPPEDV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_properties::provider::ChangesWhenCasemappedV1Marker::KEY.hashed();
                const CHANGESWHENLOWERCASEDV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_properties::provider::ChangesWhenLowercasedV1Marker::KEY.hashed();
                const CHANGESWHENNFKCCASEFOLDEDV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_properties::provider::ChangesWhenNfkcCasefoldedV1Marker::KEY.hashed();
                const CHANGESWHENTITLECASEDV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_properties::provider::ChangesWhenTitlecasedV1Marker::KEY.hashed();
                const CHANGESWHENUPPERCASEDV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_properties::provider::ChangesWhenUppercasedV1Marker::KEY.hashed();
                const DASHV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::DashV1Marker::KEY.hashed();
                const DEFAULTIGNORABLECODEPOINTV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_properties::provider::DefaultIgnorableCodePointV1Marker::KEY.hashed();
                const DEPRECATEDV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::DeprecatedV1Marker::KEY.hashed();
                const DIACRITICV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::DiacriticV1Marker::KEY.hashed();
                const EMOJICOMPONENTV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::EmojiComponentV1Marker::KEY.hashed();
                const EMOJIMODIFIERBASEV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::EmojiModifierBaseV1Marker::KEY.hashed();
                const EMOJIMODIFIERV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::EmojiModifierV1Marker::KEY.hashed();
                const EMOJIPRESENTATIONV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::EmojiPresentationV1Marker::KEY.hashed();
                const EMOJIV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::EmojiV1Marker::KEY.hashed();
                const EXTENDEDPICTOGRAPHICV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_properties::provider::ExtendedPictographicV1Marker::KEY.hashed();
                const EXTENDERV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::ExtenderV1Marker::KEY.hashed();
                const GENERALCATEGORYV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::GeneralCategoryV1Marker::KEY.hashed();
                const GRAPHEMEBASEV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::GraphemeBaseV1Marker::KEY.hashed();
                const GRAPHEMEEXTENDV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::GraphemeExtendV1Marker::KEY.hashed();
                const HEXDIGITV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::HexDigitV1Marker::KEY.hashed();
                const IDCONTINUEV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::IdContinueV1Marker::KEY.hashed();
                const IDSTARTV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::IdStartV1Marker::KEY.hashed();
                const IDEOGRAPHICV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::IdeographicV1Marker::KEY.hashed();
                const IDSBINARYOPERATORV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::IdsBinaryOperatorV1Marker::KEY.hashed();
                const IDSTRINARYOPERATORV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::IdsTrinaryOperatorV1Marker::KEY.hashed();
                const JOINCONTROLV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::JoinControlV1Marker::KEY.hashed();
                const LOGICALORDEREXCEPTIONV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_properties::provider::LogicalOrderExceptionV1Marker::KEY.hashed();
                const LOWERCASEV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::LowercaseV1Marker::KEY.hashed();
                const MATHV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::MathV1Marker::KEY.hashed();
                const NONCHARACTERCODEPOINTV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_properties::provider::NoncharacterCodePointV1Marker::KEY.hashed();
                const PATTERNSYNTAXV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::PatternSyntaxV1Marker::KEY.hashed();
                const PATTERNWHITESPACEV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::PatternWhiteSpaceV1Marker::KEY.hashed();
                const QUOTATIONMARKV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::QuotationMarkV1Marker::KEY.hashed();
                const RADICALV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::RadicalV1Marker::KEY.hashed();
                const REGIONALINDICATORV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::RegionalIndicatorV1Marker::KEY.hashed();
                const SCRIPTNAMETOVALUEV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::ScriptNameToValueV1Marker::KEY.hashed();
                const SCRIPTWITHEXTENSIONSPROPERTYV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_properties::provider::ScriptWithExtensionsPropertyV1Marker::KEY.hashed();
                const SENTENCETERMINALV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::SentenceTerminalV1Marker::KEY.hashed();
                const SOFTDOTTEDV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::SoftDottedV1Marker::KEY.hashed();
                const TERMINALPUNCTUATIONV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_properties::provider::TerminalPunctuationV1Marker::KEY.hashed();
                const UNIFIEDIDEOGRAPHV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::UnifiedIdeographV1Marker::KEY.hashed();
                const UPPERCASEV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::UppercaseV1Marker::KEY.hashed();
                const VARIATIONSELECTORV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::VariationSelectorV1Marker::KEY.hashed();
                const WHITESPACEV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::WhiteSpaceV1Marker::KEY.hashed();
                const XIDCONTINUEV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::XidContinueV1Marker::KEY.hashed();
                const XIDSTARTV1MARKER: ::icu_provider::DataKeyHash = ::icu_properties::provider::XidStartV1Marker::KEY.hashed();
                const COLLATIONFALLBACKSUPPLEMENTV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_provider_adapters::fallback::provider::CollationFallbackSupplementV1Marker::KEY.hashed();
                const LOCALEFALLBACKLIKELYSUBTAGSV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_provider_adapters::fallback::provider::LocaleFallbackLikelySubtagsV1Marker::KEY.hashed();
                const LOCALEFALLBACKPARENTSV1MARKER: ::icu_provider::DataKeyHash =
                    ::icu_provider_adapters::fallback::provider::LocaleFallbackParentsV1Marker::KEY.hashed();
                match key.hashed() {
                    COLLATIONDATAV1MARKER => collator::data_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    COLLATIONDIACRITICSV1MARKER => collator::dia_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    COLLATIONJAMOV1MARKER => collator::jamo_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    COLLATIONMETADATAV1MARKER => collator::meta_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    COLLATIONREORDERINGV1MARKER => collator::reord_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    COLLATIONSPECIALPRIMARIESV1MARKER => collator::prim_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    CANONICALCOMPOSITIONSV1MARKER => normalizer::comp_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    CANONICALDECOMPOSITIONDATAV1MARKER => normalizer::nfd_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    CANONICALDECOMPOSITIONTABLESV1MARKER => normalizer::nfdex_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    COMPATIBILITYDECOMPOSITIONSUPPLEMENTV1MARKER => normalizer::nfkd_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    COMPATIBILITYDECOMPOSITIONTABLESV1MARKER => normalizer::nfkdex_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    ALPHABETICV1MARKER => props::alpha_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    ASCIIHEXDIGITV1MARKER => props::ahex_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    BIDICONTROLV1MARKER => props::bidi_c_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    BIDIMIRROREDV1MARKER => props::bidi_m_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    CASEIGNORABLEV1MARKER => props::ci_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    CASEDV1MARKER => props::cased_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    CHANGESWHENCASEFOLDEDV1MARKER => props::cwcf_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    CHANGESWHENCASEMAPPEDV1MARKER => props::cwcm_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    CHANGESWHENLOWERCASEDV1MARKER => props::cwl_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    CHANGESWHENNFKCCASEFOLDEDV1MARKER => props::cwkcf_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    CHANGESWHENTITLECASEDV1MARKER => props::cwt_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    CHANGESWHENUPPERCASEDV1MARKER => props::cwu_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    DASHV1MARKER => props::dash_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    DEFAULTIGNORABLECODEPOINTV1MARKER => props::di_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    DEPRECATEDV1MARKER => props::dep_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    DIACRITICV1MARKER => props::dia_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    EMOJICOMPONENTV1MARKER => props::ecomp_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    EMOJIMODIFIERBASEV1MARKER => props::ebase_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    EMOJIMODIFIERV1MARKER => props::emod_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    EMOJIPRESENTATIONV1MARKER => props::epres_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    EMOJIV1MARKER => props::emoji_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    EXTENDEDPICTOGRAPHICV1MARKER => props::extpict_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    EXTENDERV1MARKER => props::ext_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    GENERALCATEGORYV1MARKER => props::gc_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    GRAPHEMEBASEV1MARKER => props::gr_base_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    GRAPHEMEEXTENDV1MARKER => props::gr_ext_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    HEXDIGITV1MARKER => props::hex_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    IDCONTINUEV1MARKER => props::idc_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    IDSTARTV1MARKER => props::ids_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    IDEOGRAPHICV1MARKER => props::ideo_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    IDSBINARYOPERATORV1MARKER => props::idsb_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    IDSTRINARYOPERATORV1MARKER => props::idst_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    JOINCONTROLV1MARKER => props::join_c_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    LOGICALORDEREXCEPTIONV1MARKER => props::loe_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    LOWERCASEV1MARKER => props::lower_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    MATHV1MARKER => props::math_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    NONCHARACTERCODEPOINTV1MARKER => props::nchar_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    PATTERNSYNTAXV1MARKER => props::pat_syn_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    PATTERNWHITESPACEV1MARKER => props::pat_ws_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    QUOTATIONMARKV1MARKER => props::qmark_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    RADICALV1MARKER => props::radical_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    REGIONALINDICATORV1MARKER => props::ri_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    SCRIPTNAMETOVALUEV1MARKER => propnames::from::sc_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    SCRIPTWITHEXTENSIONSPROPERTYV1MARKER => props::scx_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    SENTENCETERMINALV1MARKER => props::sterm_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    SOFTDOTTEDV1MARKER => props::sd_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    TERMINALPUNCTUATIONV1MARKER => props::term_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    UNIFIEDIDEOGRAPHV1MARKER => props::uideo_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    UPPERCASEV1MARKER => props::upper_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    VARIATIONSELECTORV1MARKER => props::vs_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    WHITESPACEV1MARKER => props::wspace_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    XIDCONTINUEV1MARKER => props::xidc_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    XIDSTARTV1MARKER => props::xids_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    COLLATIONFALLBACKSUPPLEMENTV1MARKER => fallback::supplement::co_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    LOCALEFALLBACKLIKELYSUBTAGSV1MARKER => fallback::likelysubtags_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    LOCALEFALLBACKPARENTSV1MARKER => fallback::parents_v1::lookup(&req.locale).map(AnyPayload::from_static_ref),
                    _ => return Err(DataErrorKind::MissingDataKey.with_req(key, req)),
                }
                .map(|payload| AnyResponse { payload: Some(payload), metadata: Default::default() })
                .ok_or_else(|| DataErrorKind::MissingLocale.with_req(key, req))
            }
        }
    };
}
pub struct BakedDataProvider;
impl_data_provider!(BakedDataProvider);
