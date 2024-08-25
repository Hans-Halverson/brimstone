// @generated
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
        #[clippy::msrv = "1.67"]
        impl $name {
            #[doc(hidden)]
            #[allow(dead_code)]
            pub const MUST_USE_MAKE_PROVIDER_MACRO: () = ();
        }
        icu_provider::impl_data_provider_never_marker!($name);
    };
}
#[doc(inline)]
pub use __make_provider as make_provider;
#[macro_use]
#[path = "macros/collator_data_v1.rs.data"]
mod collator_data_v1;
#[doc(inline)]
pub use __impl_collator_data_v1 as impl_collator_data_v1;
#[doc(inline)]
pub use __impliterable_collator_data_v1 as impliterable_collator_data_v1;
#[macro_use]
#[path = "macros/collator_dia_v1.rs.data"]
mod collator_dia_v1;
#[doc(inline)]
pub use __impl_collator_dia_v1 as impl_collator_dia_v1;
#[doc(inline)]
pub use __impliterable_collator_dia_v1 as impliterable_collator_dia_v1;
#[macro_use]
#[path = "macros/collator_jamo_v1.rs.data"]
mod collator_jamo_v1;
#[doc(inline)]
pub use __impl_collator_jamo_v1 as impl_collator_jamo_v1;
#[doc(inline)]
pub use __impliterable_collator_jamo_v1 as impliterable_collator_jamo_v1;
#[macro_use]
#[path = "macros/collator_meta_v1.rs.data"]
mod collator_meta_v1;
#[doc(inline)]
pub use __impl_collator_meta_v1 as impl_collator_meta_v1;
#[doc(inline)]
pub use __impliterable_collator_meta_v1 as impliterable_collator_meta_v1;
#[macro_use]
#[path = "macros/collator_prim_v1.rs.data"]
mod collator_prim_v1;
#[doc(inline)]
pub use __impl_collator_prim_v1 as impl_collator_prim_v1;
#[doc(inline)]
pub use __impliterable_collator_prim_v1 as impliterable_collator_prim_v1;
#[macro_use]
#[path = "macros/collator_reord_v1.rs.data"]
mod collator_reord_v1;
#[doc(inline)]
pub use __impl_collator_reord_v1 as impl_collator_reord_v1;
#[doc(inline)]
pub use __impliterable_collator_reord_v1 as impliterable_collator_reord_v1;
#[macro_use]
#[path = "macros/fallback_likelysubtags_v1.rs.data"]
mod fallback_likelysubtags_v1;
#[doc(inline)]
pub use __impl_fallback_likelysubtags_v1 as impl_fallback_likelysubtags_v1;
#[doc(inline)]
pub use __impliterable_fallback_likelysubtags_v1 as impliterable_fallback_likelysubtags_v1;
#[macro_use]
#[path = "macros/fallback_parents_v1.rs.data"]
mod fallback_parents_v1;
#[doc(inline)]
pub use __impl_fallback_parents_v1 as impl_fallback_parents_v1;
#[doc(inline)]
pub use __impliterable_fallback_parents_v1 as impliterable_fallback_parents_v1;
#[macro_use]
#[path = "macros/fallback_supplement_co_v1.rs.data"]
mod fallback_supplement_co_v1;
#[doc(inline)]
pub use __impl_fallback_supplement_co_v1 as impl_fallback_supplement_co_v1;
#[doc(inline)]
pub use __impliterable_fallback_supplement_co_v1 as impliterable_fallback_supplement_co_v1;
#[macro_use]
#[path = "macros/normalizer_comp_v1.rs.data"]
mod normalizer_comp_v1;
#[doc(inline)]
pub use __impl_normalizer_comp_v1 as impl_normalizer_comp_v1;
#[doc(inline)]
pub use __impliterable_normalizer_comp_v1 as impliterable_normalizer_comp_v1;
#[macro_use]
#[path = "macros/normalizer_nfd_v1.rs.data"]
mod normalizer_nfd_v1;
#[doc(inline)]
pub use __impl_normalizer_nfd_v1 as impl_normalizer_nfd_v1;
#[doc(inline)]
pub use __impliterable_normalizer_nfd_v1 as impliterable_normalizer_nfd_v1;
#[macro_use]
#[path = "macros/normalizer_nfdex_v1.rs.data"]
mod normalizer_nfdex_v1;
#[doc(inline)]
pub use __impl_normalizer_nfdex_v1 as impl_normalizer_nfdex_v1;
#[doc(inline)]
pub use __impliterable_normalizer_nfdex_v1 as impliterable_normalizer_nfdex_v1;
#[macro_use]
#[path = "macros/normalizer_nfkd_v1.rs.data"]
mod normalizer_nfkd_v1;
#[doc(inline)]
pub use __impl_normalizer_nfkd_v1 as impl_normalizer_nfkd_v1;
#[doc(inline)]
pub use __impliterable_normalizer_nfkd_v1 as impliterable_normalizer_nfkd_v1;
#[macro_use]
#[path = "macros/normalizer_nfkdex_v1.rs.data"]
mod normalizer_nfkdex_v1;
#[doc(inline)]
pub use __impl_normalizer_nfkdex_v1 as impl_normalizer_nfkdex_v1;
#[doc(inline)]
pub use __impliterable_normalizer_nfkdex_v1 as impliterable_normalizer_nfkdex_v1;
#[macro_use]
#[path = "macros/propnames_from_sc_v1.rs.data"]
mod propnames_from_sc_v1;
#[doc(inline)]
pub use __impl_propnames_from_sc_v1 as impl_propnames_from_sc_v1;
#[doc(inline)]
pub use __impliterable_propnames_from_sc_v1 as impliterable_propnames_from_sc_v1;
#[macro_use]
#[path = "macros/props_ahex_v1.rs.data"]
mod props_ahex_v1;
#[doc(inline)]
pub use __impl_props_ahex_v1 as impl_props_ahex_v1;
#[doc(inline)]
pub use __impliterable_props_ahex_v1 as impliterable_props_ahex_v1;
#[macro_use]
#[path = "macros/props_alpha_v1.rs.data"]
mod props_alpha_v1;
#[doc(inline)]
pub use __impl_props_alpha_v1 as impl_props_alpha_v1;
#[doc(inline)]
pub use __impliterable_props_alpha_v1 as impliterable_props_alpha_v1;
#[macro_use]
#[path = "macros/props_bidi_c_v1.rs.data"]
mod props_bidi_c_v1;
#[doc(inline)]
pub use __impl_props_bidi_c_v1 as impl_props_bidi_c_v1;
#[doc(inline)]
pub use __impliterable_props_bidi_c_v1 as impliterable_props_bidi_c_v1;
#[macro_use]
#[path = "macros/props_bidi_m_v1.rs.data"]
mod props_bidi_m_v1;
#[doc(inline)]
pub use __impl_props_bidi_m_v1 as impl_props_bidi_m_v1;
#[doc(inline)]
pub use __impliterable_props_bidi_m_v1 as impliterable_props_bidi_m_v1;
#[macro_use]
#[path = "macros/props_ci_v1.rs.data"]
mod props_ci_v1;
#[doc(inline)]
pub use __impl_props_ci_v1 as impl_props_ci_v1;
#[doc(inline)]
pub use __impliterable_props_ci_v1 as impliterable_props_ci_v1;
#[macro_use]
#[path = "macros/props_cwcf_v1.rs.data"]
mod props_cwcf_v1;
#[doc(inline)]
pub use __impl_props_cwcf_v1 as impl_props_cwcf_v1;
#[doc(inline)]
pub use __impliterable_props_cwcf_v1 as impliterable_props_cwcf_v1;
#[macro_use]
#[path = "macros/props_cwcm_v1.rs.data"]
mod props_cwcm_v1;
#[doc(inline)]
pub use __impl_props_cwcm_v1 as impl_props_cwcm_v1;
#[doc(inline)]
pub use __impliterable_props_cwcm_v1 as impliterable_props_cwcm_v1;
#[macro_use]
#[path = "macros/props_cwkcf_v1.rs.data"]
mod props_cwkcf_v1;
#[doc(inline)]
pub use __impl_props_cwkcf_v1 as impl_props_cwkcf_v1;
#[doc(inline)]
pub use __impliterable_props_cwkcf_v1 as impliterable_props_cwkcf_v1;
#[macro_use]
#[path = "macros/props_cwl_v1.rs.data"]
mod props_cwl_v1;
#[doc(inline)]
pub use __impl_props_cwl_v1 as impl_props_cwl_v1;
#[doc(inline)]
pub use __impliterable_props_cwl_v1 as impliterable_props_cwl_v1;
#[macro_use]
#[path = "macros/props_cwt_v1.rs.data"]
mod props_cwt_v1;
#[doc(inline)]
pub use __impl_props_cwt_v1 as impl_props_cwt_v1;
#[doc(inline)]
pub use __impliterable_props_cwt_v1 as impliterable_props_cwt_v1;
#[macro_use]
#[path = "macros/props_cwu_v1.rs.data"]
mod props_cwu_v1;
#[doc(inline)]
pub use __impl_props_cwu_v1 as impl_props_cwu_v1;
#[doc(inline)]
pub use __impliterable_props_cwu_v1 as impliterable_props_cwu_v1;
#[macro_use]
#[path = "macros/props_cased_v1.rs.data"]
mod props_cased_v1;
#[doc(inline)]
pub use __impl_props_cased_v1 as impl_props_cased_v1;
#[doc(inline)]
pub use __impliterable_props_cased_v1 as impliterable_props_cased_v1;
#[macro_use]
#[path = "macros/props_di_v1.rs.data"]
mod props_di_v1;
#[doc(inline)]
pub use __impl_props_di_v1 as impl_props_di_v1;
#[doc(inline)]
pub use __impliterable_props_di_v1 as impliterable_props_di_v1;
#[macro_use]
#[path = "macros/props_dash_v1.rs.data"]
mod props_dash_v1;
#[doc(inline)]
pub use __impl_props_dash_v1 as impl_props_dash_v1;
#[doc(inline)]
pub use __impliterable_props_dash_v1 as impliterable_props_dash_v1;
#[macro_use]
#[path = "macros/props_dep_v1.rs.data"]
mod props_dep_v1;
#[doc(inline)]
pub use __impl_props_dep_v1 as impl_props_dep_v1;
#[doc(inline)]
pub use __impliterable_props_dep_v1 as impliterable_props_dep_v1;
#[macro_use]
#[path = "macros/props_dia_v1.rs.data"]
mod props_dia_v1;
#[doc(inline)]
pub use __impl_props_dia_v1 as impl_props_dia_v1;
#[doc(inline)]
pub use __impliterable_props_dia_v1 as impliterable_props_dia_v1;
#[macro_use]
#[path = "macros/props_ebase_v1.rs.data"]
mod props_ebase_v1;
#[doc(inline)]
pub use __impl_props_ebase_v1 as impl_props_ebase_v1;
#[doc(inline)]
pub use __impliterable_props_ebase_v1 as impliterable_props_ebase_v1;
#[macro_use]
#[path = "macros/props_ecomp_v1.rs.data"]
mod props_ecomp_v1;
#[doc(inline)]
pub use __impl_props_ecomp_v1 as impl_props_ecomp_v1;
#[doc(inline)]
pub use __impliterable_props_ecomp_v1 as impliterable_props_ecomp_v1;
#[macro_use]
#[path = "macros/props_emod_v1.rs.data"]
mod props_emod_v1;
#[doc(inline)]
pub use __impl_props_emod_v1 as impl_props_emod_v1;
#[doc(inline)]
pub use __impliterable_props_emod_v1 as impliterable_props_emod_v1;
#[macro_use]
#[path = "macros/props_epres_v1.rs.data"]
mod props_epres_v1;
#[doc(inline)]
pub use __impl_props_epres_v1 as impl_props_epres_v1;
#[doc(inline)]
pub use __impliterable_props_epres_v1 as impliterable_props_epres_v1;
#[macro_use]
#[path = "macros/props_emoji_v1.rs.data"]
mod props_emoji_v1;
#[doc(inline)]
pub use __impl_props_emoji_v1 as impl_props_emoji_v1;
#[doc(inline)]
pub use __impliterable_props_emoji_v1 as impliterable_props_emoji_v1;
#[macro_use]
#[path = "macros/props_ext_v1.rs.data"]
mod props_ext_v1;
#[doc(inline)]
pub use __impl_props_ext_v1 as impl_props_ext_v1;
#[doc(inline)]
pub use __impliterable_props_ext_v1 as impliterable_props_ext_v1;
#[macro_use]
#[path = "macros/props_extpict_v1.rs.data"]
mod props_extpict_v1;
#[doc(inline)]
pub use __impl_props_extpict_v1 as impl_props_extpict_v1;
#[doc(inline)]
pub use __impliterable_props_extpict_v1 as impliterable_props_extpict_v1;
#[macro_use]
#[path = "macros/props_gr_base_v1.rs.data"]
mod props_gr_base_v1;
#[doc(inline)]
pub use __impl_props_gr_base_v1 as impl_props_gr_base_v1;
#[doc(inline)]
pub use __impliterable_props_gr_base_v1 as impliterable_props_gr_base_v1;
#[macro_use]
#[path = "macros/props_gr_ext_v1.rs.data"]
mod props_gr_ext_v1;
#[doc(inline)]
pub use __impl_props_gr_ext_v1 as impl_props_gr_ext_v1;
#[doc(inline)]
pub use __impliterable_props_gr_ext_v1 as impliterable_props_gr_ext_v1;
#[macro_use]
#[path = "macros/props_hex_v1.rs.data"]
mod props_hex_v1;
#[doc(inline)]
pub use __impl_props_hex_v1 as impl_props_hex_v1;
#[doc(inline)]
pub use __impliterable_props_hex_v1 as impliterable_props_hex_v1;
#[macro_use]
#[path = "macros/props_idc_v1.rs.data"]
mod props_idc_v1;
#[doc(inline)]
pub use __impl_props_idc_v1 as impl_props_idc_v1;
#[doc(inline)]
pub use __impliterable_props_idc_v1 as impliterable_props_idc_v1;
#[macro_use]
#[path = "macros/props_ids_v1.rs.data"]
mod props_ids_v1;
#[doc(inline)]
pub use __impl_props_ids_v1 as impl_props_ids_v1;
#[doc(inline)]
pub use __impliterable_props_ids_v1 as impliterable_props_ids_v1;
#[macro_use]
#[path = "macros/props_idsb_v1.rs.data"]
mod props_idsb_v1;
#[doc(inline)]
pub use __impl_props_idsb_v1 as impl_props_idsb_v1;
#[doc(inline)]
pub use __impliterable_props_idsb_v1 as impliterable_props_idsb_v1;
#[macro_use]
#[path = "macros/props_idst_v1.rs.data"]
mod props_idst_v1;
#[doc(inline)]
pub use __impl_props_idst_v1 as impl_props_idst_v1;
#[doc(inline)]
pub use __impliterable_props_idst_v1 as impliterable_props_idst_v1;
#[macro_use]
#[path = "macros/props_ideo_v1.rs.data"]
mod props_ideo_v1;
#[doc(inline)]
pub use __impl_props_ideo_v1 as impl_props_ideo_v1;
#[doc(inline)]
pub use __impliterable_props_ideo_v1 as impliterable_props_ideo_v1;
#[macro_use]
#[path = "macros/props_join_c_v1.rs.data"]
mod props_join_c_v1;
#[doc(inline)]
pub use __impl_props_join_c_v1 as impl_props_join_c_v1;
#[doc(inline)]
pub use __impliterable_props_join_c_v1 as impliterable_props_join_c_v1;
#[macro_use]
#[path = "macros/props_loe_v1.rs.data"]
mod props_loe_v1;
#[doc(inline)]
pub use __impl_props_loe_v1 as impl_props_loe_v1;
#[doc(inline)]
pub use __impliterable_props_loe_v1 as impliterable_props_loe_v1;
#[macro_use]
#[path = "macros/props_lower_v1.rs.data"]
mod props_lower_v1;
#[doc(inline)]
pub use __impl_props_lower_v1 as impl_props_lower_v1;
#[doc(inline)]
pub use __impliterable_props_lower_v1 as impliterable_props_lower_v1;
#[macro_use]
#[path = "macros/props_math_v1.rs.data"]
mod props_math_v1;
#[doc(inline)]
pub use __impl_props_math_v1 as impl_props_math_v1;
#[doc(inline)]
pub use __impliterable_props_math_v1 as impliterable_props_math_v1;
#[macro_use]
#[path = "macros/props_nchar_v1.rs.data"]
mod props_nchar_v1;
#[doc(inline)]
pub use __impl_props_nchar_v1 as impl_props_nchar_v1;
#[doc(inline)]
pub use __impliterable_props_nchar_v1 as impliterable_props_nchar_v1;
#[macro_use]
#[path = "macros/props_pat_syn_v1.rs.data"]
mod props_pat_syn_v1;
#[doc(inline)]
pub use __impl_props_pat_syn_v1 as impl_props_pat_syn_v1;
#[doc(inline)]
pub use __impliterable_props_pat_syn_v1 as impliterable_props_pat_syn_v1;
#[macro_use]
#[path = "macros/props_pat_ws_v1.rs.data"]
mod props_pat_ws_v1;
#[doc(inline)]
pub use __impl_props_pat_ws_v1 as impl_props_pat_ws_v1;
#[doc(inline)]
pub use __impliterable_props_pat_ws_v1 as impliterable_props_pat_ws_v1;
#[macro_use]
#[path = "macros/props_qmark_v1.rs.data"]
mod props_qmark_v1;
#[doc(inline)]
pub use __impl_props_qmark_v1 as impl_props_qmark_v1;
#[doc(inline)]
pub use __impliterable_props_qmark_v1 as impliterable_props_qmark_v1;
#[macro_use]
#[path = "macros/props_ri_v1.rs.data"]
mod props_ri_v1;
#[doc(inline)]
pub use __impl_props_ri_v1 as impl_props_ri_v1;
#[doc(inline)]
pub use __impliterable_props_ri_v1 as impliterable_props_ri_v1;
#[macro_use]
#[path = "macros/props_radical_v1.rs.data"]
mod props_radical_v1;
#[doc(inline)]
pub use __impl_props_radical_v1 as impl_props_radical_v1;
#[doc(inline)]
pub use __impliterable_props_radical_v1 as impliterable_props_radical_v1;
#[macro_use]
#[path = "macros/props_sd_v1.rs.data"]
mod props_sd_v1;
#[doc(inline)]
pub use __impl_props_sd_v1 as impl_props_sd_v1;
#[doc(inline)]
pub use __impliterable_props_sd_v1 as impliterable_props_sd_v1;
#[macro_use]
#[path = "macros/props_sterm_v1.rs.data"]
mod props_sterm_v1;
#[doc(inline)]
pub use __impl_props_sterm_v1 as impl_props_sterm_v1;
#[doc(inline)]
pub use __impliterable_props_sterm_v1 as impliterable_props_sterm_v1;
#[macro_use]
#[path = "macros/props_term_v1.rs.data"]
mod props_term_v1;
#[doc(inline)]
pub use __impl_props_term_v1 as impl_props_term_v1;
#[doc(inline)]
pub use __impliterable_props_term_v1 as impliterable_props_term_v1;
#[macro_use]
#[path = "macros/props_uideo_v1.rs.data"]
mod props_uideo_v1;
#[doc(inline)]
pub use __impl_props_uideo_v1 as impl_props_uideo_v1;
#[doc(inline)]
pub use __impliterable_props_uideo_v1 as impliterable_props_uideo_v1;
#[macro_use]
#[path = "macros/props_upper_v1.rs.data"]
mod props_upper_v1;
#[doc(inline)]
pub use __impl_props_upper_v1 as impl_props_upper_v1;
#[doc(inline)]
pub use __impliterable_props_upper_v1 as impliterable_props_upper_v1;
#[macro_use]
#[path = "macros/props_vs_v1.rs.data"]
mod props_vs_v1;
#[doc(inline)]
pub use __impl_props_vs_v1 as impl_props_vs_v1;
#[doc(inline)]
pub use __impliterable_props_vs_v1 as impliterable_props_vs_v1;
#[macro_use]
#[path = "macros/props_wspace_v1.rs.data"]
mod props_wspace_v1;
#[doc(inline)]
pub use __impl_props_wspace_v1 as impl_props_wspace_v1;
#[doc(inline)]
pub use __impliterable_props_wspace_v1 as impliterable_props_wspace_v1;
#[macro_use]
#[path = "macros/props_xidc_v1.rs.data"]
mod props_xidc_v1;
#[doc(inline)]
pub use __impl_props_xidc_v1 as impl_props_xidc_v1;
#[doc(inline)]
pub use __impliterable_props_xidc_v1 as impliterable_props_xidc_v1;
#[macro_use]
#[path = "macros/props_xids_v1.rs.data"]
mod props_xids_v1;
#[doc(inline)]
pub use __impl_props_xids_v1 as impl_props_xids_v1;
#[doc(inline)]
pub use __impliterable_props_xids_v1 as impliterable_props_xids_v1;
#[macro_use]
#[path = "macros/props_casemap_v1.rs.data"]
mod props_casemap_v1;
#[doc(inline)]
pub use __impl_props_casemap_v1 as impl_props_casemap_v1;
#[doc(inline)]
pub use __impliterable_props_casemap_v1 as impliterable_props_casemap_v1;
#[macro_use]
#[path = "macros/props_gc_v1.rs.data"]
mod props_gc_v1;
#[doc(inline)]
pub use __impl_props_gc_v1 as impl_props_gc_v1;
#[doc(inline)]
pub use __impliterable_props_gc_v1 as impliterable_props_gc_v1;
#[macro_use]
#[path = "macros/props_scx_v1.rs.data"]
mod props_scx_v1;
#[doc(inline)]
pub use __impl_props_scx_v1 as impl_props_scx_v1;
#[doc(inline)]
pub use __impliterable_props_scx_v1 as impliterable_props_scx_v1;
