#![allow(unused_imports)]

use icu_collections::codepointinvliststringlist::CodePointInversionListAndStringList;
use icu_provider_baked;

pub struct BakedDataProvider;
include!("../../../../icu/data/mod.rs");
impl_data_provider!(BakedDataProvider);

// Custom hardcoded data for unicode properties of strings not yet included in ICU4X.
//
// These were generated from a fork of ICU4X and will hopefully be upstreamed so they can be used
// directly.

pub const EMOJI_KEYCAP_SEQUENCE: CodePointInversionListAndStringList<'static> =
    include!("../../../../icu/custom_data/emoji_keycap_sequence.rs");

pub const RGI_EMOJI_FLAG_SEQUENCE: CodePointInversionListAndStringList<'static> =
    include!("../../../../icu/custom_data/rgi_emoji_flag_sequence.rs");

pub const RGI_EMOJI_MODIFIER_SEQUENCE: CodePointInversionListAndStringList<'static> =
    include!("../../../../icu/custom_data/rgi_emoji_modifier_sequence.rs");

pub const RGI_EMOJI_TAG_SEQUENCE: CodePointInversionListAndStringList<'static> =
    include!("../../../../icu/custom_data/rgi_emoji_tag_sequence.rs");

pub const RGI_EMOJI_ZWJ_SEQUENCE: CodePointInversionListAndStringList<'static> =
    include!("../../../../icu/custom_data/rgi_emoji_zwj_sequence.rs");

pub const RGI_EMOJI: CodePointInversionListAndStringList<'static> =
    include!("../../../../icu/custom_data/rgi_emoji.rs");
