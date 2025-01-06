#![allow(unused_imports)]

use icu_provider_baked;

pub struct BakedDataProvider;
include!("../../../../icu/data/mod.rs");
impl_data_provider!(BakedDataProvider);
