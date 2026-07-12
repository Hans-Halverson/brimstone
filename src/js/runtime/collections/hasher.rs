use std::hash::Hasher;

use rustc_hash::FxHasher as RustcFxHasher;
use siphasher::sip::SipHasher13;

use crate::runtime::{HeapPtr, shape::Shape};

/// A hasher that can be built from a shape pointer. This allows the hasher to be specialized for a
/// particular type of map, with the hasher itself having access to the shared Context where a seed
/// can be retrieved.
pub trait BsBuildHasher {
    fn build_hasher(shape_ptr: HeapPtr<Shape>) -> impl Hasher;
}

pub struct SipHasher;

impl BsBuildHasher for SipHasher {
    fn build_hasher(_: HeapPtr<Shape>) -> impl Hasher {
        SipHasher13::new()
    }
}

pub struct FxHasher;

impl BsBuildHasher for FxHasher {
    fn build_hasher(_: HeapPtr<Shape>) -> impl Hasher {
        RustcFxHasher::default()
    }
}

/// Hasher that when seeded is resistant to Hash DoS attacks.
pub type HashDosResistantHasher = SipHasher;

/// A hasher optimizing for speed rather than any security properties.
pub type FastHasher = FxHasher;
