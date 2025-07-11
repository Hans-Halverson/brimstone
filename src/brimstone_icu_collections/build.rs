use icu_casemap::CaseMapper;
use icu_collections::codepointinvlist::{CodePointInversionList, CodePointInversionListBuilder};

use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::path::Path;

/// Generate the `brimstone_icu_collections` module, which exposes the public interface:
///
/// ```
/// pub fn has_case_closure_override(c: char) -> bool;
/// pub fn get_case_closure_override(c: char) -> Option<&'static CodePointInversionList<'static>>;
/// pub fn all_case_folded_set() -> &'static CodePointInversionList<'static>;
/// ```
///
/// ## Case Closure Overrides
///
/// The Canonicalize abstract operation in non-unicode mode differs slightly from standard
/// Unicode case folding/mapping procedures. Specifically it canonicalizes code points using the
/// simple uppercase mapping (instead of case folding), but avoids mapping any code point from
/// outside the Latin1 range to inside the Latin1 range.
///
/// For case insensitive matching we do not canonicalize the input string, but instead generate the
/// case insensitive closure of the input pattern. For example each literal in the pattern is
/// replaced with the set of characters that canonicalize to the same value as that literal (this
/// set is the case closure).
///
/// We use the `add_case_closure_to` method from icu4x to generate the case closure using case
/// folding. This is always used when in unicode aware mode. However, in non-unicode mode this
/// method has a handful of code points that are incorrectly mapped due to the quirks of the
/// Canonicalize operation. This build script precomputes the correct case closures for these code
/// points and refers to them as case closure overrides. Case insensitive non-unicode matching then
/// uses these case closure overrides if they exist, and otherwise uses `add_case_closure_to`.
///
/// ## All Case Folded Characters
///
/// The `all_case_folded_set` function returns a set of all code points that map to themselves under
/// case folding. This is needed to generate complement sets in case insensitive unicode sets mode.
fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("generated_icu_collections.rs");

    println!("cargo::rerun-if-changed=build.rs");

    let overrides = gen_overrides_map();
    let all_case_folded = gen_all_case_folded_characters();
    let overrides_file = gen_overrides_file(overrides, &all_case_folded);
    gen_all_case_folded_characters();

    fs::write(&dest_path, &overrides_file).unwrap();
}

mod icu_data {
    pub struct BakedDataProvider;
    include!("../../icu/data/mod.rs");
    impl_data_provider!(BakedDataProvider);
}

/// Canonicalize (https://tc39.es/ecma262/#sec-runtime-semantics-canonicalize-ch)
fn canonicalize(case_mapper: &CaseMapper, c: char) -> char {
    let c_upper = case_mapper.simple_uppercase(c);

    if (c_upper as u32) < 128 && (c as u32) >= 128 {
        c
    } else {
        c_upper
    }
}

fn get_case_closure(case_mapper: &CaseMapper, c: char) -> CodePointInversionList {
    let mut closure_builder = CodePointInversionListBuilder::new();

    closure_builder.add_char(c);
    case_mapper.add_case_closure_to(c, &mut closure_builder);

    closure_builder.build()
}

/// Generate a map of code points that need case closure overrides to their correct case closure.
fn gen_overrides_map() -> HashMap<char, HashSet<char>> {
    let case_mapper = CaseMapper::try_new_unstable(&icu_data::BakedDataProvider).unwrap();

    let mut needs_override = HashSet::new();

    // Iterate through all code points
    for i in 0..0x110000 {
        // Skip surrogate code points
        let c = match char::from_u32(i) {
            Some(c) => c,
            None => continue,
        };

        let canonical_c = canonicalize(&case_mapper, c);

        // Find the case closure of the code point
        let closure = get_case_closure(&case_mapper, c);

        // Check if any of the code points in the closure do not have the same canonicalized value
        // as the original code points. If so, this means we need to add an override since the icu4x
        // case closure contains a code point that should not be treated as case equivalent in
        // non-unicode mode.
        for closure_char in closure.iter_chars() {
            let canonical_closure_char = canonicalize(&case_mapper, closure_char);

            if canonical_c != canonical_closure_char {
                needs_override.insert(c);
            }
        }
    }

    let mut overrides = HashMap::new();

    // For each code point that needs an override generate its case closure set according to the
    // ECMAScript canonicalization rules, and store in the overrides map.
    for c in needs_override.into_iter() {
        let mut fixed_closure = HashSet::new();

        let canonical_c = canonicalize(&case_mapper, c);
        let closure = get_case_closure(&case_mapper, c);

        for closure_char in closure.iter_chars() {
            let canonical_closure_char = canonicalize(&case_mapper, closure_char);

            if canonical_c == canonical_closure_char {
                fixed_closure.insert(closure_char);
            }
        }

        overrides.insert(c, fixed_closure);
    }

    overrides
}

/// Generate the set of all code points that map to themselves under case folding.
fn gen_all_case_folded_characters<'a>() -> CodePointInversionList<'a> {
    let case_mapper = CaseMapper::try_new_unstable(&icu_data::BakedDataProvider).unwrap();

    let mut builder = CodePointInversionListBuilder::new();

    for i in 0..0x110000u32 {
        if let Some(c) = char::from_u32(i) {
            if c == case_mapper.simple_fold(c) {
                builder.add_char(c);
            }
        }
    }

    builder.build()
}

fn gen_overrides_file(
    overrides: HashMap<char, HashSet<char>>,
    all_case_folded: &CodePointInversionList,
) -> String {
    let mut file = String::new();

    file.push_str("use icu_collections::codepointinvlist::{CodePointInversionList, CodePointInversionListBuilder};
use std::collections::HashMap;
use std::sync::LazyLock;

fn build_overrides_set() -> CodePointInversionList<'static> {
  let mut builder = CodePointInversionListBuilder::new();
");

    for overriden_char in overrides.keys() {
        file.push_str(&format!("  builder.add_char('{overriden_char}');\n"));
    }

    file.push_str(
        "  builder.build()
}

fn build_overrides_map() -> HashMap<char, CodePointInversionList<'static>> {
  let mut map = HashMap::new();

",
    );

    for (overriden_char, closure) in overrides.iter() {
        file.push_str("  let mut builder = CodePointInversionListBuilder::new();\n");

        for closure_char in closure.iter() {
            file.push_str(&format!("  builder.add_char('{closure_char}');\n"));
        }

        file.push_str(&format!("  map.insert('{overriden_char}', builder.build());\n\n"));
    }

    file.push_str(
        "  map
}

static OVERRIDES_SET: LazyLock<CodePointInversionList<'static>> = LazyLock::new(build_overrides_set);
static OVERRIDES_MAP: LazyLock<HashMap<char, CodePointInversionList<'static>>> = LazyLock::new(build_overrides_map);

pub fn has_case_closure_override(c: char) -> bool {
  OVERRIDES_SET.contains(c)
}

pub fn get_case_closure_override(c: char) -> Option<&'static CodePointInversionList<'static>> {
  OVERRIDES_MAP.get(&c)
}

const ALL_CASE_FOLDED_DATA",
);

    let inv_list_vec = all_case_folded.get_inversion_list_vec();
    file.push_str(&format!(": [u32; {}] = {:?};\n\n", inv_list_vec.len(), inv_list_vec));

    file.push_str(
        "static ALL_CASE_FOLDED_SET: LazyLock<CodePointInversionList<'static>> = LazyLock::new(|| {
    CodePointInversionList::try_from_u32_inversion_list_slice(&ALL_CASE_FOLDED_DATA).unwrap()
});

pub fn all_case_folded_set() -> &'static CodePointInversionList<'static> {
    &ALL_CASE_FOLDED_SET
}
",
    );

    file
}
