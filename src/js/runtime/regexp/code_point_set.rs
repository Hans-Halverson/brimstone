use std::{collections::HashSet, sync::LazyLock};

use brimstone_icu_collections::{
    all_case_folded_set, get_case_closure_override, has_case_closure_override,
};
use icu_collections::codepointinvlist::{CodePointInversionList, CodePointInversionListBuilder};

use crate::{
    common::{
        icu::ICU,
        unicode::{CodePoint, MAX_CODE_POINT},
        unicode_property::UnicodeProperty,
        wtf_8::{Wtf8Cow, Wtf8Str, Wtf8String},
    },
    parser::regexp::{CharacterClass, ClassExpressionType, ClassRange, RegExpFlags},
};

/// Utility for determining the set of code points for literals and character classes for a given
/// set of flags (i.e. aware of case sensitivity and unicode modes).
pub struct CodePointSetBuilder {
    flags: RegExpFlags,
}

impl CodePointSetBuilder {
    fn new(flags: RegExpFlags) -> Self {
        Self { flags }
    }

    fn is_case_insensitive_unicode_sets(&self) -> bool {
        self.flags.has_unicode_sets_flag() && self.flags.is_case_insensitive()
    }

    /// Return the set of code points that match a given code point, including the case closure if
    /// in case insensitive mode.
    pub fn code_point_to_set(
        code_point: u32,
        flags: RegExpFlags,
    ) -> CodePointInversionList<'static> {
        let builder = Self::new(flags);

        let mut set = CodePointInversionListBuilder::new();

        if builder.flags.is_case_insensitive()
            && let Some(char) = char::from_u32(code_point)
        {
            builder.add_case_closure(&mut set, char);
        } else {
            set.add32(code_point);
        }

        set.build()
    }

    /// Return the set of code points and strings matched by a character class. Handles case
    /// sensitivity and unicode modes. Only applies inversion if in unicode sets mode.
    pub fn character_class_to_set<'a, 'b>(
        character_class: &'b CharacterClass,
        flags: RegExpFlags,
    ) -> (CodePointInversionList<'a>, HashSet<Wtf8Cow<'b>>) {
        let builder = Self::new(flags);
        let (set, strings_set_builder) = builder.character_class_to_set_impl(character_class);

        let mut set_builder = CodePointInversionListBuilder::new();
        set_builder.add_set(&set);

        // Create case closure of set if in case insensitive mode
        if builder.flags.is_case_insensitive() {
            let old_set = std::mem::take(&mut set_builder).build();
            for code_point in iter_code_point_inversion_list(&old_set) {
                if let Some(char) = char::from_u32(code_point) {
                    builder.add_case_closure(&mut set_builder, char);
                } else {
                    // Keep unpaired surrogates in the set
                    set_builder.add32(code_point);
                }
            }
        }

        (set_builder.build(), strings_set_builder)
    }

    /// Return the set of code points and strings matched by a character class.
    ///
    /// Only applies case folding and/or inversion if in unicode sets mode.
    fn character_class_to_set_impl<'a>(
        &self,
        character_class: &'a CharacterClass,
    ) -> (CodePointInversionList<'static>, HashSet<Wtf8Cow<'a>>) {
        let mut set_builder = CodePointInversionListBuilder::new();
        let mut strings_set_builder = HashSet::new();

        match character_class.expression_type {
            ClassExpressionType::Union => {
                // Add code points and strings that are in any operand
                for class_range in character_class.operands.iter() {
                    self.add_character_class_range_to_set(
                        class_range,
                        &mut set_builder,
                        &mut strings_set_builder,
                    );
                }

                self.maybe_simple_case_folding(&mut set_builder);
            }
            ClassExpressionType::Intersection => {
                // Initialize sets with the first operand
                let (first_set, first_strings) =
                    self.character_class_range_to_set(&character_class.operands[0]);
                set_builder.add_set(&first_set);
                strings_set_builder = first_strings;

                // Only retain code points and strings that are in all operands
                for class_range in &character_class.operands[1..] {
                    let (other_set, other_strings) = self.character_class_range_to_set(class_range);
                    set_builder.retain_set(&other_set);
                    strings_set_builder.retain(|string| other_strings.contains(string));
                }
            }
            ClassExpressionType::Difference => {
                // Initialize sets with the first operand
                let (first_set, first_strings) =
                    self.character_class_range_to_set(&character_class.operands[0]);
                set_builder.add_set(&first_set);
                strings_set_builder = first_strings;

                // Remove code points and strings that are in later operands
                for class_range in &character_class.operands[1..] {
                    let (other_set, other_strings) = self.character_class_range_to_set(class_range);

                    set_builder.remove_set(&other_set);

                    for string in other_strings {
                        strings_set_builder.remove(&string);
                    }
                }
            }
        }

        // Eagerly invert the set if in unicode sets mode
        if character_class.is_inverted && self.flags.has_unicode_sets_flag() {
            if self.flags.is_case_insensitive() {
                let old_set = std::mem::take(&mut set_builder).build();
                let complement_set = case_insensitive_unicode_sets_complement(&old_set);
                set_builder.add_set(&complement_set);
            } else {
                set_builder.complement();
            }
        }

        (set_builder.build(), strings_set_builder)
    }

    fn character_class_range_to_set<'a>(
        &self,
        class_range: &'a ClassRange,
    ) -> (CodePointInversionList<'static>, HashSet<Wtf8Cow<'a>>) {
        let mut set_builder = CodePointInversionListBuilder::new();
        let mut strings = HashSet::new();

        self.add_character_class_range_to_set(class_range, &mut set_builder, &mut strings);
        self.maybe_simple_case_folding(&mut set_builder);

        (set_builder.build(), strings)
    }

    fn add_character_class_range_to_set<'a>(
        &self,
        class_range: &'a ClassRange,
        set_builder: &mut CodePointInversionListBuilder,
        strings_set_builder: &mut HashSet<Wtf8Cow<'a>>,
    ) {
        match class_range {
            // Accumulate single and range char ranges
            ClassRange::Single(code_point) => {
                set_builder.add32(*code_point);
            }
            ClassRange::Range(start, end) => {
                // Otherwise can add the range directly
                set_builder.add_range32(*start..=*end);
            }
            // Use the precomputed word set. This is valid in case insensitive `u` mode because
            // the case closure will be created by the caller. This is valid in case sensitive `v`
            // mode because MaybeSimpleCaseFolding will be applied by the caller.
            ClassRange::Word => set_builder.add_set(&WORD_SET),
            // Use the precomputed not word set if possible. In case insensitive `v` mode we must
            // construct the complement ourselves.
            ClassRange::NotWord => {
                if self.flags.is_case_insensitive() && self.flags.has_any_unicode_flag() {
                    if self.flags.has_unicode_sets_flag() {
                        set_builder.add_set(&NOT_WORD_CASE_INSENSITIVE_V_MODE_SET);
                    } else {
                        set_builder.add_set(&NOT_WORD_CASE_INSENSITIVE_U_MODE_SET);
                    }
                } else {
                    set_builder.add_set(&NOT_WORD_SET);
                }
            }
            // Use the precomputed whitespace set
            ClassRange::Whitespace => set_builder.add_set(&WHITESPACE_SET),
            // Use the precomputed not whitespace set if possible. In case insensitive `v` mode we
            // must construct the complement ourselves.
            ClassRange::NotWhitespace => {
                if self.is_case_insensitive_unicode_sets() {
                    set_builder.add_set(&NOT_WHITESPACE_CASE_INSENSITIVE_V_MODE_SET);
                } else {
                    set_builder.add_set(&NOT_WHITESPACE_SET)
                }
            }
            // Decimal ranges are simple so they are hardcoded
            ClassRange::Digit => {
                set_builder.add_range('0'..='9');
            }
            // Use the hardcoded simple decimal ranges when possible. In case insensitive `v` mode
            // we must construct the complement ourselves.
            ClassRange::NotDigit => {
                if self.is_case_insensitive_unicode_sets() {
                    set_builder.add_set(&NOT_DIGIT_CASE_INSENSITIVE_V_MODE_SET);
                } else {
                    set_builder.add_range32(0..('0' as u32));
                    set_builder.add_range32(('9' as u32 + 1)..=MAX_CODE_POINT);
                }
            }
            ClassRange::UnicodeProperty(property) => {
                // MaybeSimpleCaseFolding will be applied by the caller
                property.add_to_set(set_builder);

                // Add strings to set if this is a property of strings
                if let UnicodeProperty::BinaryPropertyOfStrings(property) = property {
                    for string in property.iter_strings() {
                        self.add_string_to_set(strings_set_builder, Wtf8Str::from_str(string));
                    }
                }
            }
            // Construct the complement of the unicode property set
            ClassRange::NotUnicodeProperty(property) => {
                let property_complement = if self.is_case_insensitive_unicode_sets() {
                    // In case insensitive unicode sets mode we must perform case folding before
                    // taking the complement.
                    let mut property_set = CodePointInversionListBuilder::new();
                    property.add_to_set(&mut property_set);
                    self.maybe_simple_case_folding(&mut property_set);
                    case_insensitive_unicode_sets_complement(&property_set.build())
                } else {
                    // Otherwise create the complement set directly
                    let mut property_complement = CodePointInversionListBuilder::new();
                    property.add_to_set(&mut property_complement);
                    property_complement.complement();
                    property_complement.build()
                };

                // Then add the complement set to the set builder
                set_builder.add_set(&property_complement);
            }
            ClassRange::NestedClass(nested_class) => {
                let (nested_set, nested_strings) = self.character_class_to_set_impl(nested_class);
                set_builder.add_set(&nested_set);
                strings_set_builder.extend(nested_strings);
            }
            ClassRange::StringDisjunction(disjunction) => {
                for &string in disjunction.alternatives.iter() {
                    // Check if the string has exactly one code point (only need to check at most
                    // the first two code points to be sure).
                    if string.iter_code_points().take(2).count() == 1 {
                        // Treat as a regular code point instead of a string
                        set_builder.add32(string.iter_code_points().next().unwrap());
                    } else {
                        // Treat as a string
                        self.add_string_to_set(strings_set_builder, string);
                    }
                }
            }
        }
    }

    fn add_string_to_set<'a>(&self, set: &mut HashSet<Wtf8Cow<'a>>, str: &'a Wtf8Str) {
        let string = if self.flags.is_case_insensitive() {
            // Immediately case fold strings when encountered, allowing set operations to treat
            // case equivalent strings as the same string.
            //
            // The case closure eventually emitted is the same for the code point and its simple
            // case folded form.
            let folded_string = simple_case_fold_string(str);
            Wtf8Cow::Owned(folded_string)
        } else {
            Wtf8Cow::Borrowed(str)
        };

        set.insert(string);
    }

    /// Add the spec-compliant case closure set for the given code point to the set builder.
    fn add_case_closure(&self, set_builder: &mut CodePointInversionListBuilder, code_point: char) {
        // Case closure sets do not contain the code point itself
        set_builder.add_char(code_point);

        // We use `add_case_closure_to` from icu4x whenever possible.
        //
        // Unicode aware RegExp canonicalization uses standard Unicode simple case mapping, so
        // `add_case_closure_to` is sufficient.
        //
        // However unicode unaware RegExp canonicalization uses a slightly different procedure,
        // mapping code points using simple uppercase mapping, but not mapping code points outside
        // the Latin1 range to within the Latin1 range. This has almost the same behavior as
        // `add_case_closure_to`, so we have precomputed the code points for which the behavior
        // differs. We use the precomupted override if one exists, otherwise we use
        // `add_case_closure_to`.
        if self.flags.has_any_unicode_flag() || !has_case_closure_override(code_point) {
            ICU.case_mapper.add_case_closure_to(code_point, set_builder);
        } else {
            let case_closure_override = get_case_closure_override(code_point).unwrap();
            set_builder.add_set(case_closure_override);
        }
    }

    /// Replace every element of a set with its MaybeSimpleCaseFolding equivalent, if in case
    /// insensitive unicode sets mode.
    ///
    /// MaybeSimpleCaseFolding (https://tc39.es/ecma262/#sec-maybesimplecasefolding)
    fn maybe_simple_case_folding(&self, set_builder: &mut CodePointInversionListBuilder) {
        if !self.is_case_insensitive_unicode_sets() {
            return;
        }

        // Set is fully replaced with case folded equivalent
        let mut unfolded_set = CodePointInversionListBuilder::new();
        std::mem::swap(set_builder, &mut unfolded_set);

        for code_point in iter_code_point_inversion_list(&unfolded_set.build()) {
            set_builder.add32(simple_case_fold_code_point(code_point));
        }
    }
}

/// Empty set of code points.
pub static EMPTY_SET: LazyLock<CodePointInversionList> =
    LazyLock::new(|| CodePointInversionListBuilder::new().build());

/// Set of word characters to be used for word character classes and word boundary assertions when
/// in case sensitive or unicode unaware mode.
pub static WORD_SET: LazyLock<CodePointInversionList> =
    LazyLock::new(|| create_word_set_builder().build());

/// Set of word characters to be used for word character classes and word boundary assertions when
/// in case insensitive, unicode aware mode.
pub static WORD_CASE_INSENSITIVE_UNICODE_SET: LazyLock<CodePointInversionList> =
    LazyLock::new(|| {
        let mut set_builder = create_word_set_builder();

        // Add extra code points to form the case insensitive closure of the word set
        set_builder.add_char('\u{017f}');
        set_builder.add_char('\u{212a}');

        set_builder.build()
    });

/// Set of non-word characters to be used for non-word character classes when in case sensitive or
/// unicode unaware mode.
static NOT_WORD_SET: LazyLock<CodePointInversionList> = LazyLock::new(|| {
    let mut set_builder = create_word_set_builder();
    set_builder.complement();
    set_builder.build()
});

/// Set of non-word characters to be used for non-word character classes when in case insensitive,
/// unicode `u` mode.
static NOT_WORD_CASE_INSENSITIVE_U_MODE_SET: LazyLock<CodePointInversionList> =
    LazyLock::new(|| {
        let mut set_builder = CodePointInversionListBuilder::new();
        set_builder.add_set(&WORD_CASE_INSENSITIVE_UNICODE_SET);
        set_builder.complement();
        set_builder.build()
    });

/// Set of non-word characters to be used for non-word character classes when in case insensitive,
/// unicode sets `v` mode.
static NOT_WORD_CASE_INSENSITIVE_V_MODE_SET: LazyLock<CodePointInversionList> =
    LazyLock::new(|| {
        let mut set_builder = CodePointInversionListBuilder::new();
        set_builder.add_set(&WORD_CASE_INSENSITIVE_UNICODE_SET);
        case_insensitive_unicode_sets_complement(&set_builder.build())
    });

/// Set of whitespace characters to be used for whitespace character classes.
static WHITESPACE_SET: LazyLock<CodePointInversionList> =
    LazyLock::new(|| create_whitespace_set_builder().build());

/// Set of non-whitespace characters to be used for non-whitespace character classes.
static NOT_WHITESPACE_SET: LazyLock<CodePointInversionList> = LazyLock::new(|| {
    let mut set_builder = create_whitespace_set_builder();
    set_builder.complement();
    set_builder.build()
});

/// Set of non-whitespace characters in case insensitive unicode sets `v` mode.
static NOT_WHITESPACE_CASE_INSENSITIVE_V_MODE_SET: LazyLock<CodePointInversionList> =
    LazyLock::new(|| {
        let set_builder = create_whitespace_set_builder();
        case_insensitive_unicode_sets_complement(&set_builder.build())
    });

/// Set of non-digit characters in case insensitive unicode sets `v` mode.
static NOT_DIGIT_CASE_INSENSITIVE_V_MODE_SET: LazyLock<CodePointInversionList> =
    LazyLock::new(|| {
        let mut set_builder = CodePointInversionListBuilder::new();
        set_builder.add_range('0'..='9');

        case_insensitive_unicode_sets_complement(&set_builder.build())
    });

fn create_word_set_builder() -> CodePointInversionListBuilder {
    let mut set_builder = CodePointInversionListBuilder::new();

    set_builder.add_range('a'..='z');
    set_builder.add_range('A'..='Z');
    set_builder.add_range('0'..='9');
    set_builder.add_char('_');

    set_builder
}

fn create_whitespace_set_builder() -> CodePointInversionListBuilder {
    // All code points on the right hand side of WhiteSpace or LineTerminator productions in the
    // spec.
    let mut set_builder = CodePointInversionListBuilder::new();

    set_builder.add_range('\u{0009}'..='\u{000D}');
    set_builder.add_char('\u{0020}');
    set_builder.add_char('\u{00A0}');
    set_builder.add_char('\u{1680}');
    set_builder.add_range('\u{2000}'..='\u{200A}');
    set_builder.add_range('\u{2028}'..='\u{2029}');
    set_builder.add_char('\u{202F}');
    set_builder.add_char('\u{205F}');
    set_builder.add_char('\u{3000}');
    set_builder.add_char('\u{FEFF}');

    set_builder
}

/// Iterate over the code points in a `CodePointInversionList`
///
/// We cannot use `CodePointInversionList::iter_chars` as this filters out unpaired surrogates.
fn iter_code_point_inversion_list(set: &CodePointInversionList) -> impl Iterator<Item = u32> {
    set.iter_ranges().flatten()
}

fn simple_case_fold_code_point(code_point: CodePoint) -> CodePoint {
    if let Some(char) = char::from_u32(code_point) {
        ICU.case_mapper.simple_fold(char) as CodePoint
    } else {
        // Keep unpaired surrogates in the set
        code_point
    }
}

fn simple_case_fold_string(str: &Wtf8Str) -> Wtf8String {
    let mut case_folded_string = Wtf8String::new();
    for code_point in str.iter_code_points() {
        case_folded_string.push(simple_case_fold_code_point(code_point));
    }

    case_folded_string
}

/// Create the complement of a set of code points when in case insensitive unicode sets mode.
///
/// Returns the difference between the set of all canonical case folded code points and the target.
fn case_insensitive_unicode_sets_complement(
    set: &CodePointInversionList,
) -> CodePointInversionList<'static> {
    let mut set_builder = CodePointInversionListBuilder::new();
    set_builder.add_set(all_case_folded_set());
    set_builder.remove_set(set);
    set_builder.build()
}
