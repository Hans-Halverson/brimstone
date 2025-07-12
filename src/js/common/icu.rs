use std::sync::LazyLock;

use icu_casemap::{CaseMapper, CaseMapperBorrowed};
use icu_collator::{options::CollatorOptions, Collator};
use icu_locale::{locale, Locale};
use icu_normalizer::{
    ComposingNormalizer, ComposingNormalizerBorrowed, DecomposingNormalizer,
    DecomposingNormalizerBorrowed,
};
use icu_properties::{
    props::*,
    script::{ScriptWithExtensions, ScriptWithExtensionsBorrowed},
    CodePointMapData, CodePointMapDataBorrowed, CodePointSetData, CodePointSetDataBorrowed,
    PropertyParser, PropertyParserBorrowed,
};

use super::icu_data::BakedDataProvider;

const DEFAULT_LOCALE: Locale = locale!("en");

#[allow(clippy::upper_case_acronyms)]
pub struct ICU {
    pub general_categories: GeneralCategories,
    pub scripts: Scripts,
    pub properties: Properties,
    pub normalizers: Normalizers,
    pub collator: Collator,
    pub case_mapper: CaseMapperBorrowed<'static>,
}

pub struct GeneralCategories {
    /// Classifier which maps code points to general categories
    pub classifier: CodePointMapDataBorrowed<'static, GeneralCategory>,
}

pub struct Scripts {
    /// Classifier which maps code points to scripts (without extensions)
    pub script_classifier: CodePointMapDataBorrowed<'static, Script>,
    /// Classifier which maps code points to set of scripts with extensions
    pub script_with_extension_classifier: ScriptWithExtensionsBorrowed<'static>,
    /// Mapper which maps script name to script enum
    pub names: PropertyParserBorrowed<'static, Script>,
}

pub struct Properties {
    /// The ASCII_Hex_Digit property
    pub ascii_hex_digit: CodePointSetDataBorrowed<'static>,
    /// The Alphabetic property
    pub alphabetic: CodePointSetDataBorrowed<'static>,
    /// The Bidi_Control property
    pub bidi_control: CodePointSetDataBorrowed<'static>,
    /// The Bidi_Mirrored property
    pub bidi_mirrored: CodePointSetDataBorrowed<'static>,
    /// The Case_Ignorable property
    pub case_ignorable: CodePointSetDataBorrowed<'static>,
    /// The Cased property
    pub cased: CodePointSetDataBorrowed<'static>,
    /// The Changes_When_Casefolded property
    pub changes_when_casefolded: CodePointSetDataBorrowed<'static>,
    /// The Changes_When_Casemapped property
    pub changes_when_casemapped: CodePointSetDataBorrowed<'static>,
    /// The Changes_When_Lowercased property
    pub changes_when_lowercased: CodePointSetDataBorrowed<'static>,
    /// The Changes_When_NFKC_Casefolded property
    pub changes_when_nfkc_casefolded: CodePointSetDataBorrowed<'static>,
    /// The Changes_When_Titlecased property
    pub changes_when_titlecased: CodePointSetDataBorrowed<'static>,
    /// The Changes_When_Uppercased property
    pub changes_when_uppercased: CodePointSetDataBorrowed<'static>,
    /// The Dash property
    pub dash: CodePointSetDataBorrowed<'static>,
    /// The Default_Ignorable_Code_Point property
    pub default_ignorable_code_point: CodePointSetDataBorrowed<'static>,
    /// The Deprecated property
    pub deprecated: CodePointSetDataBorrowed<'static>,
    /// The Diacritic property
    pub diacritic: CodePointSetDataBorrowed<'static>,
    /// The Emoji property
    pub emoji: CodePointSetDataBorrowed<'static>,
    /// The Emoji_Component property
    pub emoji_component: CodePointSetDataBorrowed<'static>,
    /// The Emoji_Modifier property
    pub emoji_modifier: CodePointSetDataBorrowed<'static>,
    /// The Emoji_Modifier_Base property
    pub emoji_modifier_base: CodePointSetDataBorrowed<'static>,
    /// The Emoji_Presentation property
    pub emoji_presentation: CodePointSetDataBorrowed<'static>,
    /// The Extended_Pictographic property
    pub extended_pictographic: CodePointSetDataBorrowed<'static>,
    /// The Extender property
    pub extender: CodePointSetDataBorrowed<'static>,
    /// The Grapheme_Base property
    pub grapheme_base: CodePointSetDataBorrowed<'static>,
    /// The Grapheme_Extend property
    pub grapheme_extend: CodePointSetDataBorrowed<'static>,
    /// The Hex_Digit property
    pub hex_digit: CodePointSetDataBorrowed<'static>,
    /// The IDS_Binary_Operator property
    pub ids_binary_operator: CodePointSetDataBorrowed<'static>,
    /// The IDS_Trinary_Operator property
    pub ids_trinary_operator: CodePointSetDataBorrowed<'static>,
    /// The ID_Start property
    pub id_start: CodePointSetDataBorrowed<'static>,
    /// The ID_Continue property
    pub id_continue: CodePointSetDataBorrowed<'static>,
    /// The Ideographic property
    pub ideographic: CodePointSetDataBorrowed<'static>,
    /// The Join_Control property
    pub join_control: CodePointSetDataBorrowed<'static>,
    /// The Logical_Order_Exception property
    pub logical_order_exception: CodePointSetDataBorrowed<'static>,
    /// The Lowercase property
    pub lowercase: CodePointSetDataBorrowed<'static>,
    /// The Math property
    pub math: CodePointSetDataBorrowed<'static>,
    /// The Noncharacter_Code_Point property
    pub noncharacter_code_point: CodePointSetDataBorrowed<'static>,
    /// The Pattern_Syntax property
    pub pattern_syntax: CodePointSetDataBorrowed<'static>,
    /// The Pattern_White_Space property
    pub pattern_white_space: CodePointSetDataBorrowed<'static>,
    /// The Quotation_Mark property
    pub quotation_mark: CodePointSetDataBorrowed<'static>,
    /// The Radical property
    pub radical: CodePointSetDataBorrowed<'static>,
    /// The Regional_Indicator property
    pub regional_indicator: CodePointSetDataBorrowed<'static>,
    /// The Sentence_Terminal property
    pub sentence_terminal: CodePointSetDataBorrowed<'static>,
    /// The Soft_Dotted property
    pub soft_dotted: CodePointSetDataBorrowed<'static>,
    /// The Terminal_Punctuation property
    pub terminal_punctuation: CodePointSetDataBorrowed<'static>,
    /// The Unified_Ideograph property
    pub unified_ideograph: CodePointSetDataBorrowed<'static>,
    /// The Uppercase property
    pub uppercase: CodePointSetDataBorrowed<'static>,
    /// The Variation_Selector property
    pub variation_selector: CodePointSetDataBorrowed<'static>,
    /// The White_Space property
    pub white_space: CodePointSetDataBorrowed<'static>,
    /// The XID_Continue property
    pub xid_continue: CodePointSetDataBorrowed<'static>,
    /// The XID_Start property
    pub xid_start: CodePointSetDataBorrowed<'static>,
}

pub struct Normalizers {
    /// Normalizer for the NFC normalization form.
    pub nfc: ComposingNormalizerBorrowed<'static>,
    /// Normalizer for the NFD normalization form.
    pub nfd: DecomposingNormalizerBorrowed<'static>,
    /// Normalizer for the NFKC normalization form.
    pub nfkc: ComposingNormalizerBorrowed<'static>,
    /// Normalizer for the NFKD normalization form.
    pub nfkd: DecomposingNormalizerBorrowed<'static>,
}

pub static ICU: LazyLock<ICU> = LazyLock::new(|| {
    // General categories
    static GENERAL_CATEGORIES_MAP: LazyLock<CodePointMapData<GeneralCategory>> =
        LazyLock::new(|| {
            CodePointMapData::<GeneralCategory>::try_new_unstable(&BakedDataProvider).unwrap()
        });

    // Binary property sets
    macro_rules! binary_property_static {
        ($static_name:ident, $property:ident) => {
            static $static_name: LazyLock<CodePointSetData> = LazyLock::new(|| {
                CodePointSetData::try_new_unstable::<$property>(&BakedDataProvider).unwrap()
            });
        };
    }

    binary_property_static!(ASCII_HEX_DIGIT_SET, AsciiHexDigit);
    binary_property_static!(ALPHABETIC_SET, Alphabetic);
    binary_property_static!(BIDI_CONTROL_SET, BidiControl);
    binary_property_static!(BIDI_MIRRORED_SET, BidiMirrored);
    binary_property_static!(CASE_IGNORABLE_SET, CaseIgnorable);
    binary_property_static!(CASED_SET, Cased);
    binary_property_static!(CHANGES_WHEN_CASEFOLDED_SET, ChangesWhenCasefolded);
    binary_property_static!(CHANGES_WHEN_CASEMAPPED_SET, ChangesWhenCasemapped);
    binary_property_static!(CHANGES_WHEN_LOWERCASED_SET, ChangesWhenLowercased);
    binary_property_static!(CHANGES_WHEN_NFKC_CASEFOLDED_SET, ChangesWhenNfkcCasefolded);
    binary_property_static!(CHANGES_WHEN_TITLECASED_SET, ChangesWhenTitlecased);
    binary_property_static!(CHANGES_WHEN_UPPERCASED_SET, ChangesWhenUppercased);
    binary_property_static!(DASH_SET, Dash);
    binary_property_static!(DEFAULT_IGNORABLE_CODE_POINT_SET, DefaultIgnorableCodePoint);
    binary_property_static!(DEPRECATED_SET, Deprecated);
    binary_property_static!(DIACRITIC_SET, Diacritic);
    binary_property_static!(EMOJI_SET, Emoji);
    binary_property_static!(EMOJI_COMPONENT_SET, EmojiComponent);
    binary_property_static!(EMOJI_MODIFIER_SET, EmojiModifier);
    binary_property_static!(EMOJI_MODIFIER_BASE_SET, EmojiModifierBase);
    binary_property_static!(EMOJI_PRESENTATION_SET, EmojiPresentation);
    binary_property_static!(EXTENDED_PICTOGRAPHIC_SET, ExtendedPictographic);
    binary_property_static!(EXTENDER_SET, Extender);
    binary_property_static!(GRAPHEME_BASE_SET, GraphemeBase);
    binary_property_static!(GRAPHEME_EXTEND_SET, GraphemeExtend);
    binary_property_static!(HEX_DIGIT_SET, HexDigit);
    binary_property_static!(IDS_BINARY_OPERATOR_SET, IdsBinaryOperator);
    binary_property_static!(IDS_TRINARY_OPERATOR_SET, IdsTrinaryOperator);
    binary_property_static!(ID_START_SET, IdStart);
    binary_property_static!(ID_CONTINUE_SET, IdContinue);
    binary_property_static!(IDEOGRAPHIC_SET, Ideographic);
    binary_property_static!(JOIN_CONTROL_SET, JoinControl);
    binary_property_static!(LOGICAL_ORDER_EXCEPTION_SET, LogicalOrderException);
    binary_property_static!(LOWERCASE_SET, Lowercase);
    binary_property_static!(MATH_SET, Math);
    binary_property_static!(NONCHARACTER_CODE_POINT_SET, NoncharacterCodePoint);
    binary_property_static!(PATTERN_SYNTAX_SET, PatternSyntax);
    binary_property_static!(PATTERN_WHITE_SPACE_SET, PatternWhiteSpace);
    binary_property_static!(QUOTATION_MARK_SET, QuotationMark);
    binary_property_static!(RADICAL_SET, Radical);
    binary_property_static!(REGIONAL_INDICATOR_SET, RegionalIndicator);
    binary_property_static!(SENTENCE_TERMINAL_SET, SentenceTerminal);
    binary_property_static!(SOFT_DOTTED_SET, SoftDotted);
    binary_property_static!(TERMINAL_PUNCTUATION_SET, TerminalPunctuation);
    binary_property_static!(UNIFIED_IDEOGRAPH_SET, UnifiedIdeograph);
    binary_property_static!(UPPERCASE_SET, Uppercase);
    binary_property_static!(VARIATION_SELECTOR_SET, VariationSelector);
    binary_property_static!(WHITE_SPACE_SET, WhiteSpace);
    binary_property_static!(XID_CONTINUE_SET, XidContinue);
    binary_property_static!(XID_START_SET, XidStart);

    // Scripts
    static SCRIPT_MAP: LazyLock<CodePointMapData<Script>> =
        LazyLock::new(|| CodePointMapData::<Script>::try_new_unstable(&BakedDataProvider).unwrap());
    static SCRIPT_WITH_EXTENSIONS_CLASSIFIER: LazyLock<ScriptWithExtensions> =
        LazyLock::new(|| ScriptWithExtensions::try_new_unstable(&BakedDataProvider).unwrap());
    static SCRIPT_NAMES: LazyLock<PropertyParser<Script>> =
        LazyLock::new(|| PropertyParser::try_new_unstable(&BakedDataProvider).unwrap());

    // Normalizers
    static NFC: LazyLock<ComposingNormalizer> =
        LazyLock::new(|| ComposingNormalizer::try_new_nfc_unstable(&BakedDataProvider).unwrap());
    static NFD: LazyLock<DecomposingNormalizer> =
        LazyLock::new(|| DecomposingNormalizer::try_new_nfd_unstable(&BakedDataProvider).unwrap());
    static NFKC: LazyLock<ComposingNormalizer> =
        LazyLock::new(|| ComposingNormalizer::try_new_nfkc_unstable(&BakedDataProvider).unwrap());
    static NFKD: LazyLock<DecomposingNormalizer> =
        LazyLock::new(|| DecomposingNormalizer::try_new_nfkd_unstable(&BakedDataProvider).unwrap());

    // Case mapper
    static CASE_MAPPER: LazyLock<CaseMapper> =
        LazyLock::new(|| CaseMapper::try_new_unstable(&BakedDataProvider).unwrap());

    ICU {
        general_categories: GeneralCategories { classifier: GENERAL_CATEGORIES_MAP.as_borrowed() },
        scripts: Scripts {
            script_classifier: SCRIPT_MAP.as_borrowed(),
            script_with_extension_classifier: SCRIPT_WITH_EXTENSIONS_CLASSIFIER.as_borrowed(),
            names: SCRIPT_NAMES.as_borrowed(),
        },
        properties: Properties {
            ascii_hex_digit: ASCII_HEX_DIGIT_SET.as_borrowed(),
            alphabetic: ALPHABETIC_SET.as_borrowed(),
            bidi_control: BIDI_CONTROL_SET.as_borrowed(),
            bidi_mirrored: BIDI_MIRRORED_SET.as_borrowed(),
            case_ignorable: CASE_IGNORABLE_SET.as_borrowed(),
            cased: CASED_SET.as_borrowed(),
            changes_when_casefolded: CHANGES_WHEN_CASEFOLDED_SET.as_borrowed(),
            changes_when_casemapped: CHANGES_WHEN_CASEMAPPED_SET.as_borrowed(),
            changes_when_lowercased: CHANGES_WHEN_LOWERCASED_SET.as_borrowed(),
            changes_when_nfkc_casefolded: CHANGES_WHEN_NFKC_CASEFOLDED_SET.as_borrowed(),
            changes_when_titlecased: CHANGES_WHEN_TITLECASED_SET.as_borrowed(),
            changes_when_uppercased: CHANGES_WHEN_UPPERCASED_SET.as_borrowed(),
            dash: DASH_SET.as_borrowed(),
            default_ignorable_code_point: DEFAULT_IGNORABLE_CODE_POINT_SET.as_borrowed(),
            deprecated: DEPRECATED_SET.as_borrowed(),
            diacritic: DIACRITIC_SET.as_borrowed(),
            emoji: EMOJI_SET.as_borrowed(),
            emoji_component: EMOJI_COMPONENT_SET.as_borrowed(),
            emoji_modifier: EMOJI_MODIFIER_SET.as_borrowed(),
            emoji_modifier_base: EMOJI_MODIFIER_BASE_SET.as_borrowed(),
            emoji_presentation: EMOJI_PRESENTATION_SET.as_borrowed(),
            extended_pictographic: EXTENDED_PICTOGRAPHIC_SET.as_borrowed(),
            extender: EXTENDER_SET.as_borrowed(),
            grapheme_base: GRAPHEME_BASE_SET.as_borrowed(),
            grapheme_extend: GRAPHEME_EXTEND_SET.as_borrowed(),
            hex_digit: HEX_DIGIT_SET.as_borrowed(),
            ids_binary_operator: IDS_BINARY_OPERATOR_SET.as_borrowed(),
            ids_trinary_operator: IDS_TRINARY_OPERATOR_SET.as_borrowed(),
            id_start: ID_START_SET.as_borrowed(),
            id_continue: ID_CONTINUE_SET.as_borrowed(),
            ideographic: IDEOGRAPHIC_SET.as_borrowed(),
            join_control: JOIN_CONTROL_SET.as_borrowed(),
            logical_order_exception: LOGICAL_ORDER_EXCEPTION_SET.as_borrowed(),
            lowercase: LOWERCASE_SET.as_borrowed(),
            math: MATH_SET.as_borrowed(),
            noncharacter_code_point: NONCHARACTER_CODE_POINT_SET.as_borrowed(),
            pattern_syntax: PATTERN_SYNTAX_SET.as_borrowed(),
            pattern_white_space: PATTERN_WHITE_SPACE_SET.as_borrowed(),
            quotation_mark: QUOTATION_MARK_SET.as_borrowed(),
            radical: RADICAL_SET.as_borrowed(),
            regional_indicator: REGIONAL_INDICATOR_SET.as_borrowed(),
            sentence_terminal: SENTENCE_TERMINAL_SET.as_borrowed(),
            soft_dotted: SOFT_DOTTED_SET.as_borrowed(),
            terminal_punctuation: TERMINAL_PUNCTUATION_SET.as_borrowed(),
            unified_ideograph: UNIFIED_IDEOGRAPH_SET.as_borrowed(),
            uppercase: UPPERCASE_SET.as_borrowed(),
            variation_selector: VARIATION_SELECTOR_SET.as_borrowed(),
            white_space: WHITE_SPACE_SET.as_borrowed(),
            xid_continue: XID_CONTINUE_SET.as_borrowed(),
            xid_start: XID_START_SET.as_borrowed(),
        },
        normalizers: Normalizers {
            nfc: NFC.as_borrowed(),
            nfd: NFD.as_borrowed(),
            nfkc: NFKC.as_borrowed(),
            nfkd: NFKD.as_borrowed(),
        },
        collator: Collator::try_new_unstable(
            &BakedDataProvider,
            DEFAULT_LOCALE.into(),
            CollatorOptions::default(),
        )
        .unwrap(),
        case_mapper: CASE_MAPPER.as_borrowed(),
    }
});
