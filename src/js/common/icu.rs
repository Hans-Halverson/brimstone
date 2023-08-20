use icu_normalizer::{ComposingNormalizer, DecomposingNormalizer};
use icu_properties::sets::{self, CodePointSetData, CodePointSetDataBorrowed};
use once_cell::sync::Lazy;

include!("../../../icu/data/mod.rs");

pub struct ICU {
    pub properties: Properties,
    pub normalizers: Normalizers,
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
    pub nfc: ComposingNormalizer,
    /// Normalizer for the NFD normalization form.
    pub nfd: DecomposingNormalizer,
    /// Normalizer for the NFKC normalization form.
    pub nfkc: ComposingNormalizer,
    /// Normalizer for the NFKD normalization form.
    pub nfkd: DecomposingNormalizer,
}

pub static ICU: Lazy<ICU> = Lazy::new(|| {
    static ASCII_HEX_DIGIT_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_ascii_hex_digit(&BakedDataProvider).unwrap());
    static ALPHABETIC_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_alphabetic(&BakedDataProvider).unwrap());
    static BIDI_CONTROL_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_bidi_control(&BakedDataProvider).unwrap());
    static BIDI_MIRRORED_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_bidi_mirrored(&BakedDataProvider).unwrap());
    static CASE_IGNORABLE_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_case_ignorable(&BakedDataProvider).unwrap());
    static CASED_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_cased(&BakedDataProvider).unwrap());
    static CHANGES_WHEN_CASEFOLDED_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_changes_when_casefolded(&BakedDataProvider).unwrap());
    static CHANGES_WHEN_CASEMAPPED_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_changes_when_casemapped(&BakedDataProvider).unwrap());
    static CHANGES_WHEN_LOWERCASED_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_changes_when_lowercased(&BakedDataProvider).unwrap());
    static CHANGES_WHEN_NFKC_CASEFOLDED_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_changes_when_nfkc_casefolded(&BakedDataProvider).unwrap());
    static CHANGES_WHEN_TITLECASED_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_changes_when_titlecased(&BakedDataProvider).unwrap());
    static CHANGES_WHEN_UPPERCASED_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_changes_when_uppercased(&BakedDataProvider).unwrap());
    static DASH_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_dash(&BakedDataProvider).unwrap());
    static DEFAULT_IGNORABLE_CODE_POINT_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_default_ignorable_code_point(&BakedDataProvider).unwrap());
    static DEPRECATED_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_deprecated(&BakedDataProvider).unwrap());
    static DIACRITIC_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_diacritic(&BakedDataProvider).unwrap());
    static EMOJI_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_emoji(&BakedDataProvider).unwrap());
    static EMOJI_COMPONENT_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_emoji_component(&BakedDataProvider).unwrap());
    static EMOJI_MODIFIER_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_emoji_modifier(&BakedDataProvider).unwrap());
    static EMOJI_MODIFIER_BASE_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_emoji_modifier_base(&BakedDataProvider).unwrap());
    static EMOJI_PRESENTATION_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_emoji_presentation(&BakedDataProvider).unwrap());
    static EXTENDED_PICTOGRAPHIC_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_extended_pictographic(&BakedDataProvider).unwrap());
    static EXTENDER_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_extender(&BakedDataProvider).unwrap());
    static GRAPHEME_BASE_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_grapheme_base(&BakedDataProvider).unwrap());
    static GRAPHEME_EXTEND_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_grapheme_extend(&BakedDataProvider).unwrap());
    static HEX_DIGIT_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_hex_digit(&BakedDataProvider).unwrap());
    static IDS_BINARY_OPERATOR_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_ids_binary_operator(&BakedDataProvider).unwrap());
    static IDS_TRINARY_OPERATOR_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_ids_trinary_operator(&BakedDataProvider).unwrap());
    static ID_START_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_id_start(&BakedDataProvider).unwrap());
    static ID_CONTINUE_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_id_continue(&BakedDataProvider).unwrap());
    static IDEOGRAPHIC_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_ideographic(&BakedDataProvider).unwrap());
    static JOIN_CONTROL_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_join_control(&BakedDataProvider).unwrap());
    static LOGICAL_ORDER_EXCEPTION_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_logical_order_exception(&BakedDataProvider).unwrap());
    static LOWERCASE_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_lowercase(&BakedDataProvider).unwrap());
    static MATH_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_math(&BakedDataProvider).unwrap());
    static NONCHARACTER_CODE_POINT_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_noncharacter_code_point(&BakedDataProvider).unwrap());
    static PATTERN_SYNTAX_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_pattern_syntax(&BakedDataProvider).unwrap());
    static PATTERN_WHITE_SPACE_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_pattern_white_space(&BakedDataProvider).unwrap());
    static QUOTATION_MARK_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_quotation_mark(&BakedDataProvider).unwrap());
    static RADICAL_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_radical(&BakedDataProvider).unwrap());
    static REGIONAL_INDICATOR_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_regional_indicator(&BakedDataProvider).unwrap());
    static SENTENCE_TERMINAL_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_sentence_terminal(&BakedDataProvider).unwrap());
    static SOFT_DOTTED_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_soft_dotted(&BakedDataProvider).unwrap());
    static TERMINAL_PUNCTUATION_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_terminal_punctuation(&BakedDataProvider).unwrap());
    static UNIFIED_IDEOGRAPH_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_unified_ideograph(&BakedDataProvider).unwrap());
    static UPPERCASE_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_uppercase(&BakedDataProvider).unwrap());
    static VARIATION_SELECTOR_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_variation_selector(&BakedDataProvider).unwrap());
    static WHITE_SPACE_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_white_space(&BakedDataProvider).unwrap());
    static XID_CONTINUE_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_xid_continue(&BakedDataProvider).unwrap());
    static XID_START_SET: Lazy<CodePointSetData> =
        Lazy::new(|| sets::load_xid_start(&BakedDataProvider).unwrap());

    ICU {
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
            nfc: ComposingNormalizer::try_new_nfc_unstable(&BakedDataProvider).unwrap(),
            nfd: DecomposingNormalizer::try_new_nfd_unstable(&BakedDataProvider).unwrap(),
            nfkc: ComposingNormalizer::try_new_nfkc_unstable(&BakedDataProvider).unwrap(),
            nfkd: DecomposingNormalizer::try_new_nfkd_unstable(&BakedDataProvider).unwrap(),
        },
    }
});
