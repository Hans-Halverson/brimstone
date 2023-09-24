use icu_casemapping::CaseMapping;
use icu_collator::{Collator, CollatorOptions};
use icu_locid::{locale, Locale};
use icu_normalizer::{ComposingNormalizer, DecomposingNormalizer};
use icu_properties::{
    names::{PropertyValueNameToEnumMapper, PropertyValueNameToEnumMapperBorrowed},
    script::ScriptWithExtensionsBorrowed,
    script::{self, ScriptWithExtensions},
    sets::{self, CodePointSetData, CodePointSetDataBorrowed},
    GeneralCategoryGroup, Script,
};
use icu_provider_adapters::fallback::LocaleFallbackProvider;
use once_cell::sync::Lazy;

include!("../../../icu/data/mod.rs");

const DEFAULT_LOCALE: Locale = locale!("en");

pub struct ICU {
    pub general_categories: GeneralCategories,
    pub scripts: Scripts,
    pub properties: Properties,
    pub normalizers: Normalizers,
    pub collator: Collator,
    pub case_mapping: CaseMapping,
}

pub struct GeneralCategories {
    /// The C general category
    pub other: CodePointSetDataBorrowed<'static>,
    /// The Cc general category
    pub control: CodePointSetDataBorrowed<'static>,
    /// The Cf general category
    pub format: CodePointSetDataBorrowed<'static>,
    /// The Cn general category
    pub unassigned: CodePointSetDataBorrowed<'static>,
    /// The Co general category
    pub private_use: CodePointSetDataBorrowed<'static>,
    /// The Cs general category
    pub surrogate: CodePointSetDataBorrowed<'static>,
    /// The L general category
    pub letter: CodePointSetDataBorrowed<'static>,
    /// The LC general category
    pub cased_letter: CodePointSetDataBorrowed<'static>,
    /// The Ll general category
    pub lowercase_letter: CodePointSetDataBorrowed<'static>,
    /// The Lm general category
    pub modifier_letter: CodePointSetDataBorrowed<'static>,
    /// The Lo general category
    pub other_letter: CodePointSetDataBorrowed<'static>,
    /// The Lt general category
    pub titlecase_letter: CodePointSetDataBorrowed<'static>,
    /// The Lu general category
    pub uppercase_letter: CodePointSetDataBorrowed<'static>,
    /// The M general category
    pub mark: CodePointSetDataBorrowed<'static>,
    /// The Mc general category
    pub spacing_mark: CodePointSetDataBorrowed<'static>,
    /// The Me general category
    pub enclosing_mark: CodePointSetDataBorrowed<'static>,
    /// The Mn general category
    pub nonspacing_mark: CodePointSetDataBorrowed<'static>,
    /// The N general category
    pub number: CodePointSetDataBorrowed<'static>,
    /// The Nd general category
    pub decimal_number: CodePointSetDataBorrowed<'static>,
    /// The Nl general category
    pub letter_number: CodePointSetDataBorrowed<'static>,
    /// The No general category
    pub other_number: CodePointSetDataBorrowed<'static>,
    /// The P general category
    pub punctuation: CodePointSetDataBorrowed<'static>,
    /// The Pc general category
    pub connector_punctuation: CodePointSetDataBorrowed<'static>,
    /// The Pd general category
    pub dash_punctuation: CodePointSetDataBorrowed<'static>,
    /// The Pe general category
    pub close_punctuation: CodePointSetDataBorrowed<'static>,
    /// The Pf general category
    pub final_punctuation: CodePointSetDataBorrowed<'static>,
    /// The Pi general category
    pub initial_punctuation: CodePointSetDataBorrowed<'static>,
    /// The Po general category
    pub other_punctuation: CodePointSetDataBorrowed<'static>,
    /// The Ps general category
    pub open_punctuation: CodePointSetDataBorrowed<'static>,
    /// The S general category
    pub symbol: CodePointSetDataBorrowed<'static>,
    /// The Sc general category
    pub currency_symbol: CodePointSetDataBorrowed<'static>,
    /// The Sk general category
    pub modifier_symbol: CodePointSetDataBorrowed<'static>,
    /// The Sm general category
    pub math_symbol: CodePointSetDataBorrowed<'static>,
    /// The So general category
    pub other_symbol: CodePointSetDataBorrowed<'static>,
    /// The Z general category
    pub separator: CodePointSetDataBorrowed<'static>,
    /// The Zl general category
    pub line_separator: CodePointSetDataBorrowed<'static>,
    /// The Zp general category
    pub paragraph_separator: CodePointSetDataBorrowed<'static>,
    /// The Zs general category
    pub space_separator: CodePointSetDataBorrowed<'static>,
}

pub struct Scripts {
    /// Classifier which maps code points to scripts or sets of scripts
    pub classifier: ScriptWithExtensionsBorrowed<'static>,
    /// Mapper which maps script name to script enum
    pub names: PropertyValueNameToEnumMapperBorrowed<'static, Script>,
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
    // General category sets
    macro_rules! general_category_static {
        ($static_name:ident, $category_name:ident) => {
            static $static_name: Lazy<CodePointSetData> = Lazy::new(|| {
                sets::load_for_general_category_group(
                    &BakedDataProvider,
                    GeneralCategoryGroup::$category_name,
                )
                .unwrap()
            });
        };
    }

    general_category_static!(OTHER_SET, Other);
    general_category_static!(CONTROL_SET, Control);
    general_category_static!(FORMAT_SET, Format);
    general_category_static!(UNASSIGNED_SET, Unassigned);
    general_category_static!(PRIVATE_USE_SET, PrivateUse);
    general_category_static!(SURROGATE_SET, Surrogate);
    general_category_static!(LETTER_SET, Letter);
    general_category_static!(CASED_LETTER_SET, CasedLetter);
    general_category_static!(LOWERCASE_LETTER_SET, LowercaseLetter);
    general_category_static!(MODIFIER_LETTER_SET, ModifierLetter);
    general_category_static!(OTHER_LETTER_SET, OtherLetter);
    general_category_static!(TITLECASE_LETTER_SET, TitlecaseLetter);
    general_category_static!(UPPERCASE_LETTER_SET, UppercaseLetter);
    general_category_static!(MARK_SET, Mark);
    general_category_static!(SPACING_MARK_SET, SpacingMark);
    general_category_static!(ENCLOSING_MARK_SET, EnclosingMark);
    general_category_static!(NONSPACING_MARK_SET, NonspacingMark);
    general_category_static!(NUMBER_SET, Number);
    general_category_static!(DECIMAL_NUMBER_SET, DecimalNumber);
    general_category_static!(LETTER_NUMBER_SET, LetterNumber);
    general_category_static!(OTHER_NUMBER_SET, OtherNumber);
    general_category_static!(PUNCTUATION_SET, Punctuation);
    general_category_static!(CONNECTOR_PUNCTUATION_SET, ConnectorPunctuation);
    general_category_static!(DASH_PUNCTUATION_SET, DashPunctuation);
    general_category_static!(CLOSE_PUNCTUATION_SET, ClosePunctuation);
    general_category_static!(FINAL_PUNCTUATION_SET, FinalPunctuation);
    general_category_static!(INITIAL_PUNCTUATION_SET, InitialPunctuation);
    general_category_static!(OTHER_PUNCTUATION_SET, OtherPunctuation);
    general_category_static!(OPEN_PUNCTUATION_SET, OpenPunctuation);
    general_category_static!(SYMBOL_SET, Symbol);
    general_category_static!(CURRENCY_SYMBOL_SET, CurrencySymbol);
    general_category_static!(MODIFIER_SYMBOL_SET, ModifierSymbol);
    general_category_static!(MATH_SYMBOL_SET, MathSymbol);
    general_category_static!(OTHER_SYMBOL_SET, OtherSymbol);
    general_category_static!(SEPARATOR_SET, Separator);
    general_category_static!(LINE_SEPARATOR_SET, LineSeparator);
    general_category_static!(PARAGRAPH_SEPARATOR_SET, ParagraphSeparator);
    general_category_static!(SPACE_SEPARATOR_SET, SpaceSeparator);

    // Binary property sets
    macro_rules! binary_property_static {
        ($static_name:ident, $load_fn:ident) => {
            static $static_name: Lazy<CodePointSetData> =
                Lazy::new(|| sets::$load_fn(&BakedDataProvider).unwrap());
        };
    }

    binary_property_static!(ASCII_HEX_DIGIT_SET, load_ascii_hex_digit);
    binary_property_static!(ALPHABETIC_SET, load_alphabetic);
    binary_property_static!(BIDI_CONTROL_SET, load_bidi_control);
    binary_property_static!(BIDI_MIRRORED_SET, load_bidi_mirrored);
    binary_property_static!(CASE_IGNORABLE_SET, load_case_ignorable);
    binary_property_static!(CASED_SET, load_cased);
    binary_property_static!(CHANGES_WHEN_CASEFOLDED_SET, load_changes_when_casefolded);
    binary_property_static!(CHANGES_WHEN_CASEMAPPED_SET, load_changes_when_casemapped);
    binary_property_static!(CHANGES_WHEN_LOWERCASED_SET, load_changes_when_lowercased);
    binary_property_static!(CHANGES_WHEN_NFKC_CASEFOLDED_SET, load_changes_when_nfkc_casefolded);
    binary_property_static!(CHANGES_WHEN_TITLECASED_SET, load_changes_when_titlecased);
    binary_property_static!(CHANGES_WHEN_UPPERCASED_SET, load_changes_when_uppercased);
    binary_property_static!(DASH_SET, load_dash);
    binary_property_static!(DEFAULT_IGNORABLE_CODE_POINT_SET, load_default_ignorable_code_point);
    binary_property_static!(DEPRECATED_SET, load_deprecated);
    binary_property_static!(DIACRITIC_SET, load_diacritic);
    binary_property_static!(EMOJI_SET, load_emoji);
    binary_property_static!(EMOJI_COMPONENT_SET, load_emoji_component);
    binary_property_static!(EMOJI_MODIFIER_SET, load_emoji_modifier);
    binary_property_static!(EMOJI_MODIFIER_BASE_SET, load_emoji_modifier_base);
    binary_property_static!(EMOJI_PRESENTATION_SET, load_emoji_presentation);
    binary_property_static!(EXTENDED_PICTOGRAPHIC_SET, load_extended_pictographic);
    binary_property_static!(EXTENDER_SET, load_extender);
    binary_property_static!(GRAPHEME_BASE_SET, load_grapheme_base);
    binary_property_static!(GRAPHEME_EXTEND_SET, load_grapheme_extend);
    binary_property_static!(HEX_DIGIT_SET, load_hex_digit);
    binary_property_static!(IDS_BINARY_OPERATOR_SET, load_ids_binary_operator);
    binary_property_static!(IDS_TRINARY_OPERATOR_SET, load_ids_trinary_operator);
    binary_property_static!(ID_START_SET, load_id_start);
    binary_property_static!(ID_CONTINUE_SET, load_id_continue);
    binary_property_static!(IDEOGRAPHIC_SET, load_ideographic);
    binary_property_static!(JOIN_CONTROL_SET, load_join_control);
    binary_property_static!(LOGICAL_ORDER_EXCEPTION_SET, load_logical_order_exception);
    binary_property_static!(LOWERCASE_SET, load_lowercase);
    binary_property_static!(MATH_SET, load_math);
    binary_property_static!(NONCHARACTER_CODE_POINT_SET, load_noncharacter_code_point);
    binary_property_static!(PATTERN_SYNTAX_SET, load_pattern_syntax);
    binary_property_static!(PATTERN_WHITE_SPACE_SET, load_pattern_white_space);
    binary_property_static!(QUOTATION_MARK_SET, load_quotation_mark);
    binary_property_static!(RADICAL_SET, load_radical);
    binary_property_static!(REGIONAL_INDICATOR_SET, load_regional_indicator);
    binary_property_static!(SENTENCE_TERMINAL_SET, load_sentence_terminal);
    binary_property_static!(SOFT_DOTTED_SET, load_soft_dotted);
    binary_property_static!(TERMINAL_PUNCTUATION_SET, load_terminal_punctuation);
    binary_property_static!(UNIFIED_IDEOGRAPH_SET, load_unified_ideograph);
    binary_property_static!(UPPERCASE_SET, load_uppercase);
    binary_property_static!(VARIATION_SELECTOR_SET, load_variation_selector);
    binary_property_static!(WHITE_SPACE_SET, load_white_space);
    binary_property_static!(XID_CONTINUE_SET, load_xid_continue);
    binary_property_static!(XID_START_SET, load_xid_start);

    // Scripts
    static SCRIPT_CLASSIFIER: Lazy<ScriptWithExtensions> =
        Lazy::new(|| script::load_script_with_extensions_unstable(&BakedDataProvider).unwrap());
    static SCRIPT_NAMES: Lazy<PropertyValueNameToEnumMapper<Script>> =
        Lazy::new(|| Script::get_name_to_enum_mapper(&BakedDataProvider).unwrap());

    let locale_provider = LocaleFallbackProvider::try_new_unstable(BakedDataProvider).unwrap();

    ICU {
        general_categories: GeneralCategories {
            other: OTHER_SET.as_borrowed(),
            control: CONTROL_SET.as_borrowed(),
            format: FORMAT_SET.as_borrowed(),
            unassigned: UNASSIGNED_SET.as_borrowed(),
            private_use: PRIVATE_USE_SET.as_borrowed(),
            surrogate: SURROGATE_SET.as_borrowed(),
            letter: LETTER_SET.as_borrowed(),
            cased_letter: CASED_LETTER_SET.as_borrowed(),
            lowercase_letter: LOWERCASE_LETTER_SET.as_borrowed(),
            modifier_letter: MODIFIER_LETTER_SET.as_borrowed(),
            other_letter: OTHER_LETTER_SET.as_borrowed(),
            titlecase_letter: TITLECASE_LETTER_SET.as_borrowed(),
            uppercase_letter: UPPERCASE_LETTER_SET.as_borrowed(),
            mark: MARK_SET.as_borrowed(),
            spacing_mark: SPACING_MARK_SET.as_borrowed(),
            enclosing_mark: ENCLOSING_MARK_SET.as_borrowed(),
            nonspacing_mark: NONSPACING_MARK_SET.as_borrowed(),
            number: NUMBER_SET.as_borrowed(),
            decimal_number: DECIMAL_NUMBER_SET.as_borrowed(),
            letter_number: LETTER_NUMBER_SET.as_borrowed(),
            other_number: OTHER_NUMBER_SET.as_borrowed(),
            punctuation: PUNCTUATION_SET.as_borrowed(),
            connector_punctuation: CONNECTOR_PUNCTUATION_SET.as_borrowed(),
            dash_punctuation: DASH_PUNCTUATION_SET.as_borrowed(),
            close_punctuation: CLOSE_PUNCTUATION_SET.as_borrowed(),
            final_punctuation: FINAL_PUNCTUATION_SET.as_borrowed(),
            initial_punctuation: INITIAL_PUNCTUATION_SET.as_borrowed(),
            other_punctuation: OTHER_PUNCTUATION_SET.as_borrowed(),
            open_punctuation: OPEN_PUNCTUATION_SET.as_borrowed(),
            symbol: SYMBOL_SET.as_borrowed(),
            currency_symbol: CURRENCY_SYMBOL_SET.as_borrowed(),
            modifier_symbol: MODIFIER_SYMBOL_SET.as_borrowed(),
            math_symbol: MATH_SYMBOL_SET.as_borrowed(),
            other_symbol: OTHER_SYMBOL_SET.as_borrowed(),
            separator: SEPARATOR_SET.as_borrowed(),
            line_separator: LINE_SEPARATOR_SET.as_borrowed(),
            paragraph_separator: PARAGRAPH_SEPARATOR_SET.as_borrowed(),
            space_separator: SPACE_SEPARATOR_SET.as_borrowed(),
        },
        scripts: Scripts {
            classifier: SCRIPT_CLASSIFIER.as_borrowed(),
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
            nfc: ComposingNormalizer::try_new_nfc_unstable(&BakedDataProvider).unwrap(),
            nfd: DecomposingNormalizer::try_new_nfd_unstable(&BakedDataProvider).unwrap(),
            nfkc: ComposingNormalizer::try_new_nfkc_unstable(&BakedDataProvider).unwrap(),
            nfkd: DecomposingNormalizer::try_new_nfkd_unstable(&BakedDataProvider).unwrap(),
        },
        collator: Collator::try_new_unstable(
            &locale_provider,
            &DataLocale::from(DEFAULT_LOCALE),
            CollatorOptions::new(),
        )
        .unwrap(),
        case_mapping: CaseMapping::try_new(&BakedDataProvider).unwrap(),
    }
});
