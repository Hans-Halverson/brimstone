use icu_collections::codepointinvlist::CodePointInversionListBuilder;
use icu_properties::{
    props::{GeneralCategory, GeneralCategoryGroup, Script},
    CodePointSetDataBorrowed,
};

use super::{icu::ICU, unicode::MAX_CODE_POINT};

#[derive(Clone, Copy, Debug)]
pub enum UnicodeProperty {
    Binary(BinaryUnicodeProperty),
    GeneralCategory(GeneralCategoryProperty),
    Script(ScriptProperty),
}

impl UnicodeProperty {
    pub fn add_to_set(&self, set_builder: &mut CodePointInversionListBuilder) {
        match self {
            Self::Binary(property) => property.add_to_set(set_builder),
            Self::GeneralCategory(property) => property.add_to_set(set_builder),
            Self::Script(property) => property.add_to_set(set_builder),
        }
    }
}

/// All binary unicode properties listed in the spec
#[derive(Clone, Copy, Debug)]
#[allow(clippy::upper_case_acronyms)]
pub enum BinaryUnicodeProperty {
    ASCII,
    ASCIIHexDigit,
    Alphabetic,
    Any,
    Assigned,
    BidiControl,
    BidiMirrored,
    CaseIgnorable,
    Cased,
    ChangesWhenCasefolded,
    ChangesWhenCasemapped,
    ChangesWhenLowercased,
    ChangesWhenNFKCCasefolded,
    ChangesWhenTitlecased,
    ChangesWhenUppercased,
    Dash,
    DefaultIgnorableCodePoint,
    Deprecated,
    Diacritic,
    Emoji,
    EmojiComponent,
    EmojiModifier,
    EmojiModifierBase,
    EmojiPresentation,
    ExtendedPictographic,
    Extender,
    GraphemeBase,
    GraphemeExtend,
    HexDigit,
    IDSBinaryOperator,
    IDSTrinaryOperator,
    IDContinue,
    IDStart,
    Ideographic,
    JoinControl,
    LogicalOrderException,
    Lowercase,
    Math,
    NoncharacterCodePoint,
    PatternSyntax,
    PatternWhiteSpace,
    QuotationMark,
    Radical,
    RegionalIndicator,
    SentenceTerminal,
    SoftDotted,
    TerminalPunctuation,
    UnifiedIdeograph,
    Uppercase,
    VariationSelector,
    WhiteSpace,
    XIDContinue,
    XIDStart,
}

impl BinaryUnicodeProperty {
    pub fn parse(str: &str) -> Option<BinaryUnicodeProperty> {
        let property = match str {
            "ASCII" => BinaryUnicodeProperty::ASCII,
            "ASCII_Hex_Digit" => BinaryUnicodeProperty::ASCIIHexDigit,
            "AHex" => BinaryUnicodeProperty::ASCIIHexDigit,
            "Alphabetic" | "Alpha" => BinaryUnicodeProperty::Alphabetic,
            "Any" => BinaryUnicodeProperty::Any,
            "Assigned" => BinaryUnicodeProperty::Assigned,
            "Bidi_Control" | "Bidi_C" => BinaryUnicodeProperty::BidiControl,
            "Bidi_Mirrored" | "Bidi_M" => BinaryUnicodeProperty::BidiMirrored,
            "Case_Ignorable" | "CI" => BinaryUnicodeProperty::CaseIgnorable,
            "Cased" => BinaryUnicodeProperty::Cased,
            "Changes_When_Casefolded" | "CWCF" => BinaryUnicodeProperty::ChangesWhenCasefolded,
            "Changes_When_Casemapped" | "CWCM" => BinaryUnicodeProperty::ChangesWhenCasemapped,
            "Changes_When_Lowercased" | "CWL" => BinaryUnicodeProperty::ChangesWhenLowercased,
            "Changes_When_NFKC_Casefolded" | "CWKCF" => {
                BinaryUnicodeProperty::ChangesWhenNFKCCasefolded
            }
            "Changes_When_Titlecased" | "CWT" => BinaryUnicodeProperty::ChangesWhenTitlecased,
            "Changes_When_Uppercased" | "CWU" => BinaryUnicodeProperty::ChangesWhenUppercased,
            "Dash" => BinaryUnicodeProperty::Dash,
            "Default_Ignorable_Code_Point" | "DI" => {
                BinaryUnicodeProperty::DefaultIgnorableCodePoint
            }
            "Deprecated" | "Dep" => BinaryUnicodeProperty::Deprecated,
            "Diacritic" | "Dia" => BinaryUnicodeProperty::Diacritic,
            "Emoji" => BinaryUnicodeProperty::Emoji,
            "Emoji_Component" | "EComp" => BinaryUnicodeProperty::EmojiComponent,
            "Emoji_Modifier" | "EMod" => BinaryUnicodeProperty::EmojiModifier,
            "Emoji_Modifier_Base" | "EBase" => BinaryUnicodeProperty::EmojiModifierBase,
            "Emoji_Presentation" | "EPres" => BinaryUnicodeProperty::EmojiPresentation,
            "Extended_Pictographic" | "ExtPict" => BinaryUnicodeProperty::ExtendedPictographic,
            "Extender" | "Ext" => BinaryUnicodeProperty::Extender,
            "Grapheme_Base" | "Gr_Base" => BinaryUnicodeProperty::GraphemeBase,
            "Grapheme_Extend" | "Gr_Ext" => BinaryUnicodeProperty::GraphemeExtend,
            "Hex_Digit" | "Hex" => BinaryUnicodeProperty::HexDigit,
            "IDS_Binary_Operator" | "IDSB" => BinaryUnicodeProperty::IDSBinaryOperator,
            "IDS_Trinary_Operator" | "IDST" => BinaryUnicodeProperty::IDSTrinaryOperator,
            "ID_Continue" | "IDC" => BinaryUnicodeProperty::IDContinue,
            "ID_Start" | "IDS" => BinaryUnicodeProperty::IDStart,
            "Ideographic" | "Ideo" => BinaryUnicodeProperty::Ideographic,
            "Join_Control" | "Join_C" => BinaryUnicodeProperty::JoinControl,
            "Logical_Order_Exception" | "LOE" => BinaryUnicodeProperty::LogicalOrderException,
            "Lowercase" | "Lower" => BinaryUnicodeProperty::Lowercase,
            "Math" => BinaryUnicodeProperty::Math,
            "Noncharacter_Code_Point" | "NChar" => BinaryUnicodeProperty::NoncharacterCodePoint,
            "Pattern_Syntax" | "Pat_Syn" => BinaryUnicodeProperty::PatternSyntax,
            "Pattern_White_Space" | "Pat_WS" => BinaryUnicodeProperty::PatternWhiteSpace,
            "Quotation_Mark" | "QMark" => BinaryUnicodeProperty::QuotationMark,
            "Radical" => BinaryUnicodeProperty::Radical,
            "Regional_Indicator" | "RI" => BinaryUnicodeProperty::RegionalIndicator,
            "Sentence_Terminal" | "STerm" => BinaryUnicodeProperty::SentenceTerminal,
            "Soft_Dotted" | "SD" => BinaryUnicodeProperty::SoftDotted,
            "Terminal_Punctuation" | "Term" => BinaryUnicodeProperty::TerminalPunctuation,
            "Unified_Ideograph" | "UIdeo" => BinaryUnicodeProperty::UnifiedIdeograph,
            "Uppercase" | "Upper" => BinaryUnicodeProperty::Uppercase,
            "Variation_Selector" | "VS" => BinaryUnicodeProperty::VariationSelector,
            "White_Space" | "space" => BinaryUnicodeProperty::WhiteSpace,
            "XID_Continue" | "XIDC" => BinaryUnicodeProperty::XIDContinue,
            "XID_Start" | "XIDS" => BinaryUnicodeProperty::XIDStart,
            _ => return None,
        };

        Some(property)
    }

    pub fn add_to_set(&self, set_builder: &mut CodePointInversionListBuilder) {
        match self {
            Self::ASCII => set_builder.add_range32(0x00..=0x7F),
            Self::ASCIIHexDigit => {
                add_binary_property_to_set(set_builder, &ICU.properties.ascii_hex_digit)
            }
            Self::Alphabetic => add_binary_property_to_set(set_builder, &ICU.properties.alphabetic),
            Self::Any => set_builder.add_range32(0x00..=MAX_CODE_POINT),
            Self::Assigned => {
                todo!()
            }
            Self::BidiControl => {
                add_binary_property_to_set(set_builder, &ICU.properties.bidi_control)
            }
            Self::BidiMirrored => {
                add_binary_property_to_set(set_builder, &ICU.properties.bidi_mirrored)
            }
            Self::CaseIgnorable => {
                add_binary_property_to_set(set_builder, &ICU.properties.case_ignorable)
            }
            Self::Cased => add_binary_property_to_set(set_builder, &ICU.properties.cased),
            Self::ChangesWhenCasefolded => {
                add_binary_property_to_set(set_builder, &ICU.properties.changes_when_casefolded)
            }
            Self::ChangesWhenCasemapped => {
                add_binary_property_to_set(set_builder, &ICU.properties.changes_when_casemapped)
            }
            Self::ChangesWhenLowercased => {
                add_binary_property_to_set(set_builder, &ICU.properties.changes_when_lowercased)
            }
            Self::ChangesWhenNFKCCasefolded => add_binary_property_to_set(
                set_builder,
                &ICU.properties.changes_when_nfkc_casefolded,
            ),
            Self::ChangesWhenTitlecased => {
                add_binary_property_to_set(set_builder, &ICU.properties.changes_when_titlecased)
            }
            Self::ChangesWhenUppercased => {
                add_binary_property_to_set(set_builder, &ICU.properties.changes_when_uppercased)
            }
            Self::Dash => add_binary_property_to_set(set_builder, &ICU.properties.dash),
            Self::DefaultIgnorableCodePoint => add_binary_property_to_set(
                set_builder,
                &ICU.properties.default_ignorable_code_point,
            ),
            Self::Deprecated => add_binary_property_to_set(set_builder, &ICU.properties.deprecated),
            Self::Diacritic => add_binary_property_to_set(set_builder, &ICU.properties.diacritic),
            Self::Emoji => add_binary_property_to_set(set_builder, &ICU.properties.emoji),
            Self::EmojiComponent => {
                add_binary_property_to_set(set_builder, &ICU.properties.emoji_component)
            }
            Self::EmojiModifier => {
                add_binary_property_to_set(set_builder, &ICU.properties.emoji_modifier)
            }
            Self::EmojiModifierBase => {
                add_binary_property_to_set(set_builder, &ICU.properties.emoji_modifier_base)
            }
            Self::EmojiPresentation => {
                add_binary_property_to_set(set_builder, &ICU.properties.emoji_presentation)
            }
            Self::ExtendedPictographic => {
                add_binary_property_to_set(set_builder, &ICU.properties.extended_pictographic)
            }
            Self::Extender => add_binary_property_to_set(set_builder, &ICU.properties.extender),
            Self::GraphemeBase => {
                add_binary_property_to_set(set_builder, &ICU.properties.grapheme_base)
            }
            Self::GraphemeExtend => {
                add_binary_property_to_set(set_builder, &ICU.properties.grapheme_extend)
            }
            Self::HexDigit => add_binary_property_to_set(set_builder, &ICU.properties.hex_digit),
            Self::IDSBinaryOperator => {
                add_binary_property_to_set(set_builder, &ICU.properties.ids_binary_operator)
            }
            Self::IDSTrinaryOperator => {
                add_binary_property_to_set(set_builder, &ICU.properties.ids_trinary_operator)
            }
            Self::IDContinue => {
                add_binary_property_to_set(set_builder, &ICU.properties.id_continue)
            }
            Self::IDStart => add_binary_property_to_set(set_builder, &ICU.properties.id_start),
            Self::Ideographic => {
                add_binary_property_to_set(set_builder, &ICU.properties.ideographic)
            }
            Self::JoinControl => {
                add_binary_property_to_set(set_builder, &ICU.properties.join_control)
            }
            Self::LogicalOrderException => {
                add_binary_property_to_set(set_builder, &ICU.properties.logical_order_exception)
            }
            Self::Lowercase => add_binary_property_to_set(set_builder, &ICU.properties.lowercase),
            Self::Math => add_binary_property_to_set(set_builder, &ICU.properties.math),
            Self::NoncharacterCodePoint => {
                add_binary_property_to_set(set_builder, &ICU.properties.noncharacter_code_point)
            }
            Self::PatternSyntax => {
                add_binary_property_to_set(set_builder, &ICU.properties.pattern_syntax)
            }
            Self::PatternWhiteSpace => {
                add_binary_property_to_set(set_builder, &ICU.properties.pattern_white_space)
            }
            Self::QuotationMark => {
                add_binary_property_to_set(set_builder, &ICU.properties.quotation_mark)
            }
            Self::Radical => add_binary_property_to_set(set_builder, &ICU.properties.radical),
            Self::RegionalIndicator => {
                add_binary_property_to_set(set_builder, &ICU.properties.regional_indicator)
            }
            Self::SentenceTerminal => {
                add_binary_property_to_set(set_builder, &ICU.properties.sentence_terminal)
            }
            Self::SoftDotted => {
                add_binary_property_to_set(set_builder, &ICU.properties.soft_dotted)
            }
            Self::TerminalPunctuation => {
                add_binary_property_to_set(set_builder, &ICU.properties.terminal_punctuation)
            }
            Self::UnifiedIdeograph => {
                add_binary_property_to_set(set_builder, &ICU.properties.unified_ideograph)
            }
            Self::Uppercase => add_binary_property_to_set(set_builder, &ICU.properties.uppercase),
            Self::VariationSelector => {
                add_binary_property_to_set(set_builder, &ICU.properties.variation_selector)
            }
            Self::WhiteSpace => {
                add_binary_property_to_set(set_builder, &ICU.properties.white_space)
            }
            Self::XIDContinue => {
                add_binary_property_to_set(set_builder, &ICU.properties.xid_continue)
            }
            Self::XIDStart => add_binary_property_to_set(set_builder, &ICU.properties.xid_start),
        }
    }
}

/// All General_Category properties listed in the spec
#[derive(Clone, Copy, Debug)]
pub enum GeneralCategoryProperty {
    /// The C general category
    Other,
    /// The Cc general category
    Control,
    /// The Cf general category
    Format,
    /// The Cn general category
    Unassigned,
    /// The Co general category
    PrivateUse,
    /// The Cs general category
    Surrogate,
    /// The L general category
    Letter,
    /// The LC general category
    CasedLetter,
    /// The Ll general category
    LowercaseLetter,
    /// The Lm general category
    ModifierLetter,
    /// The Lo general category
    OtherLetter,
    /// The Lt general category
    TitlecaseLetter,
    /// The Lu general category
    UppercaseLetter,
    /// The M general category
    Mark,
    /// The Mc general category
    SpacingMark,
    /// The Me general category
    EnclosingMark,
    /// The Mn general category
    NonspacingMark,
    /// The N general category
    Number,
    /// The Nd general category
    DecimalNumber,
    /// The Nl general category
    LetterNumber,
    /// The No general category
    OtherNumber,
    /// The P general category
    Punctuation,
    /// The Pc general category
    ConnectorPunctuation,
    /// The Pd general category
    DashPunctuation,
    /// The Pe general category
    ClosePunctuation,
    /// The Pf general category
    FinalPunctuation,
    /// The Pi general category
    InitialPunctuation,
    /// The Po general category
    OtherPunctuation,
    /// The Ps general category
    OpenPunctuation,
    /// The S general category
    Symbol,
    /// The Sc general category
    CurrencySymbol,
    /// The Sk general category
    ModifierSymbol,
    /// The Sm general category
    MathSymbol,
    /// The So general category
    OtherSymbol,
    /// The Z general category
    Separator,
    /// The Zl general category
    LineSeparator,
    /// The Zp general category
    ParagraphSeparator,
    /// The Zs general category
    SpaceSeparator,
}

impl GeneralCategoryProperty {
    pub fn parse(str: &str) -> Option<GeneralCategoryProperty> {
        let property = match str {
            "C" | "Other" => GeneralCategoryProperty::Other,
            "Cc" | "Control" | "cntrl" => GeneralCategoryProperty::Control,
            "Cf" | "Format" => GeneralCategoryProperty::Format,
            "Cn" | "Unassigned" => GeneralCategoryProperty::Unassigned,
            "Co" | "Private_Use" => GeneralCategoryProperty::PrivateUse,
            "Cs" | "Surrogate" => GeneralCategoryProperty::Surrogate,
            "L" | "Letter" => GeneralCategoryProperty::Letter,
            "LC" | "Cased_Letter" => GeneralCategoryProperty::CasedLetter,
            "Ll" | "Lowercase_Letter" => GeneralCategoryProperty::LowercaseLetter,
            "Lm" | "Modifier_Letter" => GeneralCategoryProperty::ModifierLetter,
            "Lo" | "Other_Letter" => GeneralCategoryProperty::OtherLetter,
            "Lt" | "Titlecase_Letter" => GeneralCategoryProperty::TitlecaseLetter,
            "Lu" | "Uppercase_Letter" => GeneralCategoryProperty::UppercaseLetter,
            "M" | "Mark" | "Combining_Mark" => GeneralCategoryProperty::Mark,
            "Mc" | "Spacing_Mark" => GeneralCategoryProperty::SpacingMark,
            "Me" | "Enclosing_Mark" => GeneralCategoryProperty::EnclosingMark,
            "Mn" | "Nonspacing_Mark" => GeneralCategoryProperty::NonspacingMark,
            "N" | "Number" => GeneralCategoryProperty::Number,
            "Nd" | "Decimal_Number" | "digit" => GeneralCategoryProperty::DecimalNumber,
            "Nl" | "Letter_Number" => GeneralCategoryProperty::LetterNumber,
            "No" | "Other_Number" => GeneralCategoryProperty::OtherNumber,
            "P" | "Punctuation" | "punct" => GeneralCategoryProperty::Punctuation,
            "Pc" | "Connector_Punctuation" => GeneralCategoryProperty::ConnectorPunctuation,
            "Pd" | "Dash_Punctuation" => GeneralCategoryProperty::DashPunctuation,
            "Pe" | "Close_Punctuation" => GeneralCategoryProperty::ClosePunctuation,
            "Pf" | "Final_Punctuation" => GeneralCategoryProperty::FinalPunctuation,
            "Pi" | "Initial_Punctuation" => GeneralCategoryProperty::InitialPunctuation,
            "Po" | "Other_Punctuation" => GeneralCategoryProperty::OtherPunctuation,
            "Ps" | "Open_Punctuation" => GeneralCategoryProperty::OpenPunctuation,
            "S" | "Symbol" => GeneralCategoryProperty::Symbol,
            "Sc" | "Currency_Symbol" => GeneralCategoryProperty::CurrencySymbol,
            "Sk" | "Modifier_Symbol" => GeneralCategoryProperty::ModifierSymbol,
            "Sm" | "Math_Symbol" => GeneralCategoryProperty::MathSymbol,
            "So" | "Other_Symbol" => GeneralCategoryProperty::OtherSymbol,
            "Z" | "Separator" => GeneralCategoryProperty::Separator,
            "Zl" | "Line_Separator" => GeneralCategoryProperty::LineSeparator,
            "Zp" | "Paragraph_Separator" => GeneralCategoryProperty::ParagraphSeparator,
            "Zs" | "Space_Separator" => GeneralCategoryProperty::SpaceSeparator,
            _ => return None,
        };

        Some(property)
    }

    pub fn add_to_set(&self, set_builder: &mut CodePointInversionListBuilder) {
        match self {
            Self::Other => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::Other)
            }
            Self::Control => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::Control)
            }
            Self::Format => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::Format)
            }
            Self::Unassigned => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::Unassigned)
            }
            Self::PrivateUse => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::PrivateUse)
            }
            Self::Surrogate => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::Surrogate)
            }
            Self::Letter => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::Letter)
            }
            Self::CasedLetter => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::CasedLetter)
            }
            Self::LowercaseLetter => add_general_category_group_to_set(
                set_builder,
                GeneralCategoryGroup::LowercaseLetter,
            ),
            Self::ModifierLetter => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::ModifierLetter)
            }
            Self::OtherLetter => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::OtherLetter)
            }
            Self::TitlecaseLetter => add_general_category_group_to_set(
                set_builder,
                GeneralCategoryGroup::TitlecaseLetter,
            ),
            Self::UppercaseLetter => add_general_category_group_to_set(
                set_builder,
                GeneralCategoryGroup::UppercaseLetter,
            ),
            Self::Mark => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::Mark)
            }
            Self::SpacingMark => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::SpacingMark)
            }
            Self::EnclosingMark => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::EnclosingMark)
            }
            Self::NonspacingMark => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::NonspacingMark)
            }
            Self::Number => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::Number)
            }
            Self::DecimalNumber => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::DecimalNumber)
            }
            Self::LetterNumber => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::LetterNumber)
            }
            Self::OtherNumber => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::OtherNumber)
            }
            Self::Punctuation => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::Punctuation)
            }
            Self::ConnectorPunctuation => add_general_category_group_to_set(
                set_builder,
                GeneralCategoryGroup::ConnectorPunctuation,
            ),
            Self::DashPunctuation => add_general_category_group_to_set(
                set_builder,
                GeneralCategoryGroup::DashPunctuation,
            ),
            Self::ClosePunctuation => add_general_category_group_to_set(
                set_builder,
                GeneralCategoryGroup::ClosePunctuation,
            ),
            Self::FinalPunctuation => add_general_category_group_to_set(
                set_builder,
                GeneralCategoryGroup::FinalPunctuation,
            ),
            Self::InitialPunctuation => add_general_category_group_to_set(
                set_builder,
                GeneralCategoryGroup::InitialPunctuation,
            ),
            Self::OtherPunctuation => add_general_category_group_to_set(
                set_builder,
                GeneralCategoryGroup::OtherPunctuation,
            ),
            Self::OpenPunctuation => add_general_category_group_to_set(
                set_builder,
                GeneralCategoryGroup::OpenPunctuation,
            ),
            Self::Symbol => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::Symbol)
            }
            Self::CurrencySymbol => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::CurrencySymbol)
            }
            Self::ModifierSymbol => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::ModifierSymbol)
            }
            Self::MathSymbol => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::MathSymbol)
            }
            Self::OtherSymbol => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::OtherSymbol)
            }
            Self::Separator => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::Separator)
            }
            Self::LineSeparator => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::LineSeparator)
            }
            Self::ParagraphSeparator => add_general_category_group_to_set(
                set_builder,
                GeneralCategoryGroup::ParagraphSeparator,
            ),
            Self::SpaceSeparator => {
                add_general_category_group_to_set(set_builder, GeneralCategoryGroup::SpaceSeparator)
            }
        }
    }
}

#[inline]
fn add_binary_property_to_set(
    set_builder: &mut CodePointInversionListBuilder,
    binary_property_set: &CodePointSetDataBorrowed<'static>,
) {
    set_builder.add_set(
        &binary_property_set
            .static_to_owned()
            .to_code_point_inversion_list(),
    );
}

#[inline]
fn add_general_category_group_to_set(
    set_builder: &mut CodePointInversionListBuilder,
    general_category_group: GeneralCategoryGroup,
) {
    // Find the general categories which are a part of this group by iterating through all general
    // categories.
    for general_category in GeneralCategory::ALL_VALUES {
        if general_category_group.contains(*general_category) {
            // Add the set of code points for this general category to the set builder.
            let general_category_set = ICU
                .general_categories
                .classifier
                .get_set_for_value(*general_category);
            set_builder.add_set(&general_category_set.to_code_point_inversion_list());
        }
    }
}

/// A script property with or without extensions
#[derive(Clone, Copy, Debug)]
pub struct ScriptProperty {
    script: Script,
    /// Whether this is a Script_Extensions property or a regular Script property
    with_extensions: bool,
}

impl ScriptProperty {
    pub fn parse(str: &str, with_extensions: bool) -> Option<ScriptProperty> {
        ICU.scripts
            .names
            .get_strict(str)
            .map(|script| ScriptProperty { script, with_extensions })
    }

    pub fn add_to_set(&self, set_builder: &mut CodePointInversionListBuilder) {
        if self.with_extensions {
            let set = ICU
                .scripts
                .script_with_extension_classifier
                .get_script_extensions_set(self.script);
            set_builder.add_set(&set);
        } else {
            let set = ICU.scripts.script_classifier.get_set_for_value(self.script);
            set_builder.add_set(&set.to_code_point_inversion_list())
        }
    }
}
