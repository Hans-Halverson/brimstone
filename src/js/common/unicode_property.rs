use icu_properties::props::{GeneralCategoryGroup, Script};

use super::{
    icu::ICU,
    unicode::{is_ascii, CodePoint},
};

#[derive(Clone, Copy, Debug)]
pub enum UnicodeProperty {
    Binary(BinaryUnicodeProperty),
    GeneralCategory(GeneralCategoryProperty),
    Script(ScriptProperty),
}

impl UnicodeProperty {
    pub fn is_match(&self, code_point: CodePoint) -> bool {
        match self {
            Self::Binary(property) => property.is_match(code_point),
            Self::GeneralCategory(property) => property.is_match(code_point),
            Self::Script(property) => property.is_match(code_point),
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

    pub fn is_match(&self, code_point: CodePoint) -> bool {
        match self {
            Self::ASCII => is_ascii(code_point),
            Self::ASCIIHexDigit => ICU.properties.ascii_hex_digit.contains32(code_point),
            Self::Alphabetic => ICU.properties.alphabetic.contains32(code_point),
            Self::Any => true,
            // Note that the assigned property is not a regular property set and instead is treated
            // as the complement of the assigned general category `\P{C}`.
            Self::Assigned => !in_category_group(code_point, GeneralCategoryGroup::Unassigned),
            Self::BidiControl => ICU.properties.bidi_control.contains32(code_point),
            Self::BidiMirrored => ICU.properties.bidi_mirrored.contains32(code_point),
            Self::CaseIgnorable => ICU.properties.case_ignorable.contains32(code_point),
            Self::Cased => ICU.properties.cased.contains32(code_point),
            Self::ChangesWhenCasefolded => ICU
                .properties
                .changes_when_casefolded
                .contains32(code_point),
            Self::ChangesWhenCasemapped => ICU
                .properties
                .changes_when_casemapped
                .contains32(code_point),
            Self::ChangesWhenLowercased => ICU
                .properties
                .changes_when_lowercased
                .contains32(code_point),
            Self::ChangesWhenNFKCCasefolded => ICU
                .properties
                .changes_when_nfkc_casefolded
                .contains32(code_point),
            Self::ChangesWhenTitlecased => ICU
                .properties
                .changes_when_titlecased
                .contains32(code_point),
            Self::ChangesWhenUppercased => ICU
                .properties
                .changes_when_uppercased
                .contains32(code_point),
            Self::Dash => ICU.properties.dash.contains32(code_point),
            Self::DefaultIgnorableCodePoint => ICU
                .properties
                .default_ignorable_code_point
                .contains32(code_point),
            Self::Deprecated => ICU.properties.deprecated.contains32(code_point),
            Self::Diacritic => ICU.properties.diacritic.contains32(code_point),
            Self::Emoji => ICU.properties.emoji.contains32(code_point),
            Self::EmojiComponent => ICU.properties.emoji_component.contains32(code_point),
            Self::EmojiModifier => ICU.properties.emoji_modifier.contains32(code_point),
            Self::EmojiModifierBase => ICU.properties.emoji_modifier_base.contains32(code_point),
            Self::EmojiPresentation => ICU.properties.emoji_presentation.contains32(code_point),
            Self::ExtendedPictographic => {
                ICU.properties.extended_pictographic.contains32(code_point)
            }
            Self::Extender => ICU.properties.extender.contains32(code_point),
            Self::GraphemeBase => ICU.properties.grapheme_base.contains32(code_point),
            Self::GraphemeExtend => ICU.properties.grapheme_extend.contains32(code_point),
            Self::HexDigit => ICU.properties.hex_digit.contains32(code_point),
            Self::IDSBinaryOperator => ICU.properties.ids_binary_operator.contains32(code_point),
            Self::IDSTrinaryOperator => ICU.properties.ids_trinary_operator.contains32(code_point),
            Self::IDContinue => ICU.properties.id_continue.contains32(code_point),
            Self::IDStart => ICU.properties.id_start.contains32(code_point),
            Self::Ideographic => ICU.properties.ideographic.contains32(code_point),
            Self::JoinControl => ICU.properties.join_control.contains32(code_point),
            Self::LogicalOrderException => ICU
                .properties
                .logical_order_exception
                .contains32(code_point),
            Self::Lowercase => ICU.properties.lowercase.contains32(code_point),
            Self::Math => ICU.properties.math.contains32(code_point),
            Self::NoncharacterCodePoint => ICU
                .properties
                .noncharacter_code_point
                .contains32(code_point),
            Self::PatternSyntax => ICU.properties.pattern_syntax.contains32(code_point),
            Self::PatternWhiteSpace => ICU.properties.pattern_white_space.contains32(code_point),
            Self::QuotationMark => ICU.properties.quotation_mark.contains32(code_point),
            Self::Radical => ICU.properties.radical.contains32(code_point),
            Self::RegionalIndicator => ICU.properties.regional_indicator.contains32(code_point),
            Self::SentenceTerminal => ICU.properties.sentence_terminal.contains32(code_point),
            Self::SoftDotted => ICU.properties.soft_dotted.contains32(code_point),
            Self::TerminalPunctuation => ICU.properties.terminal_punctuation.contains32(code_point),
            Self::UnifiedIdeograph => ICU.properties.unified_ideograph.contains32(code_point),
            Self::Uppercase => ICU.properties.uppercase.contains32(code_point),
            Self::VariationSelector => ICU.properties.variation_selector.contains32(code_point),
            Self::WhiteSpace => ICU.properties.white_space.contains32(code_point),
            Self::XIDContinue => ICU.properties.xid_continue.contains32(code_point),
            Self::XIDStart => ICU.properties.xid_start.contains32(code_point),
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

    pub fn is_match(&self, code_point: CodePoint) -> bool {
        match self {
            Self::Other => in_category_group(code_point, GeneralCategoryGroup::Other),
            Self::Control => in_category_group(code_point, GeneralCategoryGroup::Control),
            Self::Format => in_category_group(code_point, GeneralCategoryGroup::Format),
            Self::Unassigned => in_category_group(code_point, GeneralCategoryGroup::Unassigned),
            Self::PrivateUse => in_category_group(code_point, GeneralCategoryGroup::PrivateUse),
            Self::Surrogate => in_category_group(code_point, GeneralCategoryGroup::Surrogate),
            Self::Letter => in_category_group(code_point, GeneralCategoryGroup::Letter),
            Self::CasedLetter => in_category_group(code_point, GeneralCategoryGroup::CasedLetter),
            Self::LowercaseLetter => {
                in_category_group(code_point, GeneralCategoryGroup::LowercaseLetter)
            }
            Self::ModifierLetter => {
                in_category_group(code_point, GeneralCategoryGroup::ModifierLetter)
            }
            Self::OtherLetter => in_category_group(code_point, GeneralCategoryGroup::OtherLetter),
            Self::TitlecaseLetter => {
                in_category_group(code_point, GeneralCategoryGroup::TitlecaseLetter)
            }
            Self::UppercaseLetter => {
                in_category_group(code_point, GeneralCategoryGroup::UppercaseLetter)
            }
            Self::Mark => in_category_group(code_point, GeneralCategoryGroup::Mark),
            Self::SpacingMark => in_category_group(code_point, GeneralCategoryGroup::SpacingMark),
            Self::EnclosingMark => {
                in_category_group(code_point, GeneralCategoryGroup::EnclosingMark)
            }
            Self::NonspacingMark => {
                in_category_group(code_point, GeneralCategoryGroup::NonspacingMark)
            }
            Self::Number => in_category_group(code_point, GeneralCategoryGroup::Number),
            Self::DecimalNumber => {
                in_category_group(code_point, GeneralCategoryGroup::DecimalNumber)
            }
            Self::LetterNumber => in_category_group(code_point, GeneralCategoryGroup::LetterNumber),
            Self::OtherNumber => in_category_group(code_point, GeneralCategoryGroup::OtherNumber),
            Self::Punctuation => in_category_group(code_point, GeneralCategoryGroup::Punctuation),
            Self::ConnectorPunctuation => {
                in_category_group(code_point, GeneralCategoryGroup::ConnectorPunctuation)
            }
            Self::DashPunctuation => {
                in_category_group(code_point, GeneralCategoryGroup::DashPunctuation)
            }
            Self::ClosePunctuation => {
                in_category_group(code_point, GeneralCategoryGroup::ClosePunctuation)
            }
            Self::FinalPunctuation => {
                in_category_group(code_point, GeneralCategoryGroup::FinalPunctuation)
            }
            Self::InitialPunctuation => {
                in_category_group(code_point, GeneralCategoryGroup::InitialPunctuation)
            }
            Self::OtherPunctuation => {
                in_category_group(code_point, GeneralCategoryGroup::OtherPunctuation)
            }
            Self::OpenPunctuation => {
                in_category_group(code_point, GeneralCategoryGroup::OpenPunctuation)
            }
            Self::Symbol => in_category_group(code_point, GeneralCategoryGroup::Symbol),
            Self::CurrencySymbol => {
                in_category_group(code_point, GeneralCategoryGroup::CurrencySymbol)
            }
            Self::ModifierSymbol => {
                in_category_group(code_point, GeneralCategoryGroup::ModifierSymbol)
            }
            Self::MathSymbol => in_category_group(code_point, GeneralCategoryGroup::MathSymbol),
            Self::OtherSymbol => in_category_group(code_point, GeneralCategoryGroup::OtherSymbol),
            Self::Separator => in_category_group(code_point, GeneralCategoryGroup::Separator),
            Self::LineSeparator => {
                in_category_group(code_point, GeneralCategoryGroup::LineSeparator)
            }
            Self::ParagraphSeparator => {
                in_category_group(code_point, GeneralCategoryGroup::ParagraphSeparator)
            }
            Self::SpaceSeparator => {
                in_category_group(code_point, GeneralCategoryGroup::SpaceSeparator)
            }
        }
    }
}

fn in_category_group(code_point: CodePoint, category_group: GeneralCategoryGroup) -> bool {
    let category = ICU.general_categories.classifier.get32(code_point);
    category_group.contains(category)
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

    pub fn is_match(&self, code_point: CodePoint) -> bool {
        if self.with_extensions {
            ICU.scripts.classifier.has_script32(code_point, self.script)
        } else {
            ICU.scripts.classifier.get_script_val32(code_point) == self.script
        }
    }
}
