use super::{
    icu::ICU,
    unicode::{is_ascii, CodePoint},
};

#[derive(Clone, Copy, Debug)]
pub enum UnicodeProperty {
    Binary(BinaryUnicodeProperty),
}

impl UnicodeProperty {
    pub fn is_match(&self, code_point: CodePoint) -> bool {
        match self {
            Self::Binary(property) => property.is_match(code_point),
        }
    }
}

/// All binary unicode properties listed in the spec
#[derive(Clone, Copy, Debug)]
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
            Self::Assigned => {
                unimplemented!("Assigned property requires General_Category properties")
            }
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
