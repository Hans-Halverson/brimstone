/// Strings are represented as a sequence of u8s representing code points or a sequence of u16s
/// representing code units.
#[derive(Clone, Copy, PartialEq)]
pub enum StringWidth {
    OneByte,
    TwoByte,
}
