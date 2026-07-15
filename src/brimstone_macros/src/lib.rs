use proc_macro::TokenStream;

mod match_u32;

/// A macro that allows a match expression on a u32 value to match against char literal patterns.
/// This is useful for matching a generic u32 code point value against u32's represented as char
/// literals. Normally there is no way to match a non-char value against char literal patterns.
///
/// Example:
///   match_u32!(match (code_point as u32) {
///       'a' => println!("a"),
///       'b' => println!("b"),
///       _ => println!("not a or b"),
///  });
///
/// Implemented by creating const literals for each char literal in the pattern and then replacing
/// each char literal pattern with a reference to the extracted constant.
#[proc_macro]
pub fn match_u32(item: TokenStream) -> TokenStream {
    match_u32::match_u32(item)
}
