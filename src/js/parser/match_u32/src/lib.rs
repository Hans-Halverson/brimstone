use std::collections::HashMap;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    visit_mut::{self, VisitMut},
    Pat, PatIdent,
};

struct CharRewriter {
    /// Map from char to the constant declaration for that char
    char_const_decls: HashMap<char, syn::ItemConst>,
}

impl CharRewriter {
    fn new() -> Self {
        Self { char_const_decls: HashMap::new() }
    }
}

impl VisitMut for CharRewriter {
    fn visit_pat_mut(&mut self, pat: &mut Pat) {
        if let Pat::Lit(lit) = &pat {
            if let syn::Lit::Char(lit_char) = &lit.lit {
                let char = lit_char.value();
                let const_ident = format_ident!("CONST___{}", char as u32);

                // Pull a constant declaration if one doesn't already exist
                if !self.char_const_decls.contains_key(&char) {
                    let const_decl: syn::ItemConst = syn::parse2(quote! {
                        const #const_ident: u32 = #lit_char as u32;
                    })
                    .unwrap();
                    self.char_const_decls.insert(char, const_decl);
                }

                // Replace the char lit pattern with a reference to the constant
                let pat_ident = PatIdent {
                    attrs: vec![],
                    by_ref: None,
                    mutability: None,
                    ident: const_ident,
                    subpat: None,
                };

                *pat = Pat::Ident(pat_ident);
            }
        }

        visit_mut::visit_pat_mut(self, pat);
    }
}

/// A macro that allows a match expression on a u32 value to match against char literal patterns.
/// This is useful for matching a generic u32 code point value against u32's represented as char
/// literals. Normally there is no way to match a non-char value againt char literal patterns.
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
    let mut match_ast: syn::ExprMatch = syn::parse(item).unwrap();

    let mut char_rewriter = CharRewriter::new();

    // Rewrite all char literal patterns to use a constant instead
    for arm in &mut match_ast.arms {
        char_rewriter.visit_pat_mut(&mut arm.pat);
    }

    // Write back all constants and the match expression
    let char_const_decls = char_rewriter.char_const_decls.values();
    proc_macro::TokenStream::from(quote! {
        {
            #(#char_const_decls)*
            #match_ast
        }
    })
}
