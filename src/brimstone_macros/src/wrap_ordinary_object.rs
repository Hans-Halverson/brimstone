use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};

pub fn wrap_ordinary_object(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut impl_ast: syn::ItemImpl = syn::parse(item).unwrap();
    let mut method_names = HashSet::new();

    for item in &impl_ast.items {
        if let syn::ImplItem::Fn(method_ast) = item {
            method_names.insert(method_ast.sig.ident.to_string());
        }
    }

    macro_rules! implement_if_undefined {
        ($method_name:expr, $body:item) => {
            if !method_names.contains($method_name) {
                let method: syn::ImplItemFn = syn::parse2(quote! {
                    $body
                })
                .unwrap();
                impl_ast.items.push(syn::ImplItem::Fn(method));
            }
        };
    }

    implement_if_undefined!(
        "get_own_property",
        fn get_own_property(
            &self,
            cx: Context,
            key: Handle<PropertyKey>,
        ) -> EvalResult<Option<PropertyDescriptor>> {
            self.ordinary_object().get_own_property(cx, key)
        }
    );

    implement_if_undefined!(
        "define_own_property",
        fn define_own_property(
            &mut self,
            cx: Context,
            key: Handle<PropertyKey>,
            desc: PropertyDescriptor,
        ) -> EvalResult<bool> {
            self.ordinary_object().define_own_property(cx, key, desc)
        }
    );

    implement_if_undefined!(
        "has_property",
        fn has_property(&self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool> {
            self.ordinary_object().has_property(cx, key)
        }
    );

    implement_if_undefined!(
        "get",
        fn get(
            &self,
            cx: Context,
            key: Handle<PropertyKey>,
            receiver: Handle<Value>,
        ) -> EvalResult<Handle<Value>> {
            self.ordinary_object().get(cx, key, receiver)
        }
    );

    implement_if_undefined!(
        "set",
        fn set(
            &mut self,
            cx: Context,
            key: Handle<PropertyKey>,
            value: Handle<Value>,
            receiver: Handle<Value>,
        ) -> EvalResult<bool> {
            self.ordinary_object().set(cx, key, value, receiver)
        }
    );

    implement_if_undefined!(
        "delete",
        fn delete(&mut self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool> {
            self.ordinary_object().delete(cx, key)
        }
    );

    implement_if_undefined!(
        "own_property_keys",
        fn own_property_keys(&self, cx: Context) -> EvalResult<Vec<Handle<Value>>> {
            self.ordinary_object().own_property_keys(cx)
        }
    );

    proc_macro::TokenStream::from(impl_ast.into_token_stream())
}
