use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};

/// A macro that allows any object to implement all object methods by using the ordinary object
/// methods for a wrapped ordinary object. Only provides the default implementation for a method
/// if that method is not already defined.
#[proc_macro_attribute]
pub fn wrap_ordinary_object(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut impl_ast: syn::ItemImpl = syn::parse(item).unwrap();
    let mut method_names = HashSet::new();

    for item in &impl_ast.items {
        if let syn::ImplItem::Method(method_ast) = item {
            method_names.insert(method_ast.sig.ident.to_string());
        }
    }

    macro_rules! implement_if_undefined {
        ($method_name:expr, $body:item) => {
            if !method_names.contains($method_name) {
                let method: syn::ImplItemMethod = syn::parse2(quote! {
                    $body
                })
                .unwrap();
                impl_ast.items.push(syn::ImplItem::Method(method));
            }
        };
    }

    implement_if_undefined!(
        "get_prototype_of",
        fn get_prototype_of(&self) -> EvalResult<Option<Gc<ObjectValue>>> {
            self.object().get_prototype_of()
        }
    );

    implement_if_undefined!(
        "set_prototype_of",
        fn set_prototype_of(&mut self, proto: Option<Gc<ObjectValue>>) -> EvalResult<bool> {
            self.object_mut().set_prototype_of(proto)
        }
    );

    implement_if_undefined!(
        "is_extensible",
        fn is_extensible(&self) -> EvalResult<bool> {
            self.object().is_extensible()
        }
    );

    implement_if_undefined!(
        "prevent_extensions",
        fn prevent_extensions(&mut self) -> EvalResult<bool> {
            self.object_mut().prevent_extensions()
        }
    );

    implement_if_undefined!(
        "get_own_property",
        fn get_own_property(&self, key: &PropertyKey) -> EvalResult<Option<PropertyDescriptor>> {
            self.object().get_own_property(key)
        }
    );

    implement_if_undefined!(
        "define_own_property",
        fn define_own_property(
            &mut self,
            cx: &mut Context,
            key: &PropertyKey,
            desc: PropertyDescriptor,
        ) -> EvalResult<bool> {
            self.object_mut().define_own_property(cx, key, desc)
        }
    );

    implement_if_undefined!(
        "has_property",
        fn has_property(&self, key: &PropertyKey) -> EvalResult<bool> {
            self.object().has_property(key)
        }
    );

    implement_if_undefined!(
        "get",
        fn get(&self, cx: &mut Context, key: &PropertyKey, receiver: Value) -> EvalResult<Value> {
            self.object().get(cx, key, receiver)
        }
    );

    implement_if_undefined!(
        "set",
        fn set(
            &mut self,
            cx: &mut Context,
            key: &PropertyKey,
            value: Value,
            receiver: Value,
        ) -> EvalResult<bool> {
            self.object_mut().set(cx, key, value, receiver)
        }
    );

    implement_if_undefined!(
        "delete",
        fn delete(&mut self, key: &PropertyKey) -> EvalResult<bool> {
            self.object_mut().delete(key)
        }
    );

    implement_if_undefined!(
        "own_property_keys",
        fn own_property_keys(&self, cx: &mut Context) -> Vec<Value> {
            self.object().own_property_keys(cx)
        }
    );

    implement_if_undefined!(
        "private_element_find",
        fn private_element_find(
            &mut self,
            private_id: PrivateNameId,
        ) -> Option<&mut PrivateProperty> {
            self.object_mut().private_element_find(private_id)
        }
    );

    implement_if_undefined!(
        "private_field_add",
        fn private_field_add(
            &mut self,
            cx: &mut Context,
            private_id: PrivateNameId,
            value: Value,
        ) -> EvalResult<()> {
            self.object_mut().private_field_add(cx, private_id, value)
        }
    );

    implement_if_undefined!(
        "private_method_or_accessor_add",
        fn private_method_or_accessor_add(
            &mut self,
            cx: &mut Context,
            private_id: PrivateNameId,
            private_method: PrivateProperty,
        ) -> EvalResult<()> {
            self.object_mut()
                .private_method_or_accessor_add(cx, private_id, private_method)
        }
    );

    proc_macro::TokenStream::from(impl_ast.into_token_stream())
}
