use proc_macro::TokenStream;

use syn::{parse_macro_input, ItemStruct};
use quote::{quote, format_ident};

pub fn derive_query(item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as ItemStruct);

    let name = item.ident;

    let idents = item.fields
        .iter()
        .map(|field| field.ident.as_ref().expect("fields must be named"));

    let types = item.fields
        .iter()
        .map(|field| &field.ty);


    let idents2 = idents.clone(); // again with this. I kinda get it, but... c'mon
    let idents3 = idents.clone(); // ...

    let diff_name = format_ident!("__Diff_{}", name);

    let the_impl = quote! {
        #[allow(non_camel_case_types)]
        #[derive(Debug, Default, PartialEq, ::serde::Deserialize)]
        #[serde(default)]
        struct #diff_name {
            #(#idents: ::core::option::Option<#types>,)*
        }
        impl self::Query for #name {
            type Diff = #diff_name;
            fn keys() -> &'static [&'static str] {
                &[#(stringify!(#idents2)),*]
            }
            fn update(&mut self, diff: Self::Diff) -> bool {
                if diff == Self::Diff::default() {
                    return false;
                }
                let mut any_changes = false;
                #(match diff.#idents3 {
                    Some(v) => {
                        // if self.ident == v, this isn't really a change
                        // doesn't seem like a big deal, though
                        any_changes = true;
                        self.#idents3 = v;
                    },
                    None => (),
                };)*
                any_changes
            }
        }
    };

    the_impl.into()
}
