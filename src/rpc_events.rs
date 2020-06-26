use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;

use syn::{parse_macro_input, parse_quote, ItemEnum, Fields};
use quote::{quote, format_ident, ToTokens};

pub fn rpc_events(_: TokenStream, item: TokenStream) -> TokenStream {
    let mut item = parse_macro_input!(item as ItemEnum);
    let name = &item.ident;
    let untuple1_path = quote!(#name::__untuple1).to_string();
    let untuple0_path = quote!(#name::__untuple0).to_string();

    let untuple_impl = quote! {
        impl #name {
            fn __untuple1<'de, D, T>(de: D) -> ::core::result::Result<T, D::Error>
            where
                D: ::serde::Deserializer<'de>,
                T: ::serde::Deserialize<'de>,
            {
                <[T; 1]>::deserialize(de).map(|[v]| v)
            }

            fn __untuple0<'de, D>(de: D) -> ::core::result::Result<(), D::Error>
            where
                D: ::serde::Deserializer<'de>,
            {
                <[(); 0]>::deserialize(de).map(|_| ())
            }
        }
    };

    for v in item.variants.iter_mut() {
        if v.attrs.is_empty() {
            let renamed = format_ident!("{}Event", v.ident).to_string();
            v.attrs.push(parse_quote!(#[serde(rename = #renamed)]));
        }
        match &mut v.fields {
            Fields::Unit => v.attrs.push(parse_quote!(#[serde(deserialize_with = #untuple0_path)])),
            Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                fields
                    .unnamed
                    .first_mut()
                    .unwrap()
                    .attrs
                    .push(parse_quote!(#[serde(deserialize_with = #untuple1_path)]));
            },
            _ => (),
        }
    }

    let mut stream = TokenStream2::new();
    item.to_tokens(&mut stream);
    untuple_impl.to_tokens(&mut stream);

    stream.into()
}
