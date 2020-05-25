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
            fn __untuple1<'de, D: ::serde::Deserializer<'de>, T: ::serde::Deserialize<'de>>(
                de: D,
            ) -> ::core::result::Result<T, D::Error> {
                struct UntupleVisitor<'de, T: ::serde::Deserialize<'de>>(::core::marker::PhantomData<(T, &'de ())>);
                impl<'de, T: ::serde::Deserialize<'de>> ::serde::de::Visitor<'de> for UntupleVisitor<'de, T> {
                    type Value = T;
                    fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                        f.write_str("a sequence containing a single value")
                    }
                    fn visit_seq<A: ::serde::de::SeqAccess<'de>>(self, mut seq: A) -> ::core::result::Result<T, A::Error> {
                        seq.next_element()?.ok_or(::serde::de::Error::invalid_length(0, &"1"))
                    }
                }
                de.deserialize_tuple(1, UntupleVisitor(::core::marker::PhantomData::default()))
            }

            fn __untuple0<'de, D: ::serde::Deserializer<'de>>(de: D) -> ::core::result::Result<(), D::Error> {
                struct UnitVisitor;
                impl<'de> ::serde::de::Visitor<'de> for UnitVisitor {
                    type Value = ();
                    fn expecting(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                        f.write_str("an empty list")
                    }
                    fn visit_seq<A: ::serde::de::SeqAccess<'de>>(self, mut seq: A) -> ::core::result::Result<(), A::Error> {
                        match seq.size_hint() {
                            Some(0) => Ok(()),
                            Some(n) => Err(::serde::de::Error::invalid_length(n, &self)),
                            None => match seq.next_element::<self::Value>()? {
                                None => Ok(()),
                                Some(_) => Err(::serde::de::Error::invalid_value(::serde::de::Unexpected::Seq, &self)),
                            }
                        }
                    }
                }
                de.deserialize_tuple(0, UnitVisitor)
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
