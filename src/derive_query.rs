use proc_macro::TokenStream;

use syn::{
    parse_macro_input,
    ItemStruct,
    NestedMeta,
    AttributeArgs,
    Field,
    parse::{Parse, ParseStream},
    Meta,
    Lit,
};
use quote::{quote, format_ident};

struct AttrArgs(AttributeArgs);

impl Parse for AttrArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut args = AttributeArgs::new();
        while !input.is_empty() {
            args.push(input.parse()?);
        }
        Ok(Self(args))
    }
}

fn get_key(field: &Field) -> String {
    for attr in field.attrs.iter() {
        if let Some(ident) = attr.path.get_ident() {
            if ident.to_string() == "serde" {
                let args: AttrArgs = attr.parse_args().unwrap();
                for arg in args.0.iter() {
                    match arg {
                        NestedMeta::Meta(Meta::NameValue(mnv)) => {
                            if mnv.path.get_ident().unwrap().to_string() == "rename" {
                                if let Lit::Str(s) = &mnv.lit {
                                    return s.value();
                                }
                            }
                        },
                        _ => (),
                    }
                }
            }
        }
    }

    return field.ident.as_ref().expect("fields must be named").to_string();
}

pub fn derive_query(item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as ItemStruct);

    let name = item.ident;

    let idents = item.fields
        .iter()
        .map(|field| field.ident.as_ref().expect("fields must be named"));

    let types = item.fields
        .iter()
        .map(|field| &field.ty);

    let idents2 = idents.clone();
    let idents3 = idents.clone();

    let ser_fields = item.fields.iter().map(get_key);

    let diff_name = format_ident!("__Diff_{}", name);

    let the_impl = quote! {
        #[allow(non_camel_case_types)]
        #[derive(Debug, Clone, Default, PartialEq, ::serde::Deserialize)]
        #[serde(default)]
        struct #diff_name {
            #(#idents: ::core::option::Option<#types>,)*
        }
        impl #diff_name {
            fn realize(self) -> ::core::result::Result<#name, &'static str> {
                Ok(#name {
                    // yeah, I consume the object and don't give it back on error.
                    // kinda bad, I realize. I dunno what to tell you.
                    #(#idents2: self.#idents2.ok_or(stringify!(missing field: #idents2))?,)*
                })
            }
        }
        impl self::Query for #name {
            type Diff = #diff_name;
            fn keys() -> &'static [&'static str] {
                &[#(stringify!(#ser_fields)),*]
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
