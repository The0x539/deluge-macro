use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, TokenTree as TokenTree2};

use syn::{
    parse_macro_input,
    FnArg,
    parse_quote,
    parse::{Parse, ParseStream},
    ItemEnum,
    AttributeArgs,
    Attribute,
    TraitItemMethod,
    Meta,
    Visibility,
    Lit,
    ItemStruct,
    Signature,
    NestedMeta,
    ReturnType,
    Block,
    Pat,
    Expr,
    Ident,
    Type,
    Fields,
};
use quote::{quote, format_ident, ToTokens};

struct RpcMethod {
    attrs: Vec<Attribute>,
    vis: Visibility,
    sig: Signature,
    body: Option<Block>,
}

impl Parse for RpcMethod {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis = input.parse()?;
        let method: TraitItemMethod = input.parse()?;
        Ok(Self { attrs, vis, sig: method.sig, body: method.default })
    }
}

#[derive(Debug)]
enum ResponseType<'a> {
    Nothing,
    Single(&'a Box<Type>),
    Compound(&'a Box<Type>),
}

#[proc_macro_attribute]
pub fn rpc_method(attr: TokenStream, item: TokenStream) -> TokenStream {
    let RpcMethod { attrs, vis, mut sig, body } = parse_macro_input!(item as RpcMethod);

    let mut kwargs = std::collections::HashMap::<Expr, _>::new();

    let mut class = String::from("core");
    let mut name: TokenTree2 = sig.ident.clone().into();
    let mut auth_level = quote!(default());
    for arg in parse_macro_input!(attr as AttributeArgs) {
        match arg {
            NestedMeta::Meta(Meta::NameValue(mnv)) => {
                match mnv.path.get_ident().expect("unexpected path").to_string().as_str() {
                    "class" => class = match mnv.lit {
                        Lit::Str(s) => s.value(),
                        x => panic!("unexpected class value: {:?}", x),
                    },
                    "method" => name = match mnv.lit {
                        Lit::Str(s) => s.parse().unwrap(),
                        x => panic!("unexpected method name value: {:?}", x),
                    },
                    "auth_level" => auth_level = match mnv.lit {
                        Lit::Str(s) => { let s: Ident = s.parse().unwrap(); quote!(#s) }
                        x => panic!("unexpected auth_level value: {:?}", x),
                    },
                    // This doesn't seem like the best idea, but whatever.
                    kwkey => {
                        let kwarg = mnv.lit;
                        kwargs.insert(parse_quote!(#kwkey), quote!(#kwarg));
                    }
                }
            },
            x => panic!("unexpected attribute arg: {:?}", x),
        }
    }

    let args: Vec<Expr> = sig.inputs
        .iter()
        .skip(1)
        .map(|arg| match arg { FnArg::Typed(pt) => pt.pat.clone(), _ => unreachable!() })
        .map(|arg| match arg.as_ref().clone() { Pat::Ident(id) => id.ident.clone(), _ => panic!("args must just be names") })
        // okay, now we have Idents
        .map(|ident| parse_quote!(#ident))
        .collect();

    for type_param in sig.generics.type_params() {
        for bound in &type_param.bounds {
            if bound == &parse_quote!(Query) {
                let ident = &type_param.ident;
                kwargs.insert(parse_quote!("keys"), quote!(#ident::keys()));
            }
        }
    }

    let response_type = match &sig.output {
        ReturnType::Default => ResponseType::Nothing,
        ReturnType::Type(_, ty) => match ty.as_ref() {
            Type::Tuple(t) if !t.elems.is_empty() => ResponseType::Compound(ty),
            Type::Path(t) if t.path.segments.last().unwrap().ident.to_string() == "Vec" => ResponseType::Compound(ty),
            _ => ResponseType::Single(ty)
        },
    };

    let (request_pat, request_type) = match response_type {
        ResponseType::Nothing => (quote!([]), quote!([(); 0])),
        ResponseType::Single(ty) => (quote!([val]), quote!([#ty; 1])),
        ResponseType::Compound(ty) => (quote!(val), quote!(#ty)),
    };

    let nothing_val_stmt = match response_type {
        ResponseType::Nothing => quote!(let val = ();),
        _ => quote!(),
    };

    sig.output = match sig.output {
        ReturnType::Default => parse_quote!(-> self::Result<()>),
        ReturnType::Type(_, t) => parse_quote!(-> self::Result<#t>),
    };

    let method_name = format!("{}.{}", class, name);

    let ret_block = body.unwrap_or(parse_quote!( { return Ok(val); } ));

    let args_expr = match args.len() {
        0 => quote!([] as [(); 0]),
        _ => quote!((#(#args,)*)),
    };
    let kwargs_expr = match kwargs.len() {
        0 => quote!(::std::collections::HashMap::<(), ()>::new()),
        _ => {
            let (kwkeys, kwargs) = kwargs.into_iter().unzip::<_, _, Vec<_>, Vec<_>>();
            quote!({
                let mut map = ::std::collections::HashMap::new();
                #(map.insert(#kwkeys, #kwargs);)*
                map
            })
        },
    };

    let body: Block = parse_quote!({
        assert!(self.auth_level >= self::AuthLevel::#auth_level);
        let #request_pat = self.request::<#request_type, _, _>(#method_name, #args_expr, #kwargs_expr).await?;
        #nothing_val_stmt
        #ret_block
    });

    let mut stream = TokenStream2::new();
    attrs.into_iter().for_each(|attr| attr.to_tokens(&mut stream));
    vis.to_tokens(&mut stream);
    sig.to_tokens(&mut stream);
    body.to_tokens(&mut stream);

    stream.into()
}

#[proc_macro_derive(Query)]
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

#[proc_macro_attribute]
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
