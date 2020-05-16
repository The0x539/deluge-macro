use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, TokenTree as TokenTree2};

use syn::{
    parse_macro_input,
    FnArg,
    parse_quote,
    parse::{Parse, ParseStream},
    AttributeArgs,
    Attribute,
    TraitItemMethod,
    Meta,
    Visibility,
    TypePath,
    Lit,
    ItemStruct,
    Signature,
    NestedMeta,
    PathArguments,
    AngleBracketedGenericArguments,
    GenericArgument,
    ReturnType,
    Block,
    Pat,
    Expr,
    Ident,
    TypeSlice,
    Type,
};
use quote::{quote, ToTokens};

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

fn make_val_expr(ret_type: &ReturnType, query_type: &Option<&Ident>) -> TokenStream2 {
    match ret_type {
        ReturnType::Default => quote!(expect_nothing!(val)),
        ReturnType::Type(_, ref t) => {
            let mut ty: &Type = t.as_ref();
            let expect = match ty {
                Type::Slice(TypeSlice { elem, .. }) => {
                    ty = elem;
                    quote!(expect_seq)
                },
                Type::Path(TypePath { path, .. })
                  if path.segments.first().map(|s| &s.ident)
                  == Some(&parse_quote!(Option)) => {
                    ty = match path.segments.first().unwrap().arguments {
                        PathArguments::AngleBracketed(ref args) => match args {
                            AngleBracketedGenericArguments { args, .. } => match args.first().unwrap() {
                                GenericArgument::Type(x) => x,
                                _ => panic!(),
                            }
                        },
                        _ => panic!(),
                    };
                    quote!(expect_option)
                },
                _ => quote!(expect_val)
            };

            let expectation = if ty == &parse_quote!(()) {
                quote!(Value::Null, "None", ())
            } else if ty == &parse_quote!(Dict) {
                quote!(Value::Object(m), "a dict", m.into_iter().collect())
            } else if query_type.is_some() && ty == &parse_quote!(#query_type) {
                quote!(m @ Value::Object(_), "torrent status", serde_json::from_value(m).unwrap())
            } else if ty == &parse_quote!(String) {
                quote!(Value::String(s), "a string", s)
            } else if ty == &parse_quote!(InfoHash) {
                quote!(Value::String(s), "an infohash", s)
            } else if ty == &parse_quote!(i64) {
                quote!(Value::Number(num), "a number", match num.as_i64() {
                    Some(n) => n,
                    None => return Err(Error::expected("an i64", Value::Number(num.clone()))),
                })
            } else {
                todo!()
            };

            quote!(#expect!(__response, #expectation))
        }
    }
}

#[proc_macro_attribute]
pub fn rpc_method(attr: TokenStream, item: TokenStream) -> TokenStream {
    let RpcMethod { attrs, vis, mut sig, body } = parse_macro_input!(item as RpcMethod);

    let mut kwargs = Vec::new();

    let mut class = None;
    let mut name: TokenTree2 = sig.ident.clone().into();
    let mut auth_level: i64 = 0;
    for arg in parse_macro_input!(attr as AttributeArgs) {
        match arg {
            NestedMeta::Meta(Meta::NameValue(mnv)) => {
                match mnv.path.get_ident().expect("unexpected path").to_string().as_str() {
                    "class" => class = match mnv.lit {
                        Lit::Str(s) => Some(s.value()),
                        x => panic!("unexpected class value: {:?}", x),
                    },
                    "method" => name = match mnv.lit {
                        Lit::Str(s) => s.parse().unwrap(),
                        x => panic!("unexpected method name value: {:?}", x),
                    },
                    "auth_level" => auth_level = match mnv.lit {
                        Lit::Int(i) => i.base10_parse().unwrap(),
                        x => panic!("unexpected auth_level value: {:?}", x),
                    },
                    // This doesn't seem like the best idea, but whatever.
                    kwkey => {
                        let kwarg = mnv.lit;
                        kwargs.push(quote!(#kwkey => #kwarg));
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

    let ret_type = sig.output;
    sig.output = match ret_type {
        ReturnType::Default => parse_quote!(-> Result<()>),
        ReturnType::Type(_, ref t) => match t.as_ref() {
            Type::Slice(TypeSlice { elem, .. }) => {
                if sig.generics.lt_token.is_none() {
                    sig.generics.lt_token = parse_quote!(<);
                    sig.generics.gt_token = parse_quote!(>);
                }
                sig.generics.params .push(parse_quote!(__I: FromIterator<#elem>));
                parse_quote!(-> Result<__I>)
            }
            _ => parse_quote!(-> Result<#t>)
        },
    };

    let mut query_type = None;

    for type_param in sig.generics.type_params() {
        for bound in &type_param.bounds {
            if bound == &parse_quote!(Query) {
                let ident = &type_param.ident;
                query_type = Some(ident);
                kwargs.push(quote!("keys" => #ident::keys()));
            }
        }
    }

    let val_expr = make_val_expr(&ret_type, &query_type);

    let method_name = format!("{}.{}", class.expect("must specify an RPC class"), name);

    let ret_block = body.unwrap_or(parse_quote!( { return val; } ));

    let body: Block = parse_quote!({
        assert!(self.auth_level >= #auth_level);
        let __response = make_request!(self, #method_name, [#(#args),*], {#(#kwargs),*});
        let val = #val_expr;
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

    let fields = item.fields
        .iter()
        .map(|field| field.ident.as_ref().expect("fields must be named"));

    let the_impl = quote! {
        // TODO: hygiene
        impl session::Query for #name {
            fn keys() -> &'static [&'static str] {
                &[#(stringify!(#fields)),*]
            }
        }
    };

    the_impl.into()
}

#[proc_macro_attribute]
pub fn option_struct(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut item = parse_macro_input!(item as ItemStruct);

    let name = &item.ident;

    for field in &mut item.fields {
        let ty = &field.ty;
        field.ty = parse_quote!(::core::option::Option<#ty>);
    }

    let fields = item.fields
        .iter()
        .map(|field| field.ident.as_ref().expect("fields must be named"));

    let fields2 = fields.clone(); // uh, what?

    let the_impl = quote! {
        impl ::serde::ser::Serialize for #name {
            fn serialize<S: ::serde::ser::Serializer>(&self, serializer: S) -> ::core::result::Result<S::Ok, S::Error> {
                use serde::ser::{Serializer, SerializeMap};
                let mut count = 0;
                #(if self.#fields.is_some() { count += 1; })*

                let mut map = serializer.serialize_map(Some(count))?;
                #(match self.#fields2 {
                    Some(ref v) => map.serialize_entry(stringify!(#fields2), v)?,
                    None => (),
                };)*

                map.end()
            }
        }
    };

    let mut stream = TokenStream2::new();
    item.to_tokens(&mut stream);
    the_impl.to_tokens(&mut stream);

    stream.into()
}
