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

#[derive(Debug)]
enum ResponseType<'a> {
    Nothing,
    Single(&'a Box<Type>),
    Compound(&'a Box<Type>),
}

fn make_val_expr(response_type: ResponseType) -> TokenStream2 {
    match response_type {
        ResponseType::Nothing => quote! {
            if __response.is_empty() {
                Ok(())
            } else {
                Err(Error::expected("nothing", __response))
            }
        },
        ResponseType::Single(ty) => quote! {
            match __response.len() {
                1 => {
                    let v = __response.into_iter().next().unwrap();
                    Ok(serde_yaml::from_value::<#ty>(v).unwrap())
                }
                _ => Err(Error::expected("a list of length 1", __response))
            }
        },
        ResponseType::Compound(ty) => quote! {
            // TODO: propagate serde errors instead of unwrapping
            Result::<#ty>::Ok(serde_yaml::from_value::<#ty>(serde_yaml::Value::Sequence(__response)).unwrap())
        }
    }
}

#[proc_macro_attribute]
pub fn rpc_method(attr: TokenStream, item: TokenStream) -> TokenStream {
    let RpcMethod { attrs, vis, mut sig, body } = parse_macro_input!(item as RpcMethod);

    let mut kwargs = Vec::new();

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

    for type_param in sig.generics.type_params() {
        for bound in &type_param.bounds {
            if bound == &parse_quote!(Query) {
                let ident = &type_param.ident;
                kwargs.push(quote!("keys" => #ident::keys()));
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

    let val_expr = make_val_expr(response_type);

    sig.output = match sig.output {
        ReturnType::Default => parse_quote!(-> Result<()>),
        ReturnType::Type(_, t) => parse_quote!(-> Result<#t>),
    };

    let method_name = format!("{}.{}", class, name);

    let ret_block = body.unwrap_or(parse_quote!( { return Ok(val); } ));

    let body: Block = parse_quote!({
        assert!(self.auth_level as u8 >= AuthLevel::#auth_level as u8);
        let __response = make_request!(self, #method_name, [#(#args),*], {#(#kwargs),*});
        let val = #val_expr?;
        #ret_block
    });

    let mut stream = TokenStream2::new();
    attrs.into_iter().for_each(|attr| attr.to_tokens(&mut stream));
    vis.to_tokens(&mut stream);
    sig.to_tokens(&mut stream);
    body.to_tokens(&mut stream);

    //println!("{}", stream.to_string());

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
