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
                Err(self::Error::expected("nothing", __response))
            }
        },
        ResponseType::Single(ty) => quote! {
            match __response.len() {
                1 => {
                    let v = __response.into_iter().next().unwrap();
                    Ok(::serde_yaml::from_value::<#ty>(v).unwrap())
                }
                _ => Err(self::Error::expected("a list of length 1", __response))
            }
        },
        ResponseType::Compound(ty) => quote! {
            // TODO: propagate serde errors instead of unwrapping
            self::Result::<#ty>::Ok(::serde_yaml::from_value::<#ty>(::serde_yaml::Value::Sequence(__response)).unwrap())
        }
    }
}

#[proc_macro_attribute]
pub fn rpc_method(attr: TokenStream, item: TokenStream) -> TokenStream {
    let RpcMethod { attrs, vis, mut sig, body } = parse_macro_input!(item as RpcMethod);

    let mut kwkeys = Vec::new();
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
                        kwkeys.push(quote!(#kwkey));
                        kwargs.push(quote!(#kwarg));
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
                kwkeys.push(quote!("keys"));
                kwargs.push(quote!(#ident::keys()));
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
        ReturnType::Default => parse_quote!(-> self::Result<()>),
        ReturnType::Type(_, t) => parse_quote!(-> self::Result<#t>),
    };

    let method_name = format!("{}.{}", class, name);

    let ret_block = body.unwrap_or(parse_quote!( { return Ok(val); } ));

    let body: Block = parse_quote!({
        assert!(self.auth_level >= self::AuthLevel::#auth_level);
        let __args = ::std::vec![#(::serde_yaml::to_value(#args).unwrap()),*];
        let mut __kwargs = ::std::collections::HashMap::new();
        #(__kwargs.insert(::std::string::String::from(#kwkeys), ::serde_yaml::Value::from(#kwargs));)*
        let __response = self.request(#method_name, __args, __kwargs).await?;
        let val = #val_expr?;
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
        impl self::Query for #name {
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

#[proc_macro_attribute]
pub fn value_enum(attr: TokenStream, item: TokenStream) -> TokenStream {
    let ty: Type = match parse_macro_input!(attr as AttributeArgs).first().expect("must specify a type") {
        NestedMeta::Meta(Meta::Path(p)) => parse_quote!(#p),
        x => panic!("expected a type, got {:?}", x),
    };

    let ty_str: String = ty.to_token_stream().to_string();

    let mut item = parse_macro_input!(item as ItemEnum);

    let name = &item.ident;

    item.attrs.push(parse_quote!(#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize)]));
    item.attrs.push(parse_quote!(#[serde(try_from = #ty_str, into = #ty_str)]));

    let (variants, discriminants) = item.variants
        .iter()
        .map(|v| (&v.ident, &v.discriminant.as_ref().expect("every variant must have a discriminant").1))
        .unzip::<_, _, Vec<&Ident>, Vec<&Expr>>();

    let error_fmt = format!("Invalid {} value: {{:?}}", name);

    let the_impl = quote! {
        impl ::core::convert::TryFrom<#ty> for #name {
            type Error = ::std::string::String;
            fn try_from(value: #ty) -> ::core::result::Result<Self, Self::Error> {
                match value {
                    #(#discriminants => Ok(Self::#variants),)*
                    _ => Err(format!(#error_fmt, value)),
                }
            }
        }
        impl ::core::convert::Into<#ty> for #name { fn into(self) -> #ty { self as #ty } }
        impl ::core::fmt::Display for #name {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Debug::fmt(self, f)
            }
        }
    };

    let mut stream = TokenStream2::new();
    item.to_tokens(&mut stream);
    the_impl.to_tokens(&mut stream);

    stream.into()
}
