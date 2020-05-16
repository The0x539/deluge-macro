use proc_macro::TokenStream;

use syn::{
    parse_macro_input,
    FnArg,
    parse_quote,
    parse::{Parse, ParseStream},
    AttributeArgs,
    Attribute,
    TraitItemMethod,
    Meta,
    punctuated::Pair,
    Visibility,
    Lit,
    Signature,
    NestedMeta,
    ReturnType,
    Block,
    Pat,
    Expr,
};
use quote::{quote, ToTokens};

struct RpcMethod {
    attrs: Vec<Attribute>,
    vis: Visibility,
    sig: Signature,
}

impl Parse for RpcMethod {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            attrs: input.call(Attribute::parse_outer)?,
            vis: input.parse()?,
            sig: input.parse::<TraitItemMethod>()?.sig, // sue me
        })
    }
}

fn make_ret_expr(ret_type: &ReturnType) -> proc_macro2::TokenStream {
    match ret_type {
        ReturnType::Default => quote!(expect_nothing!(val)),
        ReturnType::Type(_, t) => {
            if t == &parse_quote!(()) {
                quote!(expect_val!(val, Value::Null, "None", ()))
            } else if t == &parse_quote!(Dict) {
                quote!(expect_val!(val, Value::Object(m), "a dict", m.into_iter().collect()))
            } else {
                todo!()
            }
        }
    }
}

#[proc_macro_attribute]
pub fn rpc_method(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut rpc_class: Option<String> = None;
    let mut auth_level: i64 = 0;
    for arg in parse_macro_input!(attr as AttributeArgs) {
        match arg {
            NestedMeta::Meta(Meta::NameValue(mnv)) => {
                match mnv.path.get_ident().expect("unexpected path").to_string().as_str() {
                    "class" => rpc_class = match mnv.lit {
                        Lit::Str(s) => Some(s.value()),
                        x => panic!("unexpected class value: {:?}", x),
                    },
                    "auth_level" => auth_level = match mnv.lit {
                        Lit::Int(i) => i.base10_parse().unwrap(),
                        x => panic!("unexpected auth_level value: {:?}", x),
                    },
                    x => panic!("unexepcted attribute arg: {:?}", x),
                }
            },
            x => panic!("unexpected attribute arg: {:?}", x),
        }
    }
    
    let mut method = parse_macro_input!(item as RpcMethod);

    let ret_type = method.sig.output;

    method.sig.output = match &ret_type {
        ReturnType::Default        => parse_quote!(-> Result<()>),
        ReturnType::Type(_, ref t) => parse_quote!(-> Result<#t>),
    };

    let method_name = format!("{}.{}", rpc_class.expect("must specify an rpc class"), method.sig.ident);

    let args: Vec<Expr> = method.sig.inputs
        .pairs()
        .map(|pair| match pair { Pair::Punctuated(arg, _) | Pair::End(arg) => arg })
        .skip(1)
        .map(|arg| match arg { FnArg::Typed(pt) => pt.pat.clone(), _ => unreachable!() })
        .map(|arg| match arg.as_ref().clone() { Pat::Ident(id) => id.ident.clone(), _ => panic!("args must just be names") })
        // okay, now we have Idents
        .map(|ident| parse_quote!(#ident))
        .collect();

    let ret_expr = make_ret_expr(&ret_type);

    let body: Block = parse_quote!({
        assert!(self.auth_level >= #auth_level);
        let val = make_request!(self, #method_name, [#(#args),*]);
        return #ret_expr;
    });

    let mut stream = proc_macro2::TokenStream::new();
    method.attrs.into_iter().for_each(|attr| attr.to_tokens(&mut stream));
    method.vis.to_tokens(&mut stream);
    method.sig.to_tokens(&mut stream);
    body.to_tokens(&mut stream);
    stream.into()
}
