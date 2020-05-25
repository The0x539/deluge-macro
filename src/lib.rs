mod rpc_class;
mod derive_query;
mod rpc_events;

use proc_macro::TokenStream;

#[proc_macro]
pub fn rpc_class(item: TokenStream) -> TokenStream {
    rpc_class::rpc_class(item)
}

#[proc_macro_derive(Query)]
pub fn derive_query(item: TokenStream) -> TokenStream {
    derive_query::derive_query(item)
}

#[proc_macro_attribute]
pub fn rpc_events(attr: TokenStream, item: TokenStream) -> TokenStream {
    rpc_events::rpc_events(attr, item)
}
