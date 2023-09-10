use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn register_async_fn(_attr: TokenStream, item: TokenStream) -> TokenStream {
    item
}
