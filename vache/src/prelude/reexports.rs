//! Re-exports of existing Rust types, so that they can be used in the code
//! transparently.

use proc_macro2::TokenStream;

#[allow(clippy::missing_docs_in_private_items)]
pub fn reexports() -> TokenStream {
    quote!(
        use ::anyhow::Result as __Result;
        pub type __String = String;
    )
}
