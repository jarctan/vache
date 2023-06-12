//! Disabling annoying warnings/lints and specify some required feature gates.

use proc_macro2::TokenStream;

#[allow(clippy::missing_docs_in_private_items)]
pub fn lints() -> TokenStream {
    quote!(
        #![feature(try_blocks)]
        #![feature(step_trait)]

        #![allow(clippy::needless_late_init)]
        #![allow(unused_mut)]
        #![allow(unused_imports)]
        #![allow(dead_code)]
        #![allow(unused_variables)]
        #![allow(unused_parens)]
    )
}
