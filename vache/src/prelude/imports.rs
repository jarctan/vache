//! All the imports that we need. Mainly traits that need to be in scope.

use proc_macro2::TokenStream;

#[allow(clippy::missing_docs_in_private_items)]
pub fn imports() -> TokenStream {
    quote!(
        use ::std::borrow::Borrow as __Borrow;
        use ::std::borrow::BorrowMut as __BorrowMut;
        use ::std::mem::MaybeUninit as __MaybeUninit;
        use ::num_traits::ToPrimitive as __ToPrimitive;
        use ::anyhow::Context as __AnyhowContext;
    )
}
