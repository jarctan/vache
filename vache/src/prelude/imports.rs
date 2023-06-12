//! All the imports that we need. Mainly traits that need to be in scope.

use proc_macro2::TokenStream;

#[allow(clippy::missing_docs_in_private_items)]
pub fn imports() -> TokenStream {
    quote!(
        use std::borrow::{Borrow, BorrowMut};
        use std::fmt;
        use std::mem::MaybeUninit;
        use std::ops::{Add, Deref, DerefMut, Div, Mul, Rem, Sub};
        use ::num_traits::ToPrimitive as __ToPrimitive;
        use ::anyhow::Context as __AnyhowContext;
    )
}
