//! Redefining strings.
//!
//! The current reason for redefining integers entirely is that we want the
//! debug behavior to be different.

use proc_macro2::TokenStream;

#[allow(clippy::missing_docs_in_private_items)]
pub fn string() -> TokenStream {
    quote!(
        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
        pub struct __String(::std::string::String);

        impl ::std::fmt::Display for __String {
            #[inline(always)]
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }

        impl ::std::fmt::Debug for __String {
            #[inline(always)]
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }

        impl<'a> From<&'a str> for __String {
            fn from(s: &str) -> Self {
                Self(::std::string::String::from(s))
            }
        }
    )
}
