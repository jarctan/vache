//! Redefining ranges.
//!
//! The current main reason for re-implemeting ranges is to provide a `Display`
//! implementation for them.

use proc_macro2::TokenStream;

#[allow(clippy::missing_docs_in_private_items)]
pub fn range() -> TokenStream {
    quote!(
        #[derive(Clone)]
        pub struct __Range<T>(::std::ops::Range<T>);

        impl<T> __Range<T> {
            pub const fn new(start: T, end: T) -> Self {
                Self(start..end)
            }
        }

        impl<T: fmt::Display> fmt::Display for __Range<T> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}..{}", self.0.start, self.0.end)
            }
        }

        impl<T: Clone + ::std::iter::Step> IntoIterator for Cow<'_, __Range<T>> {
            type IntoIter = ::std::ops::Range<T>;
            type Item = T;

            fn into_iter(self) -> Self::IntoIter {
                self.into_owned().0
            }
        }
    )
}
