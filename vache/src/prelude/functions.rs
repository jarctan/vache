//! Builtin functions (although they go by different names in the actual,
//! original Vache source code).

use proc_macro2::TokenStream;

#[allow(clippy::missing_docs_in_private_items)]
pub fn functions() -> TokenStream {
    quote!(
        /// Prelude function.
        pub(crate) fn __eq<B: PartialEq + Clone>(x: Cow<B>, y: Cow<B>) -> __Result<bool> {
            let b1: &B = x.borrow();
            let b2: &B = y.borrow();
            Ok(b1 == b2)
        }

        /// Prelude function.
        pub(crate) fn __le<B: PartialOrd + Clone>(x: Cow<B>, y: Cow<B>) -> __Result<bool> {
            let b1: &B = x.borrow();
            let b2: &B = y.borrow();
            Ok(b1 <= b2)
        }

        /// Prelude function.
        pub(crate) fn __lt<B: PartialOrd + Clone>(x: Cow<B>, y: Cow<B>) -> __Result<bool> {
            let b1: &B = x.borrow();
            let b2: &B = y.borrow();
            Ok(b1 < b2)
        }

        /// Prelude function.
        pub(crate) fn __ge<B: PartialOrd + Clone>(x: Cow<B>, y: Cow<B>) -> __Result<bool> {
            let b1: &B = x.borrow();
            let b2: &B = y.borrow();
            Ok(b1 >= b2)
        }

        /// Prelude function.
        pub(crate) fn __gt<B: PartialOrd + Clone>(x: Cow<B>, y: Cow<B>) -> __Result<bool> {
            let b1: &B = x.borrow();
            let b2: &B = y.borrow();
            Ok(b1 > b2)
        }

        /// Prelude function.
        pub(crate) fn __add<'a, B: Add<Output = B> + Clone>(
            x: Cow<B>,
            y: Cow<B>,
        ) -> __Result<Cow<'a, B>> {
            Ok(Cow::Owned(x.into_owned() + y.into_owned()))
        }

        /// Prelude function.
        pub(crate) fn __sub<'a, B: Sub<Output = B> + Clone>(
            x: Cow<B>,
            y: Cow<B>,
        ) -> __Result<Cow<'a, B>> {
            Ok(Cow::Owned(x.into_owned() - y.into_owned()))
        }

        /// Prelude function.
        pub(crate) fn __mul<'a, B: Mul<Output = B> + Clone>(
            x: Cow<B>,
            y: Cow<B>,
        ) -> __Result<Cow<'a, B>> {
            Ok(Cow::Owned(x.into_owned() * y.into_owned()))
        }

        /// Prelude function.
        pub(crate) fn __div<'a, B: Div<Output = B> + Clone>(
            x: Cow<B>,
            y: Cow<B>,
        ) -> __Result<Cow<'a, B>> {
            Ok(Cow::Owned(x.into_owned() / y.into_owned()))
        }

        /// Prelude function.
        pub(crate) fn __rem<'a, B: Rem<Output = B> + Clone>(
            x: Cow<B>,
            y: Cow<B>,
        ) -> __Result<Cow<'a, B>> {
            Ok(Cow::Owned(x.into_owned() % y.into_owned()))
        }
    )
}
