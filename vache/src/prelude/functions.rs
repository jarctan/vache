//! Builtin functions (although they go by different names in the actual,
//! original Vache source code).

use proc_macro2::TokenStream;

#[allow(clippy::missing_docs_in_private_items)]
pub fn functions() -> TokenStream {
    quote!(
        /// Prelude function.
        pub(crate) fn __eq<'a, B: PartialEq + Clone>(
            x: Cow<'a, B>,
            y: Cow<'a, B>,
        ) -> __Result<Cow<'a, bool>> {
            let b1: &B = x.borrow();
            let b2: &B = y.borrow();
            Ok(Cow::Owned(b1 == b2))
        }

        /// Prelude function.
        pub(crate) fn __le<'a, B: PartialOrd + Clone>(
            x: Cow<'a, B>,
            y: Cow<'a, B>,
        ) -> __Result<Cow<'a, bool>> {
            let b1: &B = x.borrow();
            let b2: &B = y.borrow();
            Ok(Cow::Owned(b1 <= b2))
        }

        /// Prelude function.
        pub(crate) fn __lt<'a, B: PartialOrd + Clone>(
            x: Cow<'a, B>,
            y: Cow<'a, B>,
        ) -> __Result<Cow<'a, bool>> {
            let b1: &B = x.borrow();
            let b2: &B = y.borrow();
            Ok(Cow::Owned(b1 < b2))
        }

        /// Prelude function.
        pub(crate) fn __ge<'a, B: PartialOrd + Clone>(
            x: Cow<'a, B>,
            y: Cow<'a, B>,
        ) -> __Result<Cow<'a, bool>> {
            let b1: &B = x.borrow();
            let b2: &B = y.borrow();
            Ok(Cow::Owned(b1 >= b2))
        }

        /// Prelude function.
        pub(crate) fn __gt<'a, B: PartialOrd + Clone>(
            x: Cow<'a, B>,
            y: Cow<'a, B>,
        ) -> __Result<Cow<'a, bool>> {
            let b1: &B = x.borrow();
            let b2: &B = y.borrow();
            Ok(Cow::Owned(b1 > b2))
        }

        /// Prelude function.
        pub(crate) fn __add<'a, C: Clone, B: ::std::ops::Add<Output = C> + Clone>(
            x: Cow<B>,
            y: Cow<B>,
        ) -> __Result<Cow<'a, C>> {
            Ok(Cow::Owned(x.into_owned() + y.into_owned()))
        }

        /// Prelude function.
        pub(crate) fn __sub<'a, C: Clone, B: ::std::ops::Sub<Output = C> + Clone>(
            x: Cow<B>,
            y: Cow<B>,
        ) -> __Result<Cow<'a, C>> {
            Ok(Cow::Owned(x.into_owned() - y.into_owned()))
        }

        /// Prelude function.
        pub(crate) fn __mul<'a, C: Clone, B: ::std::ops::Mul<Output = C> + Clone>(
            x: Cow<B>,
            y: Cow<B>,
        ) -> __Result<Cow<'a, C>> {
            Ok(Cow::Owned(x.into_owned() * y.into_owned()))
        }

        /// Prelude function.
        pub(crate) fn __div<'a, C: Clone, B: ::std::ops::Div<Output = C> + Clone>(
            x: Cow<B>,
            y: Cow<B>,
        ) -> __Result<Cow<'a, C>> {
            Ok(Cow::Owned(x.into_owned() / y.into_owned()))
        }

        /// Prelude function.
        pub(crate) fn __rem<'a, C: Clone, B: ::std::ops::Rem<Output = C> + Clone>(
            x: Cow<B>,
            y: Cow<B>,
        ) -> __Result<Cow<'a, C>> {
            Ok(Cow::Owned(x.into_owned() % y.into_owned()))
        }

        /// Prelude function.
        pub(crate) fn __or<'a>(x: Cow<bool>, y: Cow<bool>) -> __Result<Cow<'a, bool>> {
            Ok(Cow::Owned(*x || *y))
        }

        /// Prelude function.
        pub(crate) fn __and<'a>(x: Cow<'a, bool>, y: Cow<'a, bool>) -> __Result<Cow<'a, bool>> {
            Ok(Cow::Owned(*x && *y))
        }

        /// Prelude function.
        pub(crate) fn __not<'a, C: Clone, B: ::std::ops::Not<Output = C> + Clone>(
            x: Cow<'a, B>,
        ) -> __Result<Cow<'a, C>> {
            Ok(Cow::Owned(!x.into_owned()))
        }
    )
}
