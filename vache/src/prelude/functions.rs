//! Builtin functions (although they go by different names in the actual,
//! original Vache source code).

use proc_macro2::TokenStream;

#[allow(clippy::missing_docs_in_private_items)]
pub fn functions() -> TokenStream {
    quote!(
        // Redefine partial equality, so they can be applied to any type and possibly
        // implemented by the user
        pub(crate) trait __PartialEq<Rhs = Self>
        where
            Self: Clone,
        {
            fn eq<'a, 'b>(x: Var<'a, 'b, Self>, y: Var<'a, 'b, Self>) -> __Result<Cow<'b, bool>>;
            fn ne<'a, 'b>(x: Var<'a, 'b, Self>, y: Var<'a, 'b, Self>) -> __Result<Cow<'b, bool>> {
                let is_eq = __PartialEq::eq(x, y)?;
                __not(Var::Owned(is_eq))
            }
        }

        impl<B: ::std::cmp::PartialEq + Clone> __PartialEq<B> for B {
            fn eq<'a, 'b>(x: Var<'a, 'b, B>, y: Var<'a, 'b, B>) -> __Result<Cow<'b, bool>> {
                let b1: &B = x.borrow();
                let b2: &B = y.borrow();
                Ok(Cow::Owned(b1 == b2))
            }

            fn ne<'a, 'b>(x: Var<'a, 'b, B>, y: Var<'a, 'b, B>) -> __Result<Cow<'b, bool>> {
                let b1: &B = x.borrow();
                let b2: &B = y.borrow();
                Ok(Cow::Owned(b1 != b2))
            }
        }

        /// Prelude function.
        pub(crate) fn __le<'a, 'b, B: PartialOrd + Clone>(
            x: Var<'a, 'b, B>,
            y: Var<'a, 'b, B>,
        ) -> __Result<Cow<'b, bool>> {
            let b1: &B = x.borrow();
            let b2: &B = y.borrow();
            Ok(Cow::Owned(b1 <= b2))
        }
        /// Prelude function.
        pub(crate) fn __lt<'a, 'b, B: PartialOrd + Clone>(
            x: Var<'a, 'b, B>,
            y: Var<'a, 'b, B>,
        ) -> __Result<Cow<'b, bool>> {
            let b1: &B = x.borrow();
            let b2: &B = y.borrow();
            Ok(Cow::Owned(b1 < b2))
        }
        /// Prelude function.
        pub(crate) fn __ge<'a, 'b, B: PartialOrd + Clone>(
            x: Var<'a, 'b, B>,
            y: Var<'a, 'b, B>,
        ) -> __Result<Cow<'b, bool>> {
            let b1: &B = x.borrow();
            let b2: &B = y.borrow();
            Ok(Cow::Owned(b1 >= b2))
        }
        /// Prelude function.
        pub(crate) fn __gt<'a, 'b, B: PartialOrd + Clone>(
            x: Var<'a, 'b, B>,
            y: Var<'a, 'b, B>,
        ) -> __Result<Cow<'b, bool>> {
            let b1: &B = x.borrow();
            let b2: &B = y.borrow();
            Ok(Cow::Owned(b1 > b2))
        }
        /// Prelude function.
        pub(crate) fn __add<'a, 'b, C: Clone, B: ::std::ops::Add<Output = C> + Clone>(
            x: Var<'a, 'b, B>,
            y: Var<'a, 'b, B>,
        ) -> __Result<Cow<'b, C>> {
            Ok(Cow::Owned(Var::into_owned(x) + Var::into_owned(y)))
        }
        /// Prelude function.
        pub(crate) fn __sub<'a, 'b, C: Clone, B: ::std::ops::Sub<Output = C> + Clone>(
            x: Var<'a, 'b, B>,
            y: Var<'a, 'b, B>,
        ) -> __Result<Cow<'b, C>> {
            Ok(Cow::Owned(Var::into_owned(x) - Var::into_owned(y)))
        }
        /// Prelude function.
        pub(crate) fn __mul<'a, 'b, C: Clone, B: ::std::ops::Mul<Output = C> + Clone>(
            x: Var<'a, 'b, B>,
            y: Var<'a, 'b, B>,
        ) -> __Result<Cow<'b, C>> {
            Ok(Cow::Owned(Var::into_owned(x) * Var::into_owned(y)))
        }
        /// Prelude function.
        pub(crate) fn __div<'a, 'b, C: Clone, B: ::std::ops::Div<Output = C> + Clone>(
            x: Var<'a, 'b, B>,
            y: Var<'a, 'b, B>,
        ) -> __Result<Cow<'b, C>> {
            Ok(Cow::Owned(Var::into_owned(x) / Var::into_owned(y)))
        }
        /// Prelude function.
        pub(crate) fn __rem<'a, 'b, C: Clone, B: ::std::ops::Rem<Output = C> + Clone>(
            x: Var<'a, 'b, B>,
            y: Var<'a, 'b, B>,
        ) -> __Result<Cow<'b, C>> {
            Ok(Cow::Owned(Var::into_owned(x) % Var::into_owned(y)))
        }
        /// Prelude function.
        pub(crate) fn __or<'a, 'b>(
            x: Var<'a, 'b, bool>,
            y: Var<'a, 'b, bool>,
        ) -> __Result<Cow<'a, bool>> {
            Ok(Cow::Owned(**x || **y))
        }
        /// Prelude function.
        pub(crate) fn __and<'a, 'b>(
            x: Var<'a, 'b, bool>,
            y: Var<'a, 'b, bool>,
        ) -> __Result<Cow<'a, bool>> {
            Ok(Cow::Owned(**x && **y))
        }
        /// Prelude function.
        pub(crate) fn __not<'a, 'b, C: Clone, B: ::std::ops::Not<Output = C> + Clone>(
            x: Var<'a, 'b, B>,
        ) -> __Result<Cow<'b, C>> {
            let owned: B = Var::into_owned(x);
            let neg: C = !owned;
            Ok(Cow::Owned(neg))
        }

        pub(crate) fn __assert<'a, 'b>(pred: Var<'a, 'b, bool>) -> __Result<Cow<'b, ()>> {
            ::anyhow::ensure!(**pred, "Assertion failed");
            Ok(Cow::owned(()))
        }
    )
}
