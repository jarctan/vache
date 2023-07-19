//! Builtin functions (although they go by different names in the actual,
//! original Vache source code).

use proc_macro2::TokenStream;

#[allow(clippy::missing_docs_in_private_items)]
pub fn functions() -> TokenStream {
    quote!(
        struct __noRet {}
        // Redefine partial equality, so they can be applied to any type and possibly
        // implemented by the user
        pub(crate) trait __PartialEq<Rhs = Self>
        where
            Self: Clone,
        {
            fn eq<'b>(
                x: Cow<'b, Self>,
                y: Cow<'b, Self>,
            ) -> __Result<__Ret<Cow<'b, bool>, __noRet>>;
            fn ne<'b>(
                x: Cow<'b, Self>,
                y: Cow<'b, Self>,
            ) -> __Result<__Ret<Cow<'b, bool>, __noRet>> {
                let is_eq = __PartialEq::eq(x, y)?.0;
                __Ret::ok(__not(is_eq)?.0, __noRet {})
            }
        }

        impl<B: ::std::cmp::PartialEq + Clone> __PartialEq<B> for B {
            fn eq<'b>(x: Cow<'b, B>, y: Cow<'b, B>) -> __Result<__Ret<Cow<'b, bool>, __noRet>> {
                let b1: &B = &*x;
                let b2: &B = &*y;
                __Ret::ok(Cow::Owned(b1 == b2), __noRet {})
            }

            fn ne<'b>(x: Cow<'b, B>, y: Cow<'b, B>) -> __Result<__Ret<Cow<'b, bool>, __noRet>> {
                let b1: &B = &*x;
                let b2: &B = &*y;
                __Ret::ok(Cow::Owned(b1 != b2), __noRet {})
            }
        }

        pub(crate) trait __Add<Rhs = Self>
        where
            Self: Clone,
        {
            type Output: ::std::clone::Clone;

            fn add<'b>(
                x: Cow<'b, Self>,
                y: Cow<'b, Self>,
            ) -> __Result<__Ret<Cow<'b, Self::Output>, __noRet>>;
        }

        pub(crate) trait __Sub<Rhs = Self>
        where
            Self: Clone,
        {
            type Output: ::std::clone::Clone;

            fn sub<'b>(
                x: Cow<'b, Self>,
                y: Cow<'b, Self>,
            ) -> __Result<__Ret<Cow<'b, Self::Output>, __noRet>>;
        }

        pub(crate) trait __Div<Rhs = Self>
        where
            Self: Clone,
        {
            type Output: ::std::clone::Clone;

            fn div<'b>(
                x: Cow<'b, Self>,
                y: Cow<'b, Self>,
            ) -> __Result<__Ret<Cow<'b, Self::Output>, __noRet>>;
        }

        pub(crate) trait __Mul<Rhs = Self>
        where
            Self: Clone,
        {
            type Output: ::std::clone::Clone;

            fn mul<'b>(
                x: Cow<'b, Self>,
                y: Cow<'b, Self>,
            ) -> __Result<__Ret<Cow<'b, Self::Output>, __noRet>>;
        }

        pub(crate) trait __Rem<Rhs = Self>
        where
            Self: Clone,
        {
            type Output: ::std::clone::Clone;

            fn rem<'b>(
                x: Cow<'b, Self>,
                y: Cow<'b, Self>,
            ) -> __Result<__Ret<Cow<'b, Self::Output>, __noRet>>;
        }

        struct ____leRet {}
        pub(crate) fn __le<'b, B: PartialOrd + Clone>(
            x: Cow<'b, B>,
            y: Cow<'b, B>,
        ) -> __Result<__Ret<Cow<'b, bool>, ____leRet>> {
            let b1: &B = &*x;
            let b2: &B = &*y;
            __Ret::ok(Cow::Owned(b1 <= b2), ____leRet {})
        }

        struct ____ltRet {}
        pub(crate) fn __lt<'b, B: PartialOrd + Clone>(
            x: Cow<'b, B>,
            y: Cow<'b, B>,
        ) -> __Result<__Ret<Cow<'b, bool>, ____ltRet>> {
            let b1: &B = &*x;
            let b2: &B = &*y;
            __Ret::ok(Cow::Owned(b1 < b2), ____ltRet {})
        }

        struct ____geRet {}
        pub(crate) fn __ge<'a, 'b, B: PartialOrd + Clone>(
            x: Cow<'b, B>,
            y: Cow<'b, B>,
        ) -> __Result<__Ret<Cow<'b, bool>, ____geRet>> {
            let b1: &B = &*x;
            let b2: &B = &*y;
            __Ret::ok(Cow::Owned(b1 >= b2), ____geRet {})
        }

        struct ____gtRet {}
        pub(crate) fn __gt<'a, 'b, B: PartialOrd + Clone>(
            x: Cow<'b, B>,
            y: Cow<'b, B>,
        ) -> __Result<__Ret<Cow<'b, bool>, ____gtRet>> {
            let b1: &B = &*x;
            let b2: &B = &*y;
            __Ret::ok(Cow::Owned(b1 > b2), ____gtRet {})
        }

        pub(crate) fn __or<'a, 'b>(
            x: Cow<'b, bool>,
            y: Cow<'b, bool>,
        ) -> __Result<__Ret<Cow<'b, bool>, ____gtRet>> {
            __Ret::ok(Cow::Owned(*x || *y), ____gtRet {})
        }

        pub(crate) fn __and<'a, 'b>(
            x: Cow<'b, bool>,
            y: Cow<'b, bool>,
        ) -> __Result<__Ret<Cow<'b, bool>, ____gtRet>> {
            __Ret::ok(Cow::Owned(*x && *y), ____gtRet {})
        }

        pub(crate) fn __not<'a, 'b, C: Clone, B: ::std::ops::Not<Output = C> + Clone>(
            x: Cow<'b, B>,
        ) -> __Result<__Ret<Cow<'b, C>, __noRet>> {
            let owned: B = Cow::into_owned(x);
            let neg: C = !owned;
            __Ret::ok(Cow::Owned(neg), __noRet {})
        }

        pub(crate) fn __assert<'b>(pred: Cow<'b, bool>) -> __Result<__Ret<Cow<'b, ()>, __noRet>> {
            ::anyhow::ensure!(*pred, "Assertion failed");
            __Ret::ok(Cow::Owned(()), __noRet {})
        }
    )
}
