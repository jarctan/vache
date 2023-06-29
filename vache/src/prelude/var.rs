//! Defining `Var`, which is a layer of indirection over variables.

use proc_macro2::TokenStream;

#[allow(clippy::missing_docs_in_private_items)]
pub fn var() -> TokenStream {
    quote!(
        pub enum Var<'a, 'b, B>
        where
            B: 'b + Clone,
        {
            Mut(&'a mut Cow<'b, B>),
            Owned(Cow<'b, B>),
        }
        impl<B: Clone> ::std::fmt::Debug for Var<'_, '_, B>
        where
            B: ::std::fmt::Debug,
        {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                match *self {
                    Var::Mut(ref b) => write!(f, "&mut {:?}", b),
                    Var::Owned(ref o) => write!(f, "{:?}", o),
                }
            }
        }
        impl<B: Clone> ::std::fmt::Display for Var<'_, '_, B>
        where
            B: ::std::fmt::Display,
        {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                match *self {
                    Var::Mut(ref b) => ::std::fmt::Display::fmt(b, f),
                    Var::Owned(ref o) => ::std::fmt::Display::fmt(o, f),
                }
            }
        }
        impl<'a, 'b, B: 'a + Clone> Clone for Var<'a, 'b, B> {
            fn clone(&self) -> Self {
                match self {
                    Var::Mut(b) => Var::Owned(Cow::clone(b)),
                    Var::Owned(ref b) => Var::Owned(Cow::clone(b)),
                }
            }
        }
        impl<'a, 'b, B: Clone> ::std::ops::Deref for Var<'a, 'b, B> {
            type Target = Cow<'b, B>;

            fn deref(&self) -> &Cow<'b, B> {
                match self {
                    Var::Mut(borrowed) => borrowed,
                    Var::Owned(ref owned) => owned,
                }
            }
        }
        impl<'a, 'b, B: Clone> ::std::ops::DerefMut for Var<'a, 'b, B> {
            fn deref_mut(&mut self) -> &mut Cow<'b, B> {
                match self {
                    Var::Mut(borrowed) => borrowed,
                    Var::Owned(ref mut owned) => owned,
                }
            }
        }
        impl<'a, 'b, 'c, 'd, Rhs: Clone, T: ::std::cmp::PartialEq<Rhs> + Clone>
            ::std::cmp::PartialEq<Var<'a, 'b, Rhs>> for Var<'c, 'd, T>
        {
            fn eq(&self, other: &Var<'a, 'b, Rhs>) -> bool {
                <T as ::std::cmp::PartialEq<Rhs>>::eq(&**self, &**other)
            }
        }
        impl<'a, 'b, 'c, 'd, Rhs: Clone, T: ::std::cmp::PartialOrd<Rhs> + Clone>
            ::std::cmp::PartialOrd<Var<'a, 'b, Rhs>> for Var<'c, 'd, T>
        {
            fn partial_cmp(&self, other: &Var<'a, 'b, Rhs>) -> Option<::std::cmp::Ordering> {
                <T as ::std::cmp::PartialOrd<Rhs>>::partial_cmp(&**self, &**other)
            }
        }
        impl<T: ::std::iter::Step + std::cmp::PartialOrd> ::std::iter::Step for Var<'_, '_, T> {
            fn steps_between(start: &Self, end: &Self) -> Option<usize> {
                <T as ::std::iter::Step>::steps_between(&**start, &**end)
            }

            fn forward_checked(mut start: Self, count: usize) -> Option<Self> {
                Some(Var::Owned(<Cow<_> as ::std::iter::Step>::forward_checked(
                    Cow::__take(&mut start),
                    count,
                )?))
            }

            fn backward_checked(mut start: Self, count: usize) -> Option<Self> {
                Some(Var::Owned(<Cow<_> as ::std::iter::Step>::backward_checked(
                    Cow::__take(&mut start),
                    count,
                )?))
            }
        }
        impl<'a, 'b, 'c, 'd, B: Clone> Var<'a, 'b, __Vec<Var<'c, 'd, B>>> {
            pub fn remove(&mut self, index: usize) -> __Result<Var<'c, 'd, B>> {
                Cow::__take(self).remove(index)
            }
        }

        impl<'a, 'b, B: Clone> Var<'a, 'b, B> {
            pub fn into_owned(self) -> B {
                match self {
                    Var::Mut(borrowed) => Cow::into_owned(Cow::clone(borrowed)),
                    Var::Owned(owned) => Cow::into_owned(owned),
                }
            }

            pub fn to_cow(self) -> Cow<'b, B> {
                match self {
                    Var::Mut(borrowed) => Cow::clone(borrowed),
                    Var::Owned(owned) => owned,
                }
            }

            pub fn owned(b: B) -> Self {
                Var::Owned(Cow::Owned(b))
            }

            pub fn __take(&mut self) -> Self {
                let taken = match self {
                    Var::Mut(borrowed) => Cow::__take(borrowed),
                    Var::Owned(owned) => Cow::__take(owned),
                };
                Var::Owned(taken)
            }
        }
        /// Prelude function.
        #[allow(clippy::ptr_arg)]
        pub(crate) fn __ref_mut<'d, 'a: 'd, 'b: 'd, 'c: 'd, B: Clone>(
            var: &'c mut Var<'a, 'b, B>,
        ) -> Var<'d, 'b, B> {
            match var {
                Var::Mut(b) => Var::Mut(b),
                Var::Owned(ref mut o) => Var::Mut(o),
            }
        }
        /// Prelude function.
        #[allow(clippy::ptr_arg)]
        pub(crate) fn __ref<'d, 'e, 'a: 'd + 'e, 'b: 'd + 'e, 'c: 'd + 'e, B: Clone>(
            var: &'c Var<'a, 'b, B>,
        ) -> Var<'d, 'e, B> {
            match var {
                Var::Mut(b) => Var::Owned(__borrow(b)),
                Var::Owned(ref o) => Var::Owned(__borrow(o)),
            }
        }

        impl<'a, 'b, B: Clone + ::std::ops::Not> ::std::ops::Not for Var<'a, 'b, B>
        where
            <B as ::std::ops::Not>::Output: Clone,
        {
            type Output = Var<'a, 'b, <B as ::std::ops::Not>::Output>;

            // Required method
            fn not(self) -> <Self as ::std::ops::Not>::Output {
                let owned: B = Var::into_owned(self);
                Var::owned(!owned)
            }
        }
    )
}
