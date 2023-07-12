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

            #[inline(always)]
            fn deref(&self) -> &Cow<'b, B> {
                match self {
                    Var::Mut(borrowed) => borrowed,
                    Var::Owned(ref owned) => owned,
                }
            }
        }
        impl<'a, 'b, B: Clone> ::std::ops::DerefMut for Var<'a, 'b, B> {
            #[inline(always)]
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
            #[inline(always)]
            fn eq(&self, other: &Var<'a, 'b, Rhs>) -> bool {
                <T as ::std::cmp::PartialEq<Rhs>>::eq(&**self, &**other)
            }
        }
        impl<'a, 'b, 'c, 'd, Rhs: Clone, T: ::std::cmp::PartialOrd<Rhs> + Clone>
            ::std::cmp::PartialOrd<Var<'a, 'b, Rhs>> for Var<'c, 'd, T>
        {
            #[inline(always)]
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
                    Cow::take(&mut start),
                    count,
                )?))
            }

            fn backward_checked(mut start: Self, count: usize) -> Option<Self> {
                Some(Var::Owned(<Cow<_> as ::std::iter::Step>::backward_checked(
                    Cow::take(&mut start),
                    count,
                )?))
            }
        }

        impl<'a, 'b, B: Clone> Var<'a, 'b, B> {
            #[inline(always)]
            pub fn try_into_owned(self) -> Result<B, Self> {
                match self {
                    Var::Mut(_) => Err(self),
                    Var::Owned(owned) => match Cow::try_into_owned(owned) {
                        Ok(owned) => Ok(owned),
                        Err(owned) => Err(Var::Owned(owned)),
                    },
                }
            }

            #[inline(always)]
            pub fn into_owned(self) -> B {
                match self {
                    Var::Mut(borrowed) => Cow::into_owned(Cow::clone(borrowed)),
                    Var::Owned(owned) => Cow::into_owned(owned),
                }
            }

            #[inline(always)]
            pub fn to_cow(self) -> Cow<'b, B> {
                match self {
                    Var::Mut(borrowed) => Cow::clone(borrowed),
                    Var::Owned(owned) => owned,
                }
            }

            #[inline(always)]
            pub fn as_cow(&mut self) -> &mut Cow<'b, B> {
                match self {
                    Var::Mut(borrowed) => borrowed,
                    Var::Owned(ref mut owned) => owned,
                }
            }

            #[inline(always)]
            pub fn try_to_cow(self) -> Result<Cow<'b, B>, Self> {
                match self {
                    Var::Mut(_) => Err(self),
                    Var::Owned(owned) => Ok(owned),
                }
            }

            #[inline(always)]
            pub const fn owned(b: B) -> Self {
                Var::Owned(Cow::Owned(b))
            }

            #[inline(always)]
            pub fn take(&mut self) -> Self {
                let taken = match self {
                    Var::Mut(borrowed) => Cow::take(borrowed),
                    Var::Owned(owned) => Cow::take(owned),
                };
                Var::Owned(taken)
            }

            #[inline(always)]
            pub(crate) const fn borrow_mut<'c>(&'c mut self) -> Var<'c, 'b, B>
            where
                'a: 'c, // The original mutation lifetime must outlive the new mutation lifetime.
                'b: 'c, // The underlying value must outlive the mutation lifetime `'c`.
            {
                match self {
                    Var::Mut(b) => Var::Mut(b),
                    Var::Owned(ref mut o) => Var::Mut(o),
                }
            }

            #[inline(always)]
            pub(crate) const fn borrow<'c>(&'c self) -> Var<'c, 'c, B>
            where
                'a: 'c,
            {
                match self {
                    Var::Mut(b) => {
                        let borrow: Cow<'c, B> = Cow::borrow(b);
                        Var::Owned(borrow)
                    }
                    Var::Owned(ref o) => Var::Owned(Cow::borrow(o)),
                }
            }
        }

        impl<'a, 'b, B: Clone + ::std::ops::Not> ::std::ops::Not for Var<'a, 'b, B>
        where
            <B as ::std::ops::Not>::Output: Clone,
        {
            type Output = Var<'a, 'b, <B as ::std::ops::Not>::Output>;

            #[inline(always)]
            fn not(self) -> <Self as ::std::ops::Not>::Output {
                let owned: B = Var::into_owned(self);
                Var::owned(!owned)
            }
        }
    )
}
