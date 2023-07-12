//! Redefining `Cow` in the way we want it to behave (slightly different from
//! Rust `std`'s one).

use proc_macro2::TokenStream;

#[allow(clippy::missing_docs_in_private_items)]
pub fn cow() -> TokenStream {
    quote!(
        #[derive(Default)]
        pub enum Cow<'a, B>
        where
            B: 'a + Clone,
        {
            Borrowed(&'a B),
            Owned(B),
            #[default]
            Uninit,
        }

        impl<B: Clone> ::std::fmt::Debug for Cow<'_, B>
        where
            B: ::std::fmt::Debug,
        {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                match *self {
                    Cow::Borrowed(ref b) => write!(f, "&{:?}", b),
                    Cow::Owned(ref o) => write!(f, "{:?}", o),
                    Cow::Uninit => unreachable!(),
                }
            }
        }

        impl<B: Clone> ::std::fmt::Display for Cow<'_, B>
        where
            B: ::std::fmt::Display,
        {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                match *self {
                    Cow::Borrowed(ref b) => ::std::fmt::Display::fmt(b, f),
                    Cow::Owned(ref o) => ::std::fmt::Display::fmt(o, f),
                    Cow::Uninit => unreachable!(),
                }
            }
        }

        impl<'a, B: 'a + Clone> Clone for Cow<'a, B> {
            #[inline(always)]
            fn clone(&self) -> Self {
                match self {
                    Cow::Borrowed(b) => Cow::Borrowed(b),
                    Cow::Owned(ref b) => {
                        let b: &B = &*b;
                        Cow::Owned(b.to_owned())
                    }
                    Cow::Uninit => unreachable!(),
                }
            }

            #[inline(always)]
            fn clone_from(&mut self, source: &Self) {
                match (self, source) {
                    (&mut Cow::Owned(ref mut dest), Cow::Owned(ref o)) => (&*o).clone_into(dest),
                    (&mut Cow::Uninit, _) => unreachable!(),
                    (t, s) => *t = s.clone(),
                }
            }
        }

        impl<B: Clone> ::std::ops::Deref for Cow<'_, B> {
            type Target = B;

            #[inline(always)]
            fn deref(&self) -> &B {
                match self {
                    Cow::Borrowed(borrowed) => borrowed,
                    Cow::Owned(ref owned) => &owned,
                    Cow::Uninit => unreachable!(),
                }
            }
        }

        impl<'a, 'b, Rhs: Clone, T: ::std::cmp::PartialEq<Rhs> + Clone>
            ::std::cmp::PartialEq<Cow<'a, Rhs>> for Cow<'b, T>
        {
            #[inline(always)]
            fn eq(&self, other: &Cow<'a, Rhs>) -> bool {
                <T as ::std::cmp::PartialEq<Rhs>>::eq(&**self, &**other)
            }
        }

        impl<'a, 'b, Rhs: Clone, T: ::std::cmp::PartialOrd<Rhs> + Clone>
            ::std::cmp::PartialOrd<Cow<'a, Rhs>> for Cow<'b, T>
        {
            #[inline(always)]
            fn partial_cmp(&self, other: &Cow<'a, Rhs>) -> Option<::std::cmp::Ordering> {
                <T as ::std::cmp::PartialOrd<Rhs>>::partial_cmp(&**self, &**other)
            }
        }

        impl<T: ::std::iter::Step + std::cmp::PartialOrd> ::std::iter::Step for Cow<'_, T> {
            fn steps_between(start: &Self, end: &Self) -> Option<usize> {
                <T as ::std::iter::Step>::steps_between(&**start, &**end)
            }

            fn forward_checked(start: Self, count: usize) -> Option<Self> {
                Some(Cow::Owned(<T as ::std::iter::Step>::forward_checked(
                    start.into_owned(),
                    count,
                )?))
            }

            fn backward_checked(start: Self, count: usize) -> Option<Self> {
                Some(Cow::Owned(<T as ::std::iter::Step>::backward_checked(
                    start.into_owned(),
                    count,
                )?))
            }
        }

        impl<'a, 'b: 'a, B: Clone> Cow<'a, __Vec<'b, B>> {
            #[inline(always)]
            pub fn remove(&mut self, index: usize) -> __Result<Cow<'b, B>> {
                match ::std::mem::take(self) {
                    Cow::Borrowed(b) => Ok(b.get(index)?.clone()),
                    Cow::Owned(mut array) => array.consume(index),
                    Cow::Uninit => unreachable!(),
                }
            }
        }

        impl<B: Clone> ::std::ops::DerefMut for Cow<'_, B> {
            #[inline(always)]
            fn deref_mut(&mut self) -> &mut B {
                match self {
                    Cow::Borrowed(borrowed) => {
                        *self = Cow::Owned(borrowed.clone());
                        match self {
                            Cow::Owned(ref mut owned) => owned,
                            Cow::Borrowed(..) => unreachable!(),
                            Cow::Uninit => unreachable!(),
                        }
                    }
                    Cow::Owned(ref mut owned) => owned,
                    Cow::Uninit => unreachable!(),
                }
            }
        }

        impl<'b, B: Clone> Cow<'b, B> {
            #[inline(always)]
            pub(crate) fn take(&mut self) -> Self {
                ::std::mem::take(self)
            }

            #[inline(always)]
            pub(crate) const fn owned(b: B) -> Self {
                Cow::Owned(b)
            }

            #[inline(always)]
            pub(crate) const fn as_cow(&mut self) -> &mut Self {
                self
            }

            #[inline(always)]
            pub(crate) fn into_owned(self) -> B {
                match self {
                    Cow::Borrowed(borrowed) => B::clone(borrowed),
                    Cow::Owned(owned) => owned,
                    Cow::Uninit => unreachable!(),
                }
            }

            #[inline(always)]
            pub(crate) fn try_into_owned(self) -> Result<B, Self> {
                match self {
                    Cow::Borrowed(_) => Err(self),
                    Cow::Owned(owned) => Ok(owned),
                    Cow::Uninit => unreachable!(),
                }
            }

            #[inline(always)]
            pub(crate) const fn borrow<'c>(&'c self) -> Cow<'b, B> {
                match self {
                    Cow::Borrowed(b) => Cow::Borrowed(b),
                    Cow::Owned(ref o) => unsafe {
                        // Safe because the value will NOT be modified observably
                        ::std::mem::transmute::<Cow<'c, B>, Cow<'b, B>>(Cow::Borrowed(o))
                    },
                    Cow::Uninit => unreachable!(),
                }
            }
        }
    )
}
