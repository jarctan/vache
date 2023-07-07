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
            MutBorrowed(&'a mut B),
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
                    Cow::MutBorrowed(ref b) => write!(f, "&mut {:?}", b),
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
                    Cow::MutBorrowed(ref b) => ::std::fmt::Display::fmt(b, f),
                    Cow::Owned(ref o) => ::std::fmt::Display::fmt(o, f),
                    Cow::Uninit => unreachable!(),
                }
            }
        }

        impl<'a, B: 'a + Clone> Clone for Cow<'a, B> {
            fn clone(&self) -> Self {
                match self {
                    Cow::Borrowed(b) => Cow::Borrowed(b),
                    Cow::MutBorrowed(b) => {
                        let b: &B = b.borrow();
                        Cow::Owned(b.to_owned())
                    }
                    Cow::Owned(ref b) => {
                        let b: &B = b.borrow();
                        Cow::Owned(b.to_owned())
                    }
                    Cow::Uninit => unreachable!(),
                }
            }

            fn clone_from(&mut self, source: &Self) {
                match (self, source) {
                    (&mut Cow::Owned(ref mut dest), Cow::Owned(ref o)) => {
                        o.borrow().clone_into(dest)
                    }
                    (&mut Cow::Uninit, _) => unreachable!(),
                    (t, s) => *t = s.clone(),
                }
            }
        }

        impl<B: Clone> ::std::ops::Deref for Cow<'_, B> {
            type Target = B;

            fn deref(&self) -> &B {
                match self {
                    Cow::Borrowed(borrowed) => borrowed,
                    Cow::MutBorrowed(borrowed) => borrowed,
                    Cow::Owned(ref owned) => owned.borrow(),
                    Cow::Uninit => unreachable!(),
                }
            }
        }

        impl<B: Clone> ::std::ops::DerefMut for Cow<'_, B> {
            fn deref_mut(&mut self) -> &mut B {
                match self {
                    Cow::Borrowed(borrowed) => {
                        *self = Cow::Owned(borrowed.clone());
                        match self {
                            Cow::Owned(ref mut owned) => owned,
                            Cow::Borrowed(..) | Cow::MutBorrowed(..) => unreachable!(),
                            Cow::Uninit => unreachable!(),
                        }
                    }
                    Cow::MutBorrowed(borrowed) => borrowed,
                    Cow::Owned(ref mut owned) => owned,
                    Cow::Uninit => unreachable!(),
                }
            }
        }

        impl<'a, 'b, Rhs: Clone, T: ::std::cmp::PartialEq<Rhs> + Clone>
            ::std::cmp::PartialEq<Cow<'a, Rhs>> for Cow<'b, T>
        {
            fn eq(&self, other: &Cow<'a, Rhs>) -> bool {
                <T as ::std::cmp::PartialEq<Rhs>>::eq(&**self, &**other)
            }
        }

        impl<'a, 'b, Rhs: Clone, T: ::std::cmp::PartialOrd<Rhs> + Clone>
            ::std::cmp::PartialOrd<Cow<'a, Rhs>> for Cow<'b, T>
        {
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

        impl<'a, B: Clone> ::std::borrow::Borrow<B> for Cow<'a, B> {
            fn borrow(&self) -> &B {
                self
            }
        }

        impl<'a, B: Clone> ::std::borrow::BorrowMut<B> for Cow<'a, B> {
            fn borrow_mut(&mut self) -> &mut B {
                self
            }
        }

        impl<'a, 'c, 'b: 'c, B: Clone> Cow<'a, __Vec<Cow<'b, B>>> {
            pub fn remove(mut self, index: usize) -> __Result<Cow<'c, B>> {
                match self {
                    Cow::Borrowed(b) => Ok(b.get(index)?.clone()),
                    Cow::MutBorrowed(array) => array.remove(index),
                    Cow::Owned(mut array) => array.consume(index),
                    Cow::Uninit => unreachable!(),
                }
            }
        }

        /// Prelude function.
        #[allow(clippy::ptr_arg)]
        pub(crate) fn __borrow<'b, 'c: 'b, 'a, B: Clone>(cow: &'c Cow<'a, B>) -> Cow<'b, B> {
            match cow {
                Cow::Borrowed(b) => Cow::Borrowed(b),
                Cow::MutBorrowed(b) => Cow::Borrowed(b),
                Cow::Owned(ref o) => {
                    let b: &'c B = o.borrow();
                    Cow::Borrowed(b)
                }
                Cow::Uninit => unreachable!(),
            }
        }

        /// Prelude function.
        #[allow(clippy::ptr_arg)]
        pub(crate) fn __borrow_mut<'b, 'c: 'b, 'a: 'b, B: Clone>(
            cow: &'c mut Cow<'a, B>,
        ) -> Cow<'b, B> {
            match cow {
                Cow::Borrowed(b) => Cow::Borrowed(b),
                Cow::MutBorrowed(b) => Cow::Borrowed(b),
                Cow::Owned(ref mut o) => {
                    let b: &'c mut B = o.borrow_mut();
                    Cow::MutBorrowed(b)
                }
                Cow::Uninit => unreachable!(),
            }
        }

        impl<'a, B: Clone> Cow<'a, B> {
            pub fn __take(&mut self) -> Self {
                ::std::mem::take(self)
            }

            pub fn owned(b: B) -> Self {
                Cow::Owned(b)
            }

            pub fn into_owned(self) -> B {
                match self {
                    Cow::Borrowed(borrowed) => borrowed.clone(),
                    Cow::MutBorrowed(borrowed) => (*borrowed).clone(),
                    Cow::Owned(owned) => owned,
                    Cow::Uninit => unreachable!(),
                }
            }

            pub fn try_into_owned(self) -> Result<B, Self> {
                match self {
                    Cow::Borrowed(_) => Err(self),
                    Cow::MutBorrowed(_) => Err(self),
                    Cow::Owned(owned) => Ok(owned),
                    Cow::Uninit => unreachable!(),
                }
            }
        }
    )
}
