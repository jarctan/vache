//! Defining the prelude Rust code for our output in a separate file since it's
//! really big.
use proc_macro2::TokenStream;

/// Producing the necessary prelude for all our outputs.
pub fn prelude() -> TokenStream {
    quote!(
        #![feature(try_blocks)]
        #![feature(step_trait)]

        #![allow(clippy::needless_late_init)]
        #![allow(unused_mut)]
        #![allow(unused_imports)]
        #![allow(dead_code)]
        #![allow(unused_variables)]
        #![allow(unused_parens)]

        use std::borrow::{Borrow, BorrowMut};
        use std::fmt;
        use std::mem::MaybeUninit;
        use std::ops::{Add, Deref, DerefMut, Div, Mul, Rem, Sub};
        use ::num_traits::ToPrimitive as __ToPrimitive;
        use ::anyhow::Context as __AnyhowContext;

        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
        pub struct __Integer(::num_bigint::BigInt);

        impl ::num_traits::ToPrimitive for __Integer {
            fn to_i64(&self) -> Option<i64> {
                self.0.to_i64()
            }

            fn to_u64(&self) -> Option<u64> {
                self.0.to_u64()
            }
        }

        impl ::std::iter::Step for __Integer {
            fn steps_between(start: &Self, end: &Self) -> Option<usize> {
                start.0.checked_sub(&end.0)?.try_into().ok()
            }
            fn forward_checked(start: Self, count: usize) -> Option<Self> {
                Some(Self(start.0.checked_add(&count.into())?))
            }
            fn backward_checked(start: Self, count: usize) -> Option<Self> {
                Some(Self(start.0.checked_sub(&count.into())?))
            }
        }

        impl ::std::ops::Sub for __Integer {
            type Output = Self;

            fn sub(self, other: Self) -> Self {
                Self(self.0 - other.0)
            }
        }

        impl ::std::ops::Add for __Integer {
            type Output = Self;

            fn add(self, other: Self) -> Self {
                Self(self.0 + other.0)
            }
        }

        impl ::std::ops::Mul for __Integer {
            type Output = Self;

            fn mul(self, other: Self) -> Self {
                Self(self.0 * other.0)
            }
        }

        impl ::std::ops::Div for __Integer {
            type Output = Self;

            fn div(self, other: Self) -> Self {
                Self(self.0 / other.0)
            }
        }

        impl ::std::ops::Rem for __Integer {
            type Output = Self;

            fn rem(self, other: Self) -> Self {
                Self(self.0 % other.0)
            }
        }

        impl fmt::Display for __Integer {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                <::num_bigint::BigInt as fmt::Display>::fmt(&self.0, f)
            }
        }

        impl fmt::Debug for __Integer {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                <::num_bigint::BigInt as fmt::Debug>::fmt(&self.0, f)
            }
        }

        impl TryFrom<u128> for __Integer {
            type Error = <u128 as ::std::convert::TryFrom<u128>>::Error;

            fn try_from(t: u128) -> Result<Self, Self::Error> {
                Ok(Self(t.try_into()?))
            }
        }

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
            type Item = T;
            type IntoIter = ::std::ops::Range<T>;

            fn into_iter(self) -> Self::IntoIter {
                self.into_owned().0
            }
        }

        impl<'a, 'b, Rhs: Clone, T: ::std::cmp::PartialEq<Rhs> + Clone> ::std::cmp::PartialEq<Cow<'a, Rhs>> for Cow<'b, T> {
            fn eq(&self, other: &Cow<'a, Rhs>) -> bool {
                <T as ::std::cmp::PartialEq<Rhs>>::eq(&**self, &**other)
            }
        }

        impl<'a, 'b, Rhs: Clone, T: ::std::cmp::PartialOrd<Rhs> + Clone> ::std::cmp::PartialOrd<Cow<'a, Rhs>> for Cow<'b, T> {
            fn partial_cmp(&self, other: &Cow<'a, Rhs>) -> Option<::std::cmp::Ordering> {
                <T as ::std::cmp::PartialOrd<Rhs>>::partial_cmp(&**self, &**other)
            }
        }

        impl<T: ::std::iter::Step + std::cmp::PartialOrd> ::std::iter::Step for Cow<'_, T> {
            fn steps_between(start: &Self, end: &Self) -> Option<usize> {
                <T as ::std::iter::Step>::steps_between(&**start, &**end)
            }
            fn forward_checked(start: Self, count: usize) -> Option<Self> {
                Some(Cow::Owned(<T as ::std::iter::Step>::forward_checked(start.into_owned(), count)?))
            }
            fn backward_checked(start: Self, count: usize) -> Option<Self> {
                Some(Cow::Owned(<T as ::std::iter::Step>::backward_checked(start.into_owned(), count)?))
            }
        }

        use ::anyhow::Result as __Result;
        pub type __String = String;

        #[derive(Clone)]
        pub struct __Vec<T>(::std::vec::Vec<T>);

        impl<T> __Vec<T> {
            pub fn remove(&mut self, index: usize) -> __Result<T> {
                if index < self.0.len() {
                    Ok(self.0.remove(index))
                } else {
                    ::anyhow::bail!("index {index} is out of bounds")
                }
            }
            pub fn consume(mut self, index: usize) -> __Result<T> {
                if index < self.0.len() {
                    Ok(self.0.swap_remove(index))
                } else {
                    ::anyhow::bail!("index {index} is out of bounds")
                }
            }
            pub fn get(&self, index: usize) -> __Result<&T> {
                self.0.get(index).with_context(|| format!("index {index} is out of bounds"))
            }
            pub fn get_mut(&mut self, index: usize) -> __Result<&mut T> {
                self.0.get_mut(index).with_context(|| format!("index {index} is out of bounds"))
            }
        }

        impl<T: fmt::Display> fmt::Display for __Vec<T> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "[")?;
                let mut iter = self.0.iter();
                if let Some(i) = iter.next() {
                    write!(f, "{}", i);
                }
                for i in iter {
                    write!(f, ", {}", i);
                }
                write!(f, "]")
            }
        }

        pub enum Cow<'a, B>
        where
            B: 'a + Clone,
        {
            Borrowed(&'a B),
            MutBorrowed(&'a mut B),
            Owned(B),
        }

        impl<B: Clone> fmt::Debug for Cow<'_, B>
        where
            B: fmt::Debug,
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match *self {
                    Cow::Borrowed(ref b) => write!(f, "&{:?}", b),
                    Cow::MutBorrowed(ref b) => write!(f, "&mut {:?}", b),
                    Cow::Owned(ref o) => write!(f, "{:?}", o),
                }
            }
        }

        impl<B: Clone> fmt::Display for Cow<'_, B>
        where
            B: fmt::Display,
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match *self {
                    Cow::Borrowed(ref b) => fmt::Display::fmt(b, f),
                    Cow::MutBorrowed(ref b) => fmt::Display::fmt(b, f),
                    Cow::Owned(ref o) => fmt::Display::fmt(o, f),
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
                }
            }

            fn clone_from(&mut self, source: &Self) {
                match (self, source) {
                    (&mut Cow::Owned(ref mut dest), Cow::Owned(ref o)) => o.borrow().clone_into(dest),
                    (t, s) => *t = s.clone(),
                }
            }
        }

        impl<B: Clone> Cow<'_, B> {
            pub fn into_owned(self) -> B {
                match self {
                    Cow::Borrowed(borrowed) => borrowed.clone(),
                    Cow::MutBorrowed(borrowed) => (*borrowed).clone(),
                    Cow::Owned(owned) => owned,
                }
            }
        }

        impl<B: Clone> Deref for Cow<'_, B> {
            type Target = B;

            fn deref(&self) -> &B {
                match self {
                    Cow::Borrowed(borrowed) => borrowed,
                    Cow::MutBorrowed(borrowed) => borrowed,
                    Cow::Owned(ref owned) => owned.borrow(),
                }
            }
        }

        impl<B: Clone> DerefMut for Cow<'_, B> {
            fn deref_mut(&mut self) -> &mut B {
                match self {
                    Cow::Borrowed(borrowed) => {
                        *self = Cow::Owned(borrowed.clone());
                        match self {
                            Cow::Owned(ref mut owned) => owned,
                            Cow::Borrowed(..) | Cow::MutBorrowed(..) => unreachable!(),
                        }
                    }
                    Cow::MutBorrowed(borrowed) => borrowed,
                    Cow::Owned(ref mut owned) => owned,
                }
            }
        }

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
        pub(crate) fn __add<'a, B: Add<Output = B> + Clone>(x: Cow<B>, y: Cow<B>) -> __Result<Cow<'a, B>> {
            Ok(Cow::Owned(x.into_owned() + y.into_owned()))
        }

        /// Prelude function.
        pub(crate) fn __sub<'a, B: Sub<Output = B> + Clone>(x: Cow<B>, y: Cow<B>) -> __Result<Cow<'a, B>> {
            Ok(Cow::Owned(x.into_owned() - y.into_owned()))
        }

        /// Prelude function.
        pub(crate) fn __mul<'a, B: Mul<Output = B> + Clone>(x: Cow<B>, y: Cow<B>) -> __Result<Cow<'a, B>> {
            Ok(Cow::Owned(x.into_owned() * y.into_owned()))
        }

        /// Prelude function.
        pub(crate) fn __div<'a, B: Div<Output = B> + Clone>(x: Cow<B>, y: Cow<B>) -> __Result<Cow<'a, B>> {
            Ok(Cow::Owned(x.into_owned() / y.into_owned()))
        }

        /// Prelude function.
        pub(crate) fn __rem<'a, B: Rem<Output = B> + Clone>(x: Cow<B>, y: Cow<B>) -> __Result<Cow<'a, B>> {
            Ok(Cow::Owned(x.into_owned() % y.into_owned()))
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
            }
        }

        /// Prelude function.
        #[allow(clippy::ptr_arg)]
        pub(crate) fn __borrow_mut<'b, 'c: 'b, 'a: 'b, B: Clone>(cow: &'c mut Cow<'a, B>) -> Cow<'b, B> {
            match cow {
                Cow::Borrowed(b) => Cow::Borrowed(b),
                Cow::MutBorrowed(b) => Cow::Borrowed(b),
                Cow::Owned(ref mut o) => {
                    let b: &'c mut B = o.borrow_mut();
                    Cow::MutBorrowed(b)
                }
            }
        }

        impl<'a, B: Clone> Borrow<B> for Cow<'a, B> {
            fn borrow(&self) -> &B {
                self
            }
        }

        impl<'a, B: Clone> BorrowMut<B> for Cow<'a, B> {
            fn borrow_mut(&mut self) -> &mut B {
                self
            }
        }

        impl<'a, 'c, 'b: 'c, B: Clone> Cow<'a, __Vec<Cow<'b, B>>> {
            pub fn remove(self, index: usize) -> __Result<Cow<'c, B>> {
                match self {
                    Cow::Borrowed(b) => Ok(b.get(index)?.clone()),
                    Cow::MutBorrowed(array) => array.remove(index),
                    Cow::Owned(mut array) => array.consume(index),
                }
            }
        }

        pub struct Field<T>(MaybeUninit<T>);

        impl<T> Field<T> {
            pub fn new(value: T) -> Self {
                Self(MaybeUninit::new(value))
            }

            pub fn take(&mut self) -> T {
                unsafe { std::mem::replace(&mut self.0, MaybeUninit::uninit()).assume_init() }
            }

            pub fn as_ref(&self) -> &T {
                unsafe { self.0.assume_init_ref() }
            }

            pub fn as_mut(&mut self) -> &mut T {
                unsafe { self.0.assume_init_mut() }
            }
        }

        impl<T: Clone> Clone for Field<T> {
            fn clone(&self) -> Self {
                Self::new(self.as_ref().clone())
            }
        }

        impl<T: ::std::fmt::Debug> ::std::fmt::Debug for Field<T> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{:?}", self.as_ref())
            }
        }
    )
}
