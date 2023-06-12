//! Defining the prelude Rust code for our output in a separate file since it's
//! really big.
use proc_macro2::TokenStream;

/// Producing the necessary prelude for all our outputs.
pub fn prelude() -> TokenStream {
    quote!(
        #![feature(try_blocks)]
        #![allow(clippy::needless_late_init)]
        #![allow(unused_mut)]
        #![allow(unused_imports)]
        #![allow(dead_code)]
        #[warn(unused_imports)]
        #[warn(unused_parens)]

        use std::borrow::{Borrow, BorrowMut};
        use std::fmt;
        use std::mem::MaybeUninit;
        use std::ops::{Add, Deref, DerefMut, Div, Mul, Rem, Sub};
        use num_traits::ToPrimitive;
        use ::anyhow::Context as __AnyhowContext;

        use ::anyhow::Result as __Result;
        pub type __String = String;
        #[derive(Clone)]
        pub struct __Vec<T>(std::vec::Vec<T>);

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
