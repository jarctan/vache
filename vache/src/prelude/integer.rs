//! Redefining integers.
//!
//! The current reason for redefining integers entirely is that we want them to
//! implement the `Step` trait.

use proc_macro2::TokenStream;

#[allow(clippy::missing_docs_in_private_items)]
pub fn integer() -> TokenStream {
    quote!(
        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
        pub struct __Integer(::malachite::Integer);

        impl __Integer {
            #[inline(always)]
            const fn zero() -> Self {
                Self(__Zero::ZERO)
            }

            #[inline(always)]
            const fn one() -> Self {
                Self(__One::ONE)
            }

            #[inline(always)]
            fn to_usize(&self) -> Option<usize> {
                (&self.0).try_into().ok()
            }
        }

        impl ::std::iter::Step for __Integer {
            #[inline(always)]
            fn steps_between(start: &Self, end: &Self) -> Option<usize> {
                usize::try_from(&(&start.0 - &end.0)).ok()
            }
            #[inline(always)]
            fn forward_checked(start: Self, count: usize) -> Option<Self> {
                Some(Self(start.0 + ::malachite::Integer::from(count)))
            }
            #[inline(always)]
            fn backward_checked(start: Self, count: usize) -> Option<Self> {
                Some(Self(start.0 - ::malachite::Integer::from(count)))
            }
        }

        impl __Sub<__Integer> for __Integer {
            type Output = Self;

            #[inline(always)]
            fn sub<'b>(
                x: Cow<'b, Self>,
                y: Cow<'b, Self>,
            ) -> __Result<__Ret<Cow<'b, __Integer>, __noRet>> {
                match (Cow::try_into_owned(x),Cow::try_into_owned(y))  {
                    (Ok(x), Ok(y)) => __Ret::ok(
                        Cow::Owned(__Integer(x.0 - y.0)),
                        __noRet {},
                    ),
                    (Ok(x), Err(y)) => __Ret::ok(
                        Cow::Owned(__Integer(x.0 - &y.0)),
                        __noRet {},
                    ),
                    (Err(x), Ok(y)) => __Ret::ok(
                        Cow::Owned(__Integer(&x.0 - y.0)),
                        __noRet {},
                    ),
                    (Err(x), Err(y)) => __Ret::ok(
                        Cow::Owned(__Integer(&x.0 - &y.0)),
                        __noRet {},
                    ),
                }
            }
        }

        impl __Add<__Integer> for __Integer {
            type Output = Self;

            #[inline(always)]
            fn add<'b>(
                x: Cow<'b, Self>,
                y: Cow<'b, Self>,
            ) -> __Result<__Ret<Cow<'b, __Integer>, __noRet>> {
                match (Cow::try_into_owned(x),Cow::try_into_owned(y))  {
                    (Ok(x), Ok(y)) => __Ret::ok(
                        Cow::Owned(__Integer(x.0 + y.0)),
                        __noRet {},
                    ),
                    (Ok(x), Err(y)) => __Ret::ok(
                        Cow::Owned(__Integer(x.0 + &y.0)),
                        __noRet {},
                    ),
                    (Err(x), Ok(y)) => __Ret::ok(
                        Cow::Owned(__Integer(&x.0 + y.0)),
                        __noRet {},
                    ),
                    (Err(x), Err(y)) => __Ret::ok(
                        Cow::Owned(__Integer(&x.0 + &y.0)),
                        __noRet {},
                    ),
                }
            }
        }

        impl __Mul<__Integer> for __Integer {
            type Output = Self;

            #[inline(always)]
            fn mul<'b>(
                x: Cow<'b, Self>,
                y: Cow<'b, Self>,
            ) -> __Result<__Ret<Cow<'b, __Integer>, __noRet>> {
                match (Cow::try_into_owned(x),Cow::try_into_owned(y))  {
                    (Ok(x), Ok(y)) => __Ret::ok(
                        Cow::Owned(__Integer(x.0 * y.0)),
                        __noRet {},
                    ),
                    (Ok(x), Err(y)) => __Ret::ok(
                        Cow::Owned(__Integer(x.0 * &y.0)),
                        __noRet {},
                    ),
                    (Err(x), Ok(y)) => __Ret::ok(
                        Cow::Owned(__Integer(&x.0 * y.0)),
                        __noRet {},
                    ),
                    (Err(x), Err(y)) => __Ret::ok(
                        Cow::Owned(__Integer(&x.0 * &y.0)),
                        __noRet {},
                    ),
                }
            }
        }

        impl __Div<__Integer> for __Integer {
            type Output = Self;

            #[inline(always)]
            fn div<'b>(
                x: Cow<'b, Self>,
                y: Cow<'b, Self>,
            ) -> __Result<__Ret<Cow<'b, __Integer>, __noRet>> {
                match (Cow::try_into_owned(x),Cow::try_into_owned(y))  {
                    (Ok(x), Ok(y)) => __Ret::ok(
                        Cow::Owned(__Integer(x.0 / y.0)),
                        __noRet {},
                    ),
                    (Ok(x), Err(y)) => __Ret::ok(
                        Cow::Owned(__Integer(x.0 / &y.0)),
                        __noRet {},
                    ),
                    (Err(x), Ok(y)) => __Ret::ok(
                        Cow::Owned(__Integer(&x.0 / y.0)),
                        __noRet {},
                    ),
                    (Err(x), Err(y)) => __Ret::ok(
                        Cow::Owned(__Integer(&x.0 / &y.0)),
                        __noRet {},
                    ),
                }
            }
        }

        impl __Rem<__Integer> for __Integer {
            type Output = Self;

            #[inline(always)]
            fn rem<'b>(
                x: Cow<'b, Self>,
                y: Cow<'b, Self>,
            ) -> __Result<__Ret<Cow<'b, __Integer>, __noRet>> {
                match (Cow::try_into_owned(x),Cow::try_into_owned(y))  {
                    (Ok(x), Ok(y)) => __Ret::ok(
                        Cow::Owned(__Integer(x.0 % y.0)),
                        __noRet {},
                    ),
                    (Ok(x), Err(y)) => __Ret::ok(
                        Cow::Owned(__Integer(x.0 % &y.0)),
                        __noRet {},
                    ),
                    (Err(x), Ok(y)) => __Ret::ok(
                        Cow::Owned(__Integer(&x.0 % y.0)),
                        __noRet {},
                    ),
                    (Err(x), Err(y)) => __Ret::ok(
                        Cow::Owned(__Integer(&x.0 % &y.0)),
                        __noRet {},
                    ),
                }
            }
        }

        impl ::std::fmt::Display for __Integer {
            #[inline(always)]
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                <::malachite::Integer as ::std::fmt::Display>::fmt(&self.0, f)
            }
        }

        impl ::std::fmt::Debug for __Integer {
            #[inline(always)]
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                <::malachite::Integer as ::std::fmt::Debug>::fmt(&self.0, f)
            }
        }

        impl TryFrom<u128> for __Integer {
            type Error = <u128 as ::std::convert::TryFrom<u128>>::Error;

            #[inline(always)]
            fn try_from(t: u128) -> Result<Self, Self::Error> {
                Ok(Self(t.try_into()?))
            }
        }

        pub(crate) fn __rand<'b>(
            start: Cow<'b, __Integer>,
            end: Cow<'b, __Integer>,
        ) -> __Result<__Ret<Cow<'b, __Integer>, __noRet>> {
            let seed = rand::random();
            let res = ::malachite::integer::random::uniform_random_integer_range(
                ::malachite::random::Seed::from_bytes(seed),
                start.into_owned().0,
                end.into_owned().0,
            ).next().context("Could not generate any random integer")?;

            __Ret::ok(Cow::Owned(__Integer(res)), __noRet {})
        }
    )
}
