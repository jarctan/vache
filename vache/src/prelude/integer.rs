//! Redefining integers.
//!
//! The current reason for redefining integers entirely is that we want them to
//! implement the `Step` trait.

use proc_macro2::TokenStream;

#[allow(clippy::missing_docs_in_private_items)]
pub fn integer() -> TokenStream {
    quote!(
        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
        pub struct __Integer(::num_bigint::BigInt);

        impl ::num_traits::ToPrimitive for __Integer {
            #[inline(always)]
            fn to_i64(&self) -> Option<i64> {
                self.0.to_i64()
            }

            #[inline(always)]
            fn to_u64(&self) -> Option<u64> {
                self.0.to_u64()
            }
        }

        impl __Integer {
            #[inline(always)]
            fn zero() -> Self {
                Self(__Zero::zero())
            }

            #[inline(always)]
            fn one() -> Self {
                Self(__One::one())
            }
        }

        impl ::std::iter::Step for __Integer {
            #[inline(always)]
            fn steps_between(start: &Self, end: &Self) -> Option<usize> {
                start.0.checked_sub(&end.0)?.try_into().ok()
            }
            #[inline(always)]
            fn forward_checked(start: Self, count: usize) -> Option<Self> {
                Some(Self(start.0.checked_add(&count.into())?))
            }
            #[inline(always)]
            fn backward_checked(start: Self, count: usize) -> Option<Self> {
                Some(Self(start.0.checked_sub(&count.into())?))
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
                <::num_bigint::BigInt as ::std::fmt::Display>::fmt(&self.0, f)
            }
        }

        impl ::std::fmt::Debug for __Integer {
            #[inline(always)]
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                <::num_bigint::BigInt as ::std::fmt::Debug>::fmt(&self.0, f)
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
            let mut rng = ::rand::thread_rng();
            let range = ::num_bigint::UniformBigInt::new(&start.0, &end.0);

            __Ret::ok(Cow::Owned(__Integer(range.sample(&mut rng))), __noRet {})
        }
    )
}
