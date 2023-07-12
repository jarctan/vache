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

        impl __Sub<__Integer> for __Integer {
            type Output = Self;

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
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                <::num_bigint::BigInt as ::std::fmt::Display>::fmt(&self.0, f)
            }
        }

        impl ::std::fmt::Debug for __Integer {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                <::num_bigint::BigInt as ::std::fmt::Debug>::fmt(&self.0, f)
            }
        }

        impl TryFrom<u128> for __Integer {
            type Error = <u128 as ::std::convert::TryFrom<u128>>::Error;

            fn try_from(t: u128) -> Result<Self, Self::Error> {
                Ok(Self(t.try_into()?))
            }
        }
    )
}
