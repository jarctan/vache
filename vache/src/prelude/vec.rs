//! Redefining vectors/arrays.
//!
//! The current reason for redefining them is that we to implement `Display` for
//! them. We want vectors to be pretty printed nicely when used in
//! `println!("{}")`s.

use proc_macro2::TokenStream;

#[allow(clippy::missing_docs_in_private_items)]
pub fn vec() -> TokenStream {
    quote!(
        #[derive(Clone)]
        pub struct __Vec<'b, T: ::std::clone::Clone>(::std::vec::Vec<Cow<'b, T>>);

        pub(crate) struct __pushRet<'b, T: ::std::clone::Clone> {
            array: Option<Cow<'b, __Vec<'b, T>>>,
        }

        impl<'b, T: ::std::clone::Clone> __Vec<'b, T> {
            pub(crate) fn remove(&mut self, index: usize) -> __Result<Cow<'b, T>> {
                if index < self.0.len() {
                    Ok(self.0.remove(index))
                } else {
                    ::anyhow::bail!("index {index} is out of bounds")
                }
            }

            pub(crate) fn consume(mut self, index: usize) -> __Result<Cow<'b, T>> {
                if index < self.0.len() {
                    Ok(self.0.swap_remove(index))
                } else {
                    ::anyhow::bail!("index {index} is out of bounds")
                }
            }

            pub(crate) fn get(&self, index: usize) -> __Result<&Cow<'b, T>> {
                self.0
                    .get(index)
                    .with_context(|| format!("index {index} is out of bounds"))
            }

            pub(crate) fn get_mut(&mut self, index: usize) -> __Result<&mut Cow<'b, T>> {
                self.0
                    .get_mut(index)
                    .with_context(|| format!("index {index} is out of bounds"))
            }

            pub(crate) fn push<'d: 'b, 'c, 'e>(
                mut array: Var<'c, 'd, Self>,
                el: Cow<'b, T>,
            ) -> __Result<__Ret<(), __pushRet<'b, T>>> {
                (**array).0.push(el);
                __Ret::ok(
                    (),
                    __pushRet {
                        array: array.try_to_cow().ok(),
                    },
                )
            }
        }

        impl<'b, T: ::std::clone::Clone + ::std::fmt::Debug> ::std::fmt::Debug for __Vec<'b, T> {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                write!(f, "[")?;
                let mut iter = self.0.iter();
                if let Some(i) = iter.next() {
                    write!(f, "{:?}", i)?;
                }
                for i in iter {
                    write!(f, ", {:?}", i)?;
                }
                write!(f, "]")
            }
        }

        impl<'b, T: __PartialEq + ::std::clone::Clone> __PartialEq for __Vec<'b, T> {
            fn eq<'d>(
                x: Cow<'d, __Vec<'b, T>>,
                y: Cow<'d, __Vec<'b, T>>,
            ) -> __Result<__Ret<Cow<'d, bool>, __noRet>> {
                let b1: &__Vec<'b, T> = &*x;
                let b2: &__Vec<'b, T> = &*y;
                if b1.0.len() == b2.0.len() {
                    for (x, y) in b1.0.iter().zip(b2.0.iter()) {
                        if *__PartialEq::ne(Cow::borrow(x), Cow::borrow(y))?.0 {
                            return __Ret::ok(Cow::owned(false), __noRet {});
                        }
                    }
                    __Ret::ok(Cow::Owned(true), __noRet {})
                } else {
                    __Ret::ok(Cow::owned(false), __noRet {})
                }
            }
        }
    )
}
