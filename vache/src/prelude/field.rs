//! Defining fields.
//!
//! Fields are a wrapper type around structure fields (surprised, aren't you?)
//! that allow us to have partially defined structures (structures for which
//! some fields are not defined at some point). This is useful to mimic
//! Rust behavior, that allow to move out specific fields of a structure. We
//! want to have these `Field` so that we can move out elements as we want.
//!
//! # Safety
//! Note: is utterly unsafe. This relies exclusively on the assumption that the
//! borrow checker of Vache is correct to be sure that we will NEVER access a
//! field after we took/moved it.
//!
//! # How to use
//! `new()` creates a new field. `take()` takes away the value of that field,
//! assuming you will NEVER access that field again afterwards. `as_ref()` and
//! `as_mut()` are accessors that suppose that you did not `take()` before.

use proc_macro2::TokenStream;

#[allow(clippy::missing_docs_in_private_items)]
pub fn field() -> TokenStream {
    quote!(
        pub struct Field<T>(__MaybeUninit<T>);

        impl<T> Field<T> {
            pub fn new(value: T) -> Self {
                Self(__MaybeUninit::new(value))
            }

            pub fn take(&mut self) -> T {
                unsafe { std::mem::replace(&mut self.0, __MaybeUninit::uninit()).assume_init() }
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
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                write!(f, "{:?}", self.as_ref())
            }
        }
    )
}
