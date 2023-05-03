//! Compiler code.

use proc_macro2::TokenStream;
use string_builder::Builder as StringBuilder;
use Ty::*;

use crate::mir::{Program, Ty};

/// Compiler, that turns our language into source code for an
/// executable language.
pub(crate) struct Compiler {}
impl Compiler {
    /// Creates a new compiler.
    pub fn new() -> Self {
        Self {}
    }

    /// Producing the necessary prelude for all our outputs.
    fn prelude() -> TokenStream {
        quote!(
            extern crate rug;
            use std::borrow::{Cow, Borrow};
            use std::ops::{Sub, Add, Rem, Mul, Div};

            /// Prelude function.
            pub(crate) fn __eq<B: PartialEq + Clone>(x: Cow<B>, y: Cow<B>) -> bool {
                let b1: &B = x.borrow();
                let b2: &B = y.borrow();
                b1 == b2
            }

            /// Prelude function.
            pub(crate) fn __neq<B: PartialEq + Clone>(x: Cow<B>, y: Cow<B>) -> bool {
                let b1: &B = x.borrow();
                let b2: &B = y.borrow();
                b1 != b2
            }

            /// Prelude function.
            pub(crate) fn __ge<B: PartialOrd + Clone>(x: Cow<B>, y: Cow<B>) -> bool {
                let b1: &B = x.borrow();
                let b2: &B = y.borrow();
                b1 >= b2
            }

            /// Prelude function.
            pub(crate) fn __gt<B: PartialOrd + Clone>(x: Cow<B>, y: Cow<B>) -> bool {
                let b1: &B = x.borrow();
                let b2: &B = y.borrow();
                b1 > b2
            }

            /// Prelude function.
            pub(crate) fn __le<B: PartialOrd + Clone>(x: Cow<B>, y: Cow<B>) -> bool {
                let b1: &B = x.borrow();
                let b2: &B = y.borrow();
                b1 <= b2
            }

            /// Prelude function.
            pub(crate) fn __lt<B: PartialOrd + Clone>(x: Cow<B>, y: Cow<B>) -> bool {
                let b1: &B = x.borrow();
                let b2: &B = y.borrow();
                b1 < b2
            }

            /// Prelude function.
            pub(crate) fn __add<'a, B: Add<Output = B> + Clone>(
                x: Cow<B>,
                y: Cow<B>,
            ) -> Cow<'a, B> {
                Cow::Owned(x.into_owned() + y.into_owned())
            }

            /// Prelude function.
            pub(crate) fn __sub<'a, B: Sub<Output = B> + Clone>(
                x: Cow<B>,
                y: Cow<B>,
            ) -> Cow<'a, B> {
                Cow::Owned(x.into_owned() - y.into_owned())
            }

            /// Prelude function.
            pub(crate) fn __mul<'a, B: Mul<Output = B> + Clone>(
                x: Cow<B>,
                y: Cow<B>,
            ) -> Cow<'a, B> {
                Cow::Owned(x.into_owned() * y.into_owned())
            }

            /// Prelude function.
            pub(crate) fn __div<'a, B: Div<Output = B> + Clone>(
                x: Cow<B>,
                y: Cow<B>,
            ) -> Cow<'a, B> {
                Cow::Owned(x.into_owned() / y.into_owned())
            }

            /// Prelude function.
            pub(crate) fn __rem<'a, B: Rem<Output = B> + Clone>(
                x: Cow<B>,
                y: Cow<B>,
            ) -> Cow<'a, B> {
                Cow::Owned(x.into_owned() % y.into_owned())
            }

            /// Prelude function.
            #[allow(clippy::ptr_arg)]
            pub(crate) fn __clone<'a, B: ?Sized + ToOwned>(cow: &'a Cow<'a, B>) -> Cow<'a, B> {
                match *cow {
                    Cow::Borrowed(b) => Cow::Borrowed(b),
                    Cow::Owned(ref o) => {
                        let b: &'a B = o.borrow();
                        Cow::Borrowed(b)
                    }
                }
            }
        )
    }

    /// Compiles a program in our language into an executable source code.
    pub fn compile(&mut self, p: Program) -> String {
        let tokens = self.visit_program(p);
        let prelude = Self::prelude();
        let file = syn::parse2(quote! {
            #prelude
            #tokens
        })
        .unwrap();
        prettyplease::unparse(&file)
    }

    /// Translate a type into a Rust type.
    pub fn translate_type(&self, ty: Ty, show_lifetime: bool) -> TokenStream {
        match ty {
            UnitT => quote!(()),
            BoolT => quote!(bool),
            IntT => {
                if show_lifetime {
                    quote!(Cow<'a, ::rug::Integer>)
                } else {
                    quote!(Cow<::rug::Integer>)
                }
            }
            StrT => {
                if show_lifetime {
                    quote!(Cow<'a, String>)
                } else {
                    quote!(Cow<String>)
                }
            }
            StructT(name) => {
                let name = format_ident!("{}", name);
                quote!(#name)
            }
        }
    }

    /// Compiles a program.
    pub fn visit_program(&mut self, p: Program) -> TokenStream {
        todo!()
    }
}
