//! Defining the result of functions: it is the standard result + returning the
//! values passed by reference.

use proc_macro2::TokenStream;

#[allow(clippy::missing_docs_in_private_items)]
pub fn res() -> TokenStream {
    quote!(
        #[derive(Default)]
        pub struct __Ret<A, B>(A, B);

        impl<A, B> __Ret<A, B> {
            pub fn ok(a: A, b: B) -> __Result<Self> {
                Ok(Self(a, b))
            }
        }

        impl<A: ::std::process::Termination, B> std::process::Termination for __Ret<A, B> {
            fn report(self) -> ::std::process::ExitCode {
                // `A` carries the termination status
                self.0.report()
            }
        }
    )
}
