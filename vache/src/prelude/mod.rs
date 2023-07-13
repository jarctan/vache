//! Defining the prelude Rust code for all Rust outputs.
//!
//! Split in several files because:
//! * the prelude is long
//! * `rustfmt` seems to struggle with long `quote!()`s
//!
//! Each module = one part of the prelude. Usually regrouped by primitive they
//! define (integers, strings, etc) or by logically delimited parts of the
//! prelude.

use proc_macro2::TokenStream;

mod cow;
mod functions;
mod imports;
mod integer;
mod lints;
mod range;
mod reexports;
mod res;
mod string;
mod var;
mod vec;

/// Producing the necessary prelude for all our outputs.
pub fn prelude() -> TokenStream {
    let lints = lints::lints();
    let imports = imports::imports();
    let reexports = reexports::reexports();
    let integer = integer::integer();
    let range = range::range();
    let cow = cow::cow();
    let var = var::var();
    let string = string::string();
    let functions = functions::functions();
    let vec = vec::vec();
    let res = res::res();

    quote!(
        #lints

        #imports

        #reexports

        #integer

        #range

        #cow

        #var

        #res

        #vec

        #functions

        #string
    )
}
