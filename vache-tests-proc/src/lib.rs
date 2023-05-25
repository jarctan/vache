extern crate proc_macro;
use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{parse_macro_input, Block, Error, Ident, ItemFn, LitStr, Result};

#[macro_use]
extern crate quote;

struct Attr {
    expected_output: LitStr,
}

impl Parse for Attr {
    fn parse(input: ParseStream) -> Result<Self> {
        let expected_output = input.parse()?;
        Ok(Self { expected_output })
    }
}

struct TestFn {
    name: Ident,
    program: Block,
}

impl Parse for TestFn {
    fn parse(input: ParseStream) -> Result<Self> {
        let f: ItemFn = input.parse()?;
        if !f.sig.inputs.is_empty() {
            return Err(Error::new(f.sig.inputs.span(), "expected no arguments"));
        }
        Ok(Self {
            name: f.sig.ident,
            program: *f.block,
        })
    }
}

#[proc_macro_attribute]
pub fn vache_test(attr: TokenStream, item: TokenStream) -> TokenStream {
    let attr = parse_macro_input!(attr as Attr);
    let item = parse_macro_input!(item as TestFn);

    let Attr { expected_output } = attr;
    let TestFn { name, program } = item;

    quote! {
        mod #name {
            use super::*;

            #[test]
            #[serial_test::serial(compiled)]
            fn compiled() {
                let p: ::vache_lib::ast::Program = #program;
                let mut checked = ::vache_lib::check(p);
                let mir = ::vache_lib::borrow_check(::vache_lib::mir(&mut checked));
                let cur_dir = ::std::env::current_dir().unwrap();
                let binary_name = "test";
                assert_eq!(
                    ::vache_lib::run(checked, binary_name, &::std::env::current_dir().unwrap()).unwrap(),
                    #expected_output,
                    "Output mismatch for binary"
                );
                
                let dest_file = ::std::env::current_dir().unwrap().join(binary_name);
                ::std::fs::remove_file(&dest_file).expect("failed to remove binary at the end of the test");
            }

            #[test]
            fn interpreted() {
                let p: ::vache_lib::ast::Program = #program;
                let mut checked = ::vache_lib::check(p);
                let mir = ::vache_lib::borrow_check(::vache_lib::mir(&mut checked));
                println!("MIR: {:#?}", mir);
                assert_eq!(
                    ::vache_lib::interpret(mir),
                    #expected_output,
                    "Output mismatch for interpreter"
                );
            }
        }
    }
    .into()
}
