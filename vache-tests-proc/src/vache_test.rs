use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{parse_macro_input, Block, Error, Ident, ItemFn, LitStr, Result};

struct TestAttr {
    expected_output: LitStr,
}

impl Parse for TestAttr {
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

pub fn vache_test(attr: TokenStream, item: TokenStream) -> TokenStream {
    let attr = parse_macro_input!(attr as TestAttr);
    let item = parse_macro_input!(item as TestFn);

    let TestAttr { expected_output } = attr;
    let TestFn { name, program } = item;

    quote! {
        mod #name {
            use super::*;

            #[test]
            #[serial_test::serial(compiled)]
            fn compiled() -> ::anyhow::Result<()> {
                use ::anyhow::Context as AnyhowContext;
                let p: ::vache_lib::ast::Program = #program;
                let arena = ::vache_lib::Arena::new();
                let config = ::vache_lib::config::Config { input: "", ..::std::default::Default::default() };
                let mut context = ::vache_lib::Context::new(config, &arena);
                match ::vache_lib::check(&mut context, p)? {
                    Ok(mut checked) => {
                        let mir = ::vache_lib::borrow_check(::vache_lib::mir(&mut checked)?)?;
                        let cur_dir = ::std::env::current_dir().context("Could not get current directory")?;
                        let binary_name = "test";
                        assert_eq!(
                            ::vache_lib::run(&mut context, checked, binary_name, &cur_dir).context("Could not run program")?,
                            #expected_output,
                            "Output mismatch for binary"
                        );

                        let dest_file = cur_dir.join(binary_name);
                        ::std::fs::remove_file(&dest_file).context("failed to remove binary at the end of the test")?;
                        Ok(())
                    }
                    Err(err) => {
                        ::anyhow::bail!("Compile errors");
                    }
                }
            }

            #[test]
            fn interpreted() -> ::anyhow::Result<()>  {
                let p: ::vache_lib::ast::Program = #program;
                let arena = ::vache_lib::Arena::new();
                let config = ::vache_lib::config::Config { input: "", ..::std::default::Default::default() };
                let mut context = ::vache_lib::Context::new(config, &arena);
                match ::vache_lib::check(&mut context, p)? {
                    Ok(mut checked) => {
                        let mir = ::vache_lib::borrow_check(::vache_lib::mir(&mut checked)?)?;
                        eprintln!("MIR: {:#?}", mir);
                        assert_eq!(
                            ::vache_lib::interpret(mir)?,
                            #expected_output,
                            "Output mismatch for interpreter"
                        );
                        Ok(())
                    }
                    Err(err) => {
                        ::anyhow::bail!("Compile errors");
                    }
                }
            }
        }
    }
    .into()
}
