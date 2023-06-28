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
                let res: Result<(), ::vache_lib::reporter::Diagnostics> = try {
                    let mut checked = ::vache_lib::typecheck(&mut context, p)?;
                    let mir = ::vache_lib::borrow_check(&mut context, ::vache_lib::mir(&mut checked)?)?;
                    let cur_dir = ::std::env::current_dir().context("Could not get current directory")?;
                    let binary_name = "test-binary";
                    let res = ::vache_lib::run(&mut context, checked, binary_name, &cur_dir).context("Could not run program")?;
                    let expected = #expected_output;
                    ::anyhow::ensure!(
                        res == expected,
                        "output mismatch\nexpected:\n{expected}\nfound:\n{res}"
                    );

                    let dest_file = cur_dir.join(binary_name);
                    ::std::fs::remove_file(&dest_file).context("failed to remove binary at the end of the test")?;
                };
                if let Err(diagnostics) = res {
                    diagnostics.display()?;
                    ::anyhow::bail!("Compile errors");
                }
                Ok(())
            }

            #[test]
            fn interpreted() -> ::anyhow::Result<()>  {
                let p: ::vache_lib::ast::Program = #program;
                let arena = ::vache_lib::Arena::new();
                let config = ::vache_lib::config::Config { input: "", ..::std::default::Default::default() };
                let mut context = ::vache_lib::Context::new(config, &arena);
                let res: Result<(), ::vache_lib::reporter::Diagnostics> = try {
                    let mut checked = ::vache_lib::typecheck(&mut context, p)?;
                    let mir = ::vache_lib::borrow_check(&mut context, ::vache_lib::mir(&mut checked)?)?;
                    eprintln!("MIR: {:#?}", mir);
                    let res = ::vache_lib::interpret(mir).context("interpreter error")?;
                    let expected = #expected_output;
                    ::anyhow::ensure!(
                        res == expected,
                        "output mismatch\nexpected:\n{expected}\nfound:\n{res}"
                    );
                };
                if let Err(diagnostics) = res {
                    diagnostics.display()?;
                    ::anyhow::bail!("Compile errors");
                }
                Ok(())
            }
        }
    }
    .into()
}
