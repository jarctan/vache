extern crate proc_macro;
use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, Attribute, Block, Error, FnArg, Ident, ItemFn, LitStr, Pat, Result, Token,
    Type, Expr,
};

#[macro_use]
extern crate quote;

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

#[proc_macro_attribute]
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
                        let mir = ::vache_lib::borrow_check(::vache_lib::mir(&mut checked));
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
                        let mir = ::vache_lib::borrow_check(::vache_lib::mir(&mut checked));
                        eprintln!("MIR: {:#?}", mir);
                        assert_eq!(
                            ::vache_lib::interpret(mir),
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

struct ParseAttr {
    input_str: LitStr,
    rule: Ident,
}

impl Parse for ParseAttr {
    fn parse(input: ParseStream) -> Result<Self> {
        let input_str = input.parse()?;
        input.parse::<Token![as]>()?;
        let rule = input.parse()?;
        Ok(Self { input_str, rule })
    }
}

struct ParseFn {
    name: Ident,
    body: Block,
    attrs: Vec<Attribute>,
    arg_name: Ident,
    arg_ty: Type,
}

impl Parse for ParseFn {
    fn parse(input: ParseStream) -> Result<Self> {
        let f: ItemFn = input.parse()?;
        if f.sig.inputs.len() == 1 {
            match f.sig.inputs.into_pairs().next().unwrap().into_value() {
                FnArg::Receiver(_) => panic!("first argument may not be a receiver"),
                FnArg::Typed(pat) => {
                    let ty = pat.ty;
                    match *pat.pat {
                        Pat::Ident(arg_name) => Ok(Self {
                            name: f.sig.ident,
                            attrs: f.attrs,
                            body: *f.block,
                            arg_name: arg_name.ident,
                            arg_ty: *ty,
                        }),
                        _ => panic!("first argument must be an identifier"),
                    }
                }
            }
        } else {
            Err(Error::new(
                f.sig.inputs.span(),
                "expected exactly one argument",
            ))
        }
    }
}

#[proc_macro_attribute]
pub fn parses(attr: TokenStream, item: TokenStream) -> TokenStream {
    let attr = parse_macro_input!(attr as ParseAttr);
    let item = parse_macro_input!(item as ParseFn);

    let ParseAttr { input_str, rule } = attr;
    let ParseFn {
        name,
        attrs,
        body,
        arg_name,
        arg_ty,
    } = item;

    quote! {
        #(#attrs)*
        fn #name() -> ::anyhow::Result<()> {
            use ::anyhow::Context as AnyhowContext;
            use ::pest::Parser;
            use crate::ast::parse_rule;
            let arena = crate::Arena::new();
            let input = #input_str;

            let config = crate::config::Config { input, ..::std::default::Default::default() };
            let mut ctx = crate::Context::new(config, &arena);

            let #arg_name: #arg_ty = parse_rule(&mut ctx, crate::grammar::Rule::#rule)?;
            eprintln!("Parsed expression: {:?}", #arg_name);
            #body
            Ok(())
        }
    }
    .into()
}

struct ShouldFailAttr {
    expected_errors: Punctuated<Expr, Token![,]>,
}

impl Parse for ShouldFailAttr {
    fn parse(input: ParseStream) -> Result<Self> {
        let expected_errors = Punctuated::parse_terminated(input)?;
        Ok(Self { expected_errors })
    }
}

struct ShouldFailFn {
    name: Ident,
    attrs: Vec<Attribute>,
    program: Block,
}

impl Parse for ShouldFailFn {
    fn parse(input: ParseStream) -> Result<Self> {
        let f: ItemFn = input.parse()?;
        if !f.sig.inputs.is_empty() {
            return Err(Error::new(f.sig.inputs.span(), "expected no arguments"));
        }
        Ok(Self {
            name: f.sig.ident,
            attrs: f.attrs,
            program: *f.block,
        })
    }
}

#[proc_macro_attribute]
pub fn should_fail(attr: TokenStream, item: TokenStream) -> TokenStream {
    let attr = parse_macro_input!(attr as ShouldFailAttr);
    let item = parse_macro_input!(item as ShouldFailFn);

    let ShouldFailAttr { expected_errors } = attr;
    let ShouldFailFn {
        name,
        attrs,
        program,
    } = item;

    quote! {
        #(#attrs)*
        fn #name() -> ::anyhow::Result<()> {
            use ::anyhow::Context as AnyhowContext;
            use ::std::collections::HashSet;
            let p: ::vache_lib::ast::Program = #program;
            let arena = ::vache_lib::Arena::new();
            let config = ::vache_lib::config::Config { input: "", ..::std::default::Default::default() };
            let mut context = ::vache_lib::Context::new(config, &arena);
            match ::vache_lib::check(&mut context, p)? {
                Err(diagnostics) => {
                    let codes = diagnostics.iter().flat_map(|diag| &diag.code).map(|code| &**code).collect::<HashSet<_>>();
                    let expected = [#expected_errors].into_iter().collect::<HashSet<_>>();
                    if codes == expected {
                        Ok(())
                    } else {
                        ::anyhow::bail!("Error codes mismatch. Expected {expected:?}, got {codes:?}");
                    }
                }
                Ok(mut checked) => {
                    ::anyhow::bail!("Program should not typecheck.");
                }
            }
        }
    }
    .into()
}
