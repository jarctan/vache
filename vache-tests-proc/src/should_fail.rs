use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{parse_macro_input, Attribute, Block, Error, Expr, Ident, ItemFn, Result, Token};

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
            match ::vache_lib::typecheck(&mut context, p)? {
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
