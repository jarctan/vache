use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, Attribute, Block, Error, FnArg, Ident, ItemFn, LitStr, Pat, Result, Token,
    Type,
};

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

            let #arg_name: #arg_ty = match parse_rule(&mut ctx, crate::grammar::Rule::#rule) {
                Ok(el) => el,
                Err(err) => {
                    ctx.reporter.display()?;
                    bail!(err);
                }
            };
            eprintln!("Parsed expression: {:?}", #arg_name);
            #body
            Ok(())
        }
    }
    .into()
}
