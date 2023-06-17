use glob::glob;
use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, Attribute, Block, Error, FnArg, Ident, ItemFn, LitStr, Pat, Result, Type,
};

struct TestResFn {
    name: Ident,
    body: Block,
    attrs: Vec<Attribute>,
    arg_name: Ident,
    arg_ty: Type,
}

impl Parse for TestResFn {
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
                format!(
                    "expected exactly one argument, found {} in {}",
                    f.sig.inputs.len(),
                    f.sig.ident
                ),
            ))
        }
    }
}

pub fn by_resources(attr: TokenStream, item: TokenStream) -> TokenStream {
    // Get the specified directory path pattern from the attribute
    let path_pat = parse_macro_input!(attr as LitStr).value();

    // Get the corresponding entries thanks to the glob package
    let entries = glob(&path_pat).expect("Failed to read directory");

    // Parse the function item
    let TestResFn {
        name,
        attrs,
        body,
        arg_name,
        arg_ty,
    } = parse_macro_input!(item as TestResFn);

    let mut generated_functions = Vec::new();

    // Iterate over the files in the directory
    for entry in entries.filter_map(|x| x.ok()) {
        let filename = entry
            .file_prefix()
            .unwrap_or_else(|| panic!("Could not get filename of entry {:?}", entry.display()))
            .to_str()
            .expect("Unable to convert filename to string");

        let path = entry.display().to_string();

        // Generate a new function name by appending the file name to the original
        // function's name
        let new_name = format_ident!("{name}__{filename}");

        generated_functions.push(quote! {
            #(#attrs)*
            fn #new_name() -> ::anyhow::Result<()> {
                let #arg_name: #arg_ty = #path.into();
                // Function body
                #body
                Ok(())
            }
        });
    }

    quote! {
        #(#generated_functions)*
    }
    .into() // Because of quote -> TokenStream translation
}
