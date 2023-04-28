//! Compiler code.

use proc_macro2::TokenStream;
use string_builder::Builder as StringBuilder;
use Expr::*;
use Stmt::*;
use Ty::*;

use crate::tast::{Block, Expr, Fun, Program, SelfVisitor, Stmt, Struct, Ty};

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
}

impl SelfVisitor for Compiler {
    type BOutput = TokenStream;
    type EOutput = TokenStream;
    type FOutput = TokenStream;
    type POutput = TokenStream;
    type SOutput = TokenStream;
    type TOutput = TokenStream;

    fn visit_expr(&mut self, e: Expr) -> TokenStream {
        match e {
            UnitE => quote!(()),
            IntegerE(i) => {
                let i = i.to_f64();
                quote!(Cow::Owned(::rug::Integer::from_f64(#i).unwrap()))
            }
            StringE(s) => {
                quote!(Cow::Owned(::std::string::String::from(#s)))
            }
            VarE(v) => {
                let varname = format_ident!("{}", String::from(v.name));
                quote!(#varname)
            }
            CallE { name, args } => {
                if name == "print" {
                    let mut builder = StringBuilder::default();

                    // Compute the format string
                    let mut args_iter = args.iter();
                    if args_iter.next().is_some() {
                        builder.append("{}");
                    }
                    for _ in args_iter {
                        builder.append(" {}");
                    }

                    let args = args.into_iter().map(|arg| self.visit_expr(arg));
                    let fmt_str = builder.string().unwrap();
                    quote!(println!(#fmt_str, #(#args),*))
                } else {
                    let args = args.into_iter().map(|arg| self.visit_expr(arg));
                    let name = match &*name {
                        "+" => "__add".to_string(),
                        "-" => "__sub".to_string(),
                        "*" => "__mul".to_string(),
                        "/" => "__div".to_string(),
                        "%" => "__rem".to_string(),
                        "<" => "__lt".to_string(),
                        ">" => "__gt".to_string(),
                        "<=" => "__le".to_string(),
                        ">=" => "__ge".to_string(),
                        "==" => "__eq".to_string(),
                        "!=" => "__neq".to_string(),
                        _ => name,
                    };
                    let name = format_ident!("{name}");
                    quote!(#name(#(#args),*))
                }
            }
            IfE(box cond, box iftrue, box iffalse) => {
                let cond = self.visit_expr(cond);
                let iftrue = self.visit_block(iftrue);
                let iffalse = self.visit_block(iffalse);
                quote! {
                    if #cond #iftrue else #iffalse
                }
            }
            BlockE(box e) => self.visit_block(e),
            CopyE(box e) => {
                let e = self.visit_expr(e);
                quote!(__clone(&#e))
            }
            OwnE(box e) => {
                let e = self.visit_expr(e);
                quote!(Cow::Owned(#e.into_owned()))
            }
            FieldE(box s, field) => {
                let s = self.visit_expr(s);
                let field = format_ident!("{field}");
                quote!(#s.#field)
            }
            StructE { name, fields } => {
                let name = format_ident!("{name}");
                let fields: TokenStream = fields
                    .into_iter()
                    .map(|(k, e)| {
                        let k = format_ident!("{k}");
                        let e = self.visit_expr(e);
                        quote!(#k: #e,)
                    })
                    .collect();
                quote! (
                    #name {
                        #fields
                    }
                )
            }
        }
    }

    fn visit_block(&mut self, b: Block) -> TokenStream {
        let stmts: Vec<TokenStream> = b.stmts.into_iter().map(|s| self.visit_stmt(s)).collect();
        let ret = self.visit_expr(b.ret);
        quote! {
            {
                #(#stmts)*
                #ret
            }
        }
    }

    fn visit_fun(&mut self, f: Fun) -> TokenStream {
        let name = format_ident!("{}", f.name);
        let params: Vec<TokenStream> = f
            .params
            .into_iter()
            .map(|param| {
                let name = format_ident!("{}", String::from(param.name));
                let ty = self.translate_type(param.ty, true);
                quote! {
                    #name: #ty
                }
            })
            .collect();
        let body = self.visit_block(f.body);
        let ty = self.translate_type(f.ret_ty, true);
        if params.is_empty() {
            quote! {
                pub fn #name(#(#params),*) -> #ty #body
            }
        } else {
            quote! {
                pub fn #name<'a>(#(#params),*) -> #ty #body
            }
        }
    }

    fn visit_stmt(&mut self, s: Stmt) -> TokenStream {
        match s {
            Declare(v, e) => {
                let name = format_ident!("{}", String::from(v.name));
                let e = self.visit_expr(e);
                let ty = self.translate_type(v.ty, false);
                quote! {
                    let mut #name: #ty = #e;
                }
            }
            Assign(v, e) => {
                let name = format_ident!("{}", String::from(v.name));
                let e = self.visit_expr(e);
                quote! {
                    #name = #e;
                }
            }
            ExprS(e) => {
                let e = self.visit_expr(e);
                quote! {
                    #e;
                }
            }
            WhileS { cond, body } => {
                let cond = self.visit_expr(cond);
                let body = self.visit_block(body);
                quote! {
                    while #cond #body
                }
            }
        }
    }

    fn visit_program(&mut self, p: Program) -> TokenStream {
        let Program { funs, structs } = p;
        let funs: Vec<TokenStream> = funs.into_values().map(|f| self.visit_fun(f)).collect();
        let structs: Vec<TokenStream> = structs
            .into_values()
            .map(|s| self.visit_struct(s))
            .collect();
        quote! {
            #(#structs)*
            #(#funs)*
        }
    }

    fn visit_struct(&mut self, strukt: Struct) -> TokenStream {
        let name = format_ident!("{}", strukt.name);
        let fields: TokenStream = strukt
            .fields
            .into_iter()
            .map(|(k, ty)| {
                let k = format_ident!("{k}");
                let ty = self.translate_type(ty, true);
                quote!(#k: #ty,)
            })
            .collect();
        quote!(
            #[derive(Debug, Clone)]
            pub struct #name<'a> {
                #fields
            }
        )
    }
}
