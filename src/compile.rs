//! Compiler code.

use proc_macro2::TokenStream;

use crate::tast::{Block, Expr, Fun, Program, SelfVisitor, Stmt, Ty};
use Expr::*;
use Stmt::*;
use Ty::*;

/// Compiler, that turns our language into source code for an
/// executable language.
pub(crate) struct Compiler {}
impl Compiler {
    /// Creates a new compiler.
    pub fn new() -> Self {
        Self {}
    }

    /// Compiles a program in our language into an executable source code.
    pub fn compile(&mut self, p: Program) -> String {
        let tokens = self.visit_program(p);
        let file = syn::parse2(tokens).unwrap();
        prettyplease::unparse(&file)
    }

    /// Translate a type into a Rust type.
    pub fn translate_type(&self, ty: Ty) -> TokenStream {
        match ty {
            UnitT => quote!(()),
            BoolT => quote!(bool),
            IntT => quote!(Cow<::rug::Integer>),
        }
    }
}

impl SelfVisitor for Compiler {
    type EOutput = TokenStream;
    type SOutput = TokenStream;
    type BOutput = TokenStream;
    type FOutput = TokenStream;
    type POutput = TokenStream;

    fn visit_expr(&mut self, e: Expr) -> TokenStream {
        match e {
            UnitE => quote!(()),
            IntegerE(i) => {
                if let Some(bounded) = i.to_u64() {
                    quote!(Cow(::rug::Integer::from_u64(#bounded)))
                } else {
                    let digits = i.to_string_radix(10);
                    // NB: Room for optimization here
                    quote!(Cow::Owned(::rug::Integer::from_string_radix(#digits, 10)))
                }
            }
            VarE(v) => {
                let varname = format_ident!("{}", String::from(v));
                quote!(__clone(&#varname))
            }
            CallE { name, args } => {
                let args = args.into_iter().map(|arg| self.visit_expr(arg));
                let name = match &*name {
                    "+" => "__add".to_string(),
                    "-" => "__sub".to_string(),
                    "*" => "__mul".to_string(),
                    "/" => "__div".to_string(),
                    "%" => "__rem".to_string(),
                    "<" => "__lt".to_string(),
                    ">" => "__gt".to_string(),
                    "==" => "__eq".to_string(),
                    _ => name,
                };
                let name = format_ident!("{name}");
                quote!(#name(#(#args),*))
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
                let ty = self.translate_type(param.ty);
                quote! {
                    #name: #ty
                }
            })
            .collect();
        let body = self.visit_block(f.body);
        quote! {
            pub fn #name(#(#params),*) #body
        }
    }

    fn visit_stmt(&mut self, s: Stmt) -> TokenStream {
        match s {
            Declare(v, e) => {
                let name = format_ident!("{}", String::from(v.name));
                let e = self.visit_expr(e);
                quote! {
                    let #name = #e;
                }
            }
            Assign(v, e) => {
                let name = format_ident!("{}", String::from(v));
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
            While { cond, body } => {
                let cond = self.visit_expr(cond);
                let body = self.visit_block(body);
                quote! {
                    while #cond #body
                }
            }
        }
    }

    fn visit_program(&mut self, p: Program) -> TokenStream {
        let p: Vec<TokenStream> = p.into_iter().map(|f| self.visit_fun(f)).collect();
        quote!(#(#p)*)
    }
}
