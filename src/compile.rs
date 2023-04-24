//! Compiler code.

use proc_macro2::TokenStream;

use crate::tast::{Block, Expr, Fun, Program, SelfVisitor, Stmt};

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
}

impl SelfVisitor for Compiler {
    type Output = TokenStream;

    fn visit_expr(&mut self, e: Expr) -> TokenStream {
        match e {
            Expr::UnitE => quote!(()),
            Expr::IntegerE(i) => {
                if let Some(bounded) = i.to_u64() {
                    quote!(::rug::Integer::from_u64(#bounded))
                } else {
                    let digits = i.to_string_radix(10);
                    quote!(::rug::Integer::from_string_radix(#digits, 10)) // Room for optimization here
                }
            }
            Expr::VarE(v) => {
                let varname = format_ident!("{}", String::from(v));
                quote!(__clone(&#varname))
            }
            Expr::CallE { name, args } => {
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
            Expr::IfE(box cond, box iftrue, box iffalse) => {
                let cond = self.visit_expr(cond);
                let iftrue = self.visit_block(iftrue);
                let iffalse = self.visit_block(iffalse);
                quote! {
                    if #cond #iftrue else #iffalse
                }
            }
            Expr::BlockE(box e) => self.visit_block(e),
            Expr::Copy(_) => todo!(),
            Expr::Own(_) => todo!(),
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
        let body = self.visit_block(f.body);
        quote! {
            pub fn #name() #body
        }
    }

    fn visit_stmt(&mut self, _s: Stmt) -> TokenStream {
        todo!()
    }
}
