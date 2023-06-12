//! Compiler code.

use std::default::default;

use num_traits::ToPrimitive;
use proc_macro2::TokenStream;
use string_builder::Builder as StringBuilder;
use ExprKind::*;
use PlaceKind::*;
use Ty::*;

use crate::{
    tast::{
        Block, Expr, ExprKind, Fun, Mode, Place, PlaceKind, Program, Stmt, Struct, Ty, Varname,
    },
    Context,
};

/// Compiler, that turns our language into source code for an
/// executable language.
///
/// 'c: compilation phase lifetime
/// 'ctx: compiler context lifetime
pub(crate) struct Compiler<'c, 'ctx: 'c> {
    /// Compiler context.
    ctx: &'c mut Context<'ctx>,
}
impl<'c, 'ctx: 'c> Compiler<'c, 'ctx> {
    /// Creates a new compiler.
    pub fn new(ctx: &'c mut Context<'ctx>) -> Self {
        Self { ctx }
    }

    /// Compiles a program in our language into an executable source code.
    pub fn compile(&mut self, p: Program<'ctx>) -> String {
        let tokens = self.visit_program(p);
        let prelude = crate::prelude();
        let file = syn::parse2(quote! {
            #prelude
            #tokens
        })
        .unwrap();
        prettyplease::unparse(&file)
    }

    /// Translate a type into a Rust type.
    pub fn translate_type(ty: &Ty, show_lifetime: bool) -> TokenStream {
        match ty {
            UnitT => quote!(()),
            BoolT => quote!(bool),
            IntT => {
                if show_lifetime {
                    quote!(Cow<'a, ::num_bigint::BigInt>)
                } else {
                    quote!(Cow<::num_bigint::BigInt>)
                }
            }
            StrT => {
                if show_lifetime {
                    quote!(Cow<'a, __String>)
                } else {
                    quote!(Cow<__String>)
                }
            }
            StructT(name) => {
                let name = format_ident!("{}", name);
                if show_lifetime {
                    quote!(Cow<'a, #name>)
                } else {
                    quote!(Cow<#name>)
                }
            }
            ArrayT(ty) => {
                let ty = Self::translate_type(ty, show_lifetime);
                if show_lifetime {
                    quote!(Cow<'a, __Vec<#ty>>)
                } else {
                    quote!(Cow<__Vec<#ty>>)
                }
            }
            IterT(_) => todo!(),
            HoleT => unreachable!(),
        }
    }

    /// Compiles a variable.
    fn visit_var(&mut self, var: impl Into<Varname<'ctx>>) -> TokenStream {
        let ident = format_ident!("{}", var.into().as_str());
        quote!(#ident)
    }

    /// Compiles a place.
    fn visit_place(&mut self, place: Place<'ctx>) -> TokenStream {
        match place.kind {
            VarP(var) => {
                let var = self.visit_var(var);
                match place.mode {
                    Mode::Cloned => quote!(#var.clone()),
                    Mode::Borrowed => quote!(__borrow(&#var)),
                    Mode::SBorrowed => quote!(#var),
                    Mode::MutBorrowed => quote!(#var),
                    Mode::Moved => quote!(#var),
                    Mode::Assigning => quote!(#var),
                }
            }
            IndexP(box array, box index) => {
                let (line, col) = index.span.line_col(self.ctx.files);
                let filename = self.ctx.files.name();
                let codespan = format!("Out of bounds indexing at {filename}:{line}:{col}");
                let array = self.visit_expr(array);
                let index = self.visit_expr(index);
                let index = quote!((#index).to_usize().unwrap());
                match place.mode {
                    Mode::Borrowed => {
                        quote!(__borrow((#array).get(#index).context(#codespan)?))
                    }
                    Mode::SBorrowed => {
                        quote!((#array).get(#index).context(#codespan)?)
                    }
                    Mode::MutBorrowed => {
                        quote!(__borrow_mut((#array).get_mut(#index).context(#codespan)?))
                    }
                    Mode::Cloned => quote!((#array).get(#index).context(#codespan)?.clone()),
                    Mode::Moved => quote!((#array).remove(#index).context(#codespan)?),
                    Mode::Assigning => quote!((*(#array).get_mut(#index).context(#codespan)?)),
                }
            }
            FieldP(box strukt, field) => {
                let strukt = self.visit_expr(strukt);
                let field = format_ident!("{}", field);
                match place.mode {
                    Mode::Borrowed => quote!(__borrow((#strukt).#field.as_ref())),
                    Mode::SBorrowed => unimplemented!(),
                    Mode::MutBorrowed => quote!(__borrow_mut((#strukt).#field.as_mut())),
                    Mode::Cloned => quote!((#strukt).#field.clone().take()),
                    Mode::Moved => quote!((#strukt).#field.take()),
                    Mode::Assigning => quote!(*(#strukt).#field.as_mut()),
                }
            }
        }
    }

    /// Compiles a struct.
    fn visit_struct(&mut self, strukt: Struct) -> TokenStream {
        let name = format_ident!("{}", strukt.name);
        let fields: TokenStream = strukt
            .fields
            .into_iter()
            .map(|(k, ty)| {
                let k = format_ident!("{k}");
                let ty = Self::translate_type(&ty.kind, true);
                quote!(#k: Field<#ty>,)
            })
            .collect();
        quote!(
            #[derive(Debug, Clone)]
            pub struct #name<'a> {
                #fields
            }
        )
    }

    /// Compiles a expression kind.
    fn visit_expr(&mut self, expr: Expr<'ctx>) -> TokenStream {
        match expr.kind {
            UnitE => quote!(()),
            IntegerE(i) => {
                let i = i
                    .to_u128()
                    .expect("Integer {i} is too big to be represented in source code");
                quote!(Cow::Owned(::num_bigint::BigInt::try_from(#i).unwrap()))
            }
            StringE(s) => {
                quote!(Cow::Owned(__String::from(#s)))
            }
            PlaceE(p) => self.visit_place(p),
            StructE { name, fields } => {
                let name = format_ident!("{name}");
                let fields = fields.into_iter().map(|(name, expr)| {
                    let name = format_ident!("{name}");
                    let expr = self.visit_expr(expr);
                    quote!(#name: Field::new(#expr))
                });
                quote!(Cow::Owned(#name {
                    #(#fields),*
                }))
            }
            ArrayE(array) => {
                let items = array.into_iter().map(|item| self.visit_expr(item));
                quote!(Cow::Owned(__Vec(vec![#(#items),*])))
            }
            CallE { name, args } => {
                if name == "print" {
                    let mut builder = StringBuilder::default();

                    // Compute the format string, which is essentially `{} {} {}...`
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
                    let name = match name {
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
                        _ => name.to_string(),
                    };
                    let name = format_ident!("{name}");
                    quote!(#name(#(#args),*)?)
                }
            }
            IfE(box cond, box iftrue, box iffalse) => {
                let cond = self.visit_expr(cond);
                let iftrue = self.visit_block(iftrue);
                let iffalse = self.visit_block(iffalse);

                quote! {
                    if #cond #iftrue #iffalse
                }
            }
            BlockE(box block) => self.visit_block(block),
            HoleE => {
                panic!("Cannot compile code with holes; your code probably even did not typecheck")
            }
        }
    }

    /// Compiles a single statement.
    fn visit_stmt(&mut self, stmt: Stmt<'ctx>) -> TokenStream {
        match stmt {
            Stmt::DeclareS(lhs, rhs) => {
                let name = format_ident!("{}", lhs.name().as_str());
                let ty = Self::translate_type(&lhs.ty, false);
                let rhs = self.visit_expr(rhs);
                quote! {
                    let mut #name: #ty = #rhs;
                }
            }
            Stmt::AssignS(lhs, rhs) => {
                assert!(matches!(lhs.mode, Mode::Assigning));
                let lhs = self.visit_place(lhs);
                let rhs = self.visit_expr(rhs);
                quote! {
                    #lhs = #rhs;
                }
            }
            Stmt::ExprS(expr) => {
                let expr = self.visit_expr(expr);
                quote! {
                    #expr;
                }
            }
            Stmt::WhileS { cond, body } => {
                let cond = self.visit_expr(cond);
                let body = self.visit_block(body);
                quote! {
                    while #cond #body
                }
            }
            Stmt::HoleS => unreachable!(),
        }
    }

    /// Compiles a block.
    fn visit_block(&mut self, block: Block<'ctx>) -> TokenStream {
        let stmts: Vec<_> = block
            .stmts
            .into_iter()
            .map(|stmt| self.visit_stmt(stmt))
            .collect();
        let ret = if let ExprKind::UnitE = block.ret.kind {
            default()
        } else {
            self.visit_expr(block.ret)
        };
        quote! {
            {
                #(#stmts);*
                #ret
            }
        }
    }

    /// Compiles a function.
    fn visit_fun(&mut self, f: Fun<'ctx>) -> TokenStream {
        let name = format_ident!("{}", f.name);
        let params: Vec<TokenStream> = f
            .params
            .into_iter()
            .map(|param| {
                let name = format_ident!("{}", param.name().as_str());
                let ty = Self::translate_type(&param.ty, true);
                quote! {
                    #name: #ty
                }
            })
            .collect();

        let ret_ty = match f.ret_ty.kind {
            UnitT => quote!(::anyhow::Result<()>),
            ty => {
                let ty = Self::translate_type(&ty, true);
                quote!(::anyhow::Result<#ty>)
            }
        };

        let body = self.visit_block(f.body);

        let body = quote! {{
            let res: #ret_ty = try {
                #body
            };
            res
        }};

        if params.is_empty() {
            quote! {
                pub fn #name(#(#params),*) -> #ret_ty #body
            }
        } else {
            quote! {
                pub fn #name<'a>(#(#params),*) -> #ret_ty #body
            }
        }
    }

    /// Compiles a program.
    pub fn visit_program(&mut self, p: Program<'ctx>) -> TokenStream {
        let Program {
            funs,
            structs,
            arena: _,
        } = p;
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
}
