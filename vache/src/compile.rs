//! Compiler code.

use std::default::default;

use itertools::Itertools;
use num_traits::ToPrimitive;
use proc_macro2::TokenStream;
use string_builder::Builder as StringBuilder;
use ExprKind::*;
use PlaceKind::*;
use Ty::*;

use crate::tast::{
    Block, Enum, Expr, ExprKind, Fun, Mode, Place, PlaceKind, Program, Stmt, Struct, Ty, Varname,
};
use crate::Context;

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
                    quote!(Cow<'a, __Integer>)
                } else {
                    quote!(Cow<__Integer>)
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
            EnumT(name) => {
                let name = format_ident!("{}", name);
                if show_lifetime {
                    quote!(Cow<'a, #name>)
                } else {
                    quote!(Cow<#name>)
                }
            }
            VarT(name) => {
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
            IterT(ty) => {
                let ty = Self::translate_type(ty, show_lifetime);
                if show_lifetime {
                    quote!(Cow<'a, __Range<#ty>>)
                } else {
                    quote!(Cow<__Range<#ty>>)
                }
            }
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
                    Mode::Assigning => quote!((#strukt).#field),
                }
            }
        }
    }

    /// Compiles a struct.
    fn visit_struct(&mut self, strukt: &Struct) -> TokenStream {
        let name = format_ident!("{}", strukt.name);
        let fields: TokenStream = strukt
            .fields
            .iter()
            .map(|(field, ty)| {
                let field = format_ident!("{field}");
                let ty = Self::translate_type(&ty.kind, true);
                quote!(#field: Field<#ty>,)
            })
            .collect();
        quote!(
            #[derive(Debug, Clone)]
            pub struct #name<'a> {
                #fields
            }
        )
    }

    /// Compiles an `enum`.
    fn visit_enum(&mut self, enun: &Enum) -> TokenStream {
        let name = format_ident!("{}", enun.name);
        let variants: TokenStream = enun
            .variants
            .iter()
            .map(|(variant, args)| {
                let variant = format_ident!("{variant}");
                if args.is_empty() {
                    quote!(#variant,)
                } else {
                    let args = args.iter().map(|arg| Self::translate_type(&arg.kind, true));
                    quote!(#variant(#(#args),*),)
                }
            })
            .collect();

        let variants_display: Vec<_> = enun
            .variants
            .iter()
            .map(|(&variant, args)| {
                let variant_str = variant;
                let variant_ident = format_ident!("{}", variant);
                if !args.is_empty() {
                    let args_lhs1 = (0..args.len()).map(|i| format_ident!("__arg{i}"));
                    let args_lhs2 = args_lhs1.clone();
                    let variant_str = format!("{variant_str}({})", (0..args.len()).map(|_| "{}").join(", "));
                    quote!(#name::#variant_ident(#(#args_lhs1),*) => write!(f, #variant_str, #(#args_lhs2),*),)
                } else {
                    quote!(#name::#variant_ident => write!(f, #variant_str),)
                }
            })
            .collect();

        quote!(
            #[derive(Debug, Clone)]
            pub enum #name<'a> {
                #variants
            }

            impl<'a> ::std::fmt::Display for #name<'a> {
                fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                    match self {
                        #(#variants_display)*
                    }
                }
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
                quote!(Cow::Owned( __Integer::try_from(#i).unwrap()))
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
                if name.name == "print" {
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
                    let name = match name.name {
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
                        _ => name.name.to_string(),
                    };
                    let name = format_ident!("{name}");
                    quote!(#name(#(#args),*)?)
                }
            }
            VariantE {
                enun,
                variant,
                args,
            } => {
                let enun = format_ident!("{enun}");
                let variant = format_ident!("{variant}");

                // Special case for unit variant, do not show the `()`
                if args.is_empty() {
                    quote!(Cow::Owned(#enun::#variant))
                } else {
                    let args = args.into_iter().map(|arg| self.visit_expr(arg));
                    quote!(Cow::Owned(#enun::#variant(#(#args),*)))
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
            BlockE(box block) => self.visit_block(block),
            HoleE => {
                panic!("Cannot compile code with holes; your code probably even did not typecheck")
            }
            RangeE(box start, box end) => {
                let start = self.visit_expr(start);
                let end = self.visit_expr(end);
                quote!(Cow::Owned(__Range::new(#start,#end)))
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
                // Different output if we have a field at lhs
                let is_field = matches!(lhs.kind, FieldP(..));

                let lhs = self.visit_place(lhs);
                let rhs = self.visit_expr(rhs);

                // For fields, add a `Field` wrapper
                if is_field {
                    quote! {
                        #lhs = Field::new(#rhs);
                    }
                } else {
                    quote! {
                        #lhs = #rhs;
                    }
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

            Stmt::ForS { item, iter, body } => {
                let item = format_ident!("{}", item.name().as_str());
                let iter = self.visit_expr(iter);
                let body = self.visit_block(body);
                quote! {
                    for #item in #iter #body
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
                    mut #name: #ty
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
            enums,
            arena: _,
        } = p;
        let funs: Vec<TokenStream> = funs.into_values().map(|f| self.visit_fun(f)).collect();
        let structs: Vec<TokenStream> = structs.values().map(|s| self.visit_struct(s)).collect();
        let enums: Vec<TokenStream> = enums.values().map(|s| self.visit_enum(s)).collect();
        quote! {
            #(#structs)*
            #(#enums)*
            #(#funs)*
        }
    }
}
