//! Compiler code.

use std::default::default;

use itertools::Itertools;
use num_traits::{One, ToPrimitive, Zero};
use proc_macro2::TokenStream;
use string_builder::Builder as StringBuilder;
use ExprKind::*;
use PatKind::*;
use PlaceKind::*;
use StmtKind::*;
use Ty::*;

use crate::lft_name_gen::LftGenerator;
use crate::tast::{
    Arg, ArgKind, Block, Enum, Expr, ExprKind, Fun, FunSig, LhsMode, LhsPlace, LineCol, Mode,
    Namespaced, Pat, PatKind, Place, PlaceKind, Program, Stmt, StmtKind, Struct, Trait, Ty, TyVar,
    Varname,
};
use crate::Context;

/// Wrapper type.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Wrapper {
    /// Wrap in a `Vec`.
    Vec,
    /// `Cow`.
    Cow,
    /// Variable.
    Var,
}

impl From<Wrapper> for TokenStream {
    fn from(wrapper: Wrapper) -> Self {
        match wrapper {
            Wrapper::Cow => quote!(Cow),
            Wrapper::Var => quote!(Var),
            Wrapper::Vec => quote!(__Vec),
        }
    }
}

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
    ///
    /// * wrapper: name of the wrapper type (`Cow`, `Var`, etc.).
    pub fn translate_type(ty: &Ty, lifetimes: &[&syn::Lifetime], wrapper: Wrapper) -> TokenStream {
        let lft = quote!(#(#lifetimes,)*);
        let wrapper = TokenStream::from(wrapper);
        match ty {
            UnitT => quote!(()),
            BoolT => quote!(#wrapper<#lft bool>),
            IntT => quote!(#wrapper<#lft __Integer>),
            UsizeT => quote!(#wrapper<#lft u64>),
            StrT => quote!(#wrapper<#lft __String>),
            StructT(name) => {
                let name = format_ident!("{}", name);
                quote!(#wrapper<#lft #name>)
            }
            EnumT(name) => {
                let name = format_ident!("{}", name);
                quote!(#wrapper<#lft #name>)
            }
            VarT(TyVar::Named(name)) => {
                let name = format_ident!("{}", name);
                quote!(#wrapper<#lft #name>)
            }
            VarT(TyVar::Gen(..)) => {
                // We should not have free, compiler-generated type variables at this stage
                unreachable!()
            }
            ArrayT(ty) => {
                let ty = Self::translate_type(
                    ty,
                    // Note: only transmit the second lifetime
                    if !lifetimes.is_empty() {
                        &lifetimes[lifetimes.len() - 1..]
                    } else {
                        &[]
                    },
                    Wrapper::Vec,
                );
                quote!(#wrapper<#lft #ty>)
            }
            TupleT(items) => {
                let items = items
                    .iter()
                    .map(|item| Self::translate_type(item, lifetimes, Wrapper::Cow));
                quote!(#wrapper<#lft (#(#items),*)>)
            }
            IterT(ty) => {
                let ty = Self::translate_type(ty, lifetimes, Wrapper::Cow);
                quote!(#wrapper<#lft __Range<#ty>>)
            }
        }
    }

    /// Compiles a variable.
    fn visit_var(&mut self, var: impl Into<Varname<'ctx>>) -> TokenStream {
        let ident = format_ident!("{}", var.into().as_str());
        quote!(#ident)
    }

    /// Compiles a left-hand side place.
    fn visit_lhs_place(
        &mut self,
        place: &LhsPlace<'ctx>,
        f_ret_struct: &syn::Ident,
    ) -> TokenStream {
        match &place.kind {
            VarP(var) => {
                let var = self.visit_var(var);
                let ty = Self::translate_type(&place.ty, &[], Wrapper::Cow);
                match place.mode {
                    LhsMode::Assigning => quote!(*#var.as_cow()),
                    LhsMode::Declaring => quote!(let mut #var: #ty),
                }
            }
            IndexP(box array, box index) => {
                let LineCol { line, col } = index.span.start_line_col(self.ctx.files);
                let filename = self.ctx.files.name();
                let codespan = format!("Out of bounds indexing at {filename}:{line}:{col}");
                let array = self.visit_expr(array, Wrapper::Cow, f_ret_struct);
                let index = self.visit_expr(index, Wrapper::Cow, f_ret_struct);
                let index = quote!((#index).to_usize().context(#codespan)?);
                match place.mode {
                    LhsMode::Assigning => quote!(*(#array).get_mut(#index).context(#codespan)?),
                    LhsMode::Declaring => unreachable!(),
                }
            }
            FieldP(box strukt, field) => {
                let strukt = self.visit_expr(strukt, Wrapper::Cow, f_ret_struct);
                let field = format_ident!("{}", field);
                match place.mode {
                    LhsMode::Assigning => quote!((#strukt).#field),
                    LhsMode::Declaring => unreachable!(),
                }
            }
            ElemP(box tuple, elem) => {
                let tuple = self.visit_expr(tuple, Wrapper::Cow, f_ret_struct);
                let elem = syn::Index::from(*elem);
                match place.mode {
                    LhsMode::Assigning => quote!((#tuple).#elem),
                    LhsMode::Declaring => unreachable!(),
                }
            }
        }
    }

    /// Compiles a (rhs) place.
    fn visit_rhs_place(
        &mut self,
        place: &Place<'ctx>,
        wrapper: Wrapper,
        f_ret_struct: &syn::Ident,
    ) -> TokenStream {
        match &place.kind {
            VarP(var) => {
                let var = self.visit_var(var);
                match place.mode {
                    Mode::Cloned => match wrapper {
                        Wrapper::Cow => quote!(Cow::clone(&#var)),
                        Wrapper::Var => quote!(Var::Owned(Cow::clone(&#var))),
                        Wrapper::Vec => unreachable!(),
                    },
                    Mode::Borrowed => match wrapper {
                        Wrapper::Cow => quote!(Cow::borrow(&#var)),
                        Wrapper::Var => quote!(Var::Owned(Cow::borrow(&#var))),
                        Wrapper::Vec => unreachable!(),
                    },
                    Mode::MutBorrowed => match wrapper {
                        Wrapper::Cow => unreachable!(),
                        Wrapper::Var => quote!(Var::Mut(&mut #var)),
                        Wrapper::Vec => unreachable!(),
                    },
                    Mode::SBorrowed => quote!((&#var)),
                    Mode::SMutBorrowed => quote!((&mut #var)),
                    Mode::Moved => match wrapper {
                        Wrapper::Cow => quote!(Cow::take(&mut #var)),
                        Wrapper::Var => quote!(Var::Owned(Cow::take(&mut #var))),
                        Wrapper::Vec => unreachable!(),
                    },
                }
            }
            IndexP(box array, box index) => {
                let LineCol { line, col } = index.span.start_line_col(self.ctx.files);
                let filename = self.ctx.files.name();
                let codespan = format!("Out of bounds indexing at {filename}:{line}:{col}");
                let array = self.visit_expr(array, Wrapper::Cow, f_ret_struct);
                let index = self.visit_expr(index, Wrapper::Cow, f_ret_struct);
                let index = quote!((#index).to_usize().context(#codespan)?);
                match place.mode {
                    Mode::Borrowed => match wrapper {
                        Wrapper::Var => {
                            quote!(Var::Owned(Cow::borrow((#array).get(#index).context(#codespan)?)))
                        }
                        Wrapper::Cow => {
                            quote!(Cow::borrow((#array).get(#index).context(#codespan)?))
                        }
                        Wrapper::Vec => unreachable!(),
                    },
                    Mode::SBorrowed => quote!((#array).get(#index).context(#codespan)?),
                    Mode::SMutBorrowed => quote!(#array.get_mut(#index).context(#codespan)?),
                    Mode::MutBorrowed => match wrapper {
                        Wrapper::Var => {
                            quote!(Var::Mut((#array).get_mut(#index).context(#codespan)?))
                        }
                        Wrapper::Cow => unimplemented!(),
                        Wrapper::Vec => unreachable!(),
                    },
                    Mode::Cloned => match wrapper {
                        Wrapper::Var => {
                            quote!(Var::Owned(Cow::clone((#array).get(#index).context(#codespan)?)))
                        }
                        Wrapper::Cow => {
                            quote!(Cow::clone(&*(#array).get(#index).context(#codespan)?))
                        }
                        Wrapper::Vec => unreachable!(),
                    },
                    Mode::Moved => match wrapper {
                        Wrapper::Var => {
                            quote!(Var::Owned(#array.remove(#index).context(#codespan)?))
                        }
                        Wrapper::Cow => {
                            quote!(Cow::remove(&mut #array.take(), #index).context(#codespan)?)
                        }
                        Wrapper::Vec => unreachable!(),
                    },
                }
            }
            FieldP(box strukt, field) => {
                let strukt = self.visit_expr(strukt, Wrapper::Cow, f_ret_struct);
                let field = format_ident!("{}", field);
                match place.mode {
                    Mode::Borrowed => match wrapper {
                        Wrapper::Var => quote!(Var::Owned(Cow::borrow(&(#strukt).#field))),
                        Wrapper::Cow => quote!(Cow::borrow(&(#strukt).#field)),
                        Wrapper::Vec => unreachable!(),
                    },
                    Mode::SBorrowed => unimplemented!(),
                    Mode::MutBorrowed => match wrapper {
                        Wrapper::Var => quote!(Var::Mut(&mut (#strukt).#field)),
                        Wrapper::Cow => unreachable!(),
                        Wrapper::Vec => unreachable!(),
                    },
                    Mode::SMutBorrowed => unimplemented!(),
                    Mode::Cloned => match wrapper {
                        Wrapper::Var => quote!(Var::Owned(Cow::clone(&(#strukt).#field))),
                        Wrapper::Cow => quote!(Cow::clone(&(#strukt).#field)),
                        Wrapper::Vec => unreachable!(),
                    },
                    Mode::Moved => match wrapper {
                        Wrapper::Var => quote!(Var::Owned(Cow::take(&mut (#strukt).#field))),
                        Wrapper::Cow => quote!(Cow::take(&mut (#strukt).#field)),
                        Wrapper::Vec => unreachable!(),
                    },
                }
            }
            ElemP(box tuple, elem) => {
                let tuple = self.visit_expr(tuple, Wrapper::Cow, f_ret_struct);
                let elem = syn::Index::from(*elem);
                match place.mode {
                    Mode::Borrowed => match wrapper {
                        Wrapper::Var => quote!(Var::borrow(&(#tuple).#elem)),
                        Wrapper::Cow => quote!(__borrow(&(#tuple).#elem)),
                        Wrapper::Vec => unreachable!(),
                    },
                    Mode::SBorrowed => unimplemented!(),
                    Mode::MutBorrowed => match wrapper {
                        Wrapper::Var => quote!(Var::borrow_mut(&mut (#tuple).#elem)),
                        Wrapper::Cow => quote!(__borrow_mut(&mut *(#tuple).#elem)),
                        Wrapper::Vec => unreachable!(),
                    },
                    Mode::SMutBorrowed => unimplemented!(),
                    Mode::Cloned => match wrapper {
                        Wrapper::Var => quote!(Var::clone(&(#tuple).#elem)),
                        Wrapper::Cow => quote!(Cow::clone(&(#tuple).#elem)),
                        Wrapper::Vec => unreachable!(),
                    },
                    Mode::Moved => match wrapper {
                        Wrapper::Var => quote!(Var::Owned(Cow::take(&mut (#tuple).#elem))),
                        Wrapper::Cow => quote!(Cow::take(&mut (#tuple).#elem)),
                        Wrapper::Vec => unreachable!(),
                    },
                }
            }
        }
    }

    /// Compiles a struct.
    fn visit_struct(&mut self, strukt: &Struct<'ctx>) -> TokenStream {
        let name = format_ident!("{}", strukt.name);

        let ty_params = strukt.ty_params.iter().map(|param| match param {
            TyVar::Named(name) => {
                let name = format_ident!("{}", name);
                quote!(#name: ::std::fmt::Debug + ::std::clone::Clone)
            }
            TyVar::Gen(..) => unreachable!(),
        });

        // Generate one lifetime for our structure.
        let mut lft_generator = LftGenerator::new();
        let b = lft_generator.generate();

        let fields: TokenStream = strukt
            // Note: we use the same order as in the code, so that the `Debug` of it is
            // deterministic
            .fields_order
            .iter()
            .map(|field| {
                // Unwrap ok because the name was retrieved from `fields_order`
                let ty = strukt.fields.get(field).unwrap();
                let field = format_ident!("{field}");
                let ty = Self::translate_type(&ty.kind, &[&b], Wrapper::Cow);
                quote!(#field: #ty,)
            })
            .collect();

        quote!(
            #[derive(Debug, Clone)]
            pub struct #name<#b, #(#ty_params,)*> {
                #fields
            }
        )
    }

    /// Compiles an `enum`.
    fn visit_enum(&mut self, enun: &Enum<'ctx>) -> TokenStream {
        let name = format_ident!("{}", enun.name);

        // Generate one lifetime for our structure.
        let mut lft_generator = LftGenerator::new();
        let a = lft_generator.generate();

        let variants: TokenStream = enun
            .variants
            .iter()
            .map(|(variant, args)| {
                let variant = format_ident!("{variant}");
                if args.is_empty() {
                    quote!(#variant,)
                } else {
                    let args = args
                        .iter()
                        .map(|arg| Self::translate_type(&arg.kind, &[&a], Wrapper::Cow));
                    quote!(#variant(#(#args),*),)
                }
            })
            .collect();

        // Type parameters with trait bounds
        let ty_params_w_bounds: Vec<_> = enun
            .ty_params
            .iter()
            .map(|param| match param {
                TyVar::Named(name) => {
                    let name = format_ident!("{}", name);
                    quote!(#name: ::std::fmt::Debug + ::std::clone::Clone)
                }
                TyVar::Gen(..) => unreachable!(),
            })
            .collect();

        quote!(
            #[derive(Debug, Clone)]
            pub enum #name<#a, #(#ty_params_w_bounds,)*> {
                #variants
            }
        )
    }

    /// Compiles a struct.
    fn visit_trait(&mut self, trayt: &Trait<'ctx>) -> TokenStream {
        let name = format_ident!("{}", trayt.name);

        let ty_params = trayt.ty_params.iter().map(|param| match param {
            TyVar::Named(name) => {
                let name = format_ident!("{}", name);
                quote!(#name: ::std::fmt::Debug + ::std::clone::Clone)
            }
            TyVar::Gen(..) => unreachable!(),
        });

        let (structs, methods): (Vec<_>, Vec<_>) = trayt
            .methods
            .values()
            .map(|fun_sig| {
                self.visit_fun_sig(
                    fun_sig,
                    false,
                    format_ident!("__{}__{}Ret", trayt.name, fun_sig.name),
                )
            })
            .unzip();

        quote!(
            #(#structs)*
            pub trait #name<#(#ty_params,)*> where Self: ::std::clone::Clone {
                #(#methods)*
            }
        )
    }

    /// Compiles a expression.
    fn visit_expr(
        &mut self,
        expr: &Expr<'ctx>,
        wrapper: Wrapper,
        f_ret_struct: &syn::Ident,
    ) -> TokenStream {
        let wrapper_tkn = TokenStream::from(wrapper);
        match &expr.kind {
            UnitE => quote!(()),
            BoolE(b) => quote!(#wrapper_tkn::owned(#b)),
            UsizeE(i) => quote!(#wrapper_tkn::owned(#i)),
            IntegerE(i) => {
                // Special shortcuts for zero and one because they are so common
                if i.is_zero() {
                    quote!(#wrapper_tkn::owned(__Integer::zero()))
                } else if i.is_one() {
                    quote!(#wrapper_tkn::owned(__Integer::one()))
                } else {
                    let i = i
                        .to_u128()
                        .expect("Integer {i} is too big to be represented in source code");
                    quote!(#wrapper_tkn::owned(__Integer::try_from(#i).unwrap()))
                }
            }
            StringE(s) => {
                quote!(#wrapper_tkn::owned(__String::from(#s)))
            }
            PlaceE(p) => self.visit_rhs_place(p, wrapper, f_ret_struct),
            StructE { name, fields } => {
                let name = format_ident!("{name}");
                let fields = fields.iter().map(|(name, expr)| {
                    let name = format_ident!("{name}");
                    let expr = self.visit_expr(expr, Wrapper::Cow, f_ret_struct);
                    quote!(#name: #expr)
                });
                quote!(#wrapper_tkn::owned(#name {
                    #(#fields),*
                }))
            }
            ArrayE(array) => {
                let items = array
                    .iter()
                    .map(|item| self.visit_expr(item, Wrapper::Cow, f_ret_struct));
                quote!(#wrapper_tkn::owned(__Vec(vec![#(#items),*])))
            }
            TupleE(items) => {
                let items = items
                    .iter()
                    .map(|item| self.visit_expr(item, Wrapper::Cow, f_ret_struct));
                quote!(#wrapper_tkn::owned((#(#items),*)))
            }
            CallE { name, args } => {
                if name.name == "debug" {
                    let mut builder = StringBuilder::default();

                    // Compute the format string, which is essentially `{} {} {}...`
                    let mut args_iter = args.iter();
                    if args_iter.next().is_some() {
                        builder.append("{:?}");
                    }
                    for _ in args_iter {
                        builder.append(" {:?}");
                    }

                    let args = args.iter().map(|arg| {
                        // Note: All arguments for print are `pass-by-value` (Cow)
                        self.visit_arg(arg, Wrapper::Cow, f_ret_struct)
                    });
                    let fmt_str = builder.string().unwrap();
                    quote!(println!(#fmt_str, #(#args),*))
                } else {
                    // Get the final automatic bindings
                    let bindings = args
                        .iter()
                        .flat_map(|arg| arg.as_binding())
                        .map(|(_, to)| {
                            let place = self.visit_lhs_place(to, f_ret_struct);
                            quote!(#place = __res.1.a.unwrap();)
                        })
                        .collect_vec();

                    // Tokenize the arguments and the real name of the function
                    let mut prelude = vec![];
                    let args = args
                        .iter()
                        .enumerate()
                        .map(|(i, arg)| {
                            if arg.byref() {
                                // `Var` for pass-by-reference
                                self.visit_arg(arg, Wrapper::Var, f_ret_struct)
                            } else {
                                // `Cow` for pass-by-value
                                let arg = self.visit_arg(arg, Wrapper::Cow, f_ret_struct);
                                let tmp = format_ident!("__n{i}");
                                prelude.push(quote!(let #tmp = #arg;));
                                quote!(#tmp)
                            }
                        })
                        .collect_vec();

                    // Tokenize the real name of the function
                    let name = match name.name {
                        "+" => quote!(__Add::add),
                        "-" => quote!(__Sub::sub),
                        "*" => quote!(__Mul::mul),
                        "/" => quote!(__Div::div),
                        "%" => quote!(__Rem::rem),
                        "<" => quote!(__lt),
                        ">" => quote!(__gt),
                        "<=" => quote!(__le),
                        ">=" => quote!(__ge),
                        "==" => quote!(__PartialEq::eq),
                        "!=" => quote!(__PartialEq::ne),
                        "&&" => quote!(__and),
                        "||" => quote!(__or),
                        "!" => quote!(__not),
                        "assert" => quote!(__assert),
                        "rand" => quote!(__rand),
                        "push" => quote!(__Vec::push),
                        "len" => quote!(__Vec::len),
                        _ => {
                            let ident = self.visit_namespaced(name);
                            quote!(#ident)
                        }
                    };

                    // If needed, wrap in a `final_res`
                    let final_res = match wrapper {
                        Wrapper::Var => quote!(Var::Owned(__res.0)),
                        Wrapper::Cow => quote!(__res.0),
                        Wrapper::Vec => unreachable!(),
                    };

                    quote!({
                        let __res = {
                            #(#prelude)*
                            #name(#(#args),*)?
                        };
                        #(#bindings)*
                        #final_res
                    })
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
                    quote!(#wrapper_tkn::owned(#enun::#variant))
                } else {
                    let args = args
                        .iter()
                        .map(|arg| self.visit_expr(arg, Wrapper::Cow, f_ret_struct));
                    quote!(#wrapper_tkn::owned(#enun::#variant(#(#args),*)))
                }
            }
            IfE(box cond, box iftrue, box iffalse) => {
                let cond = self.visit_expr(cond, Wrapper::Cow, f_ret_struct);
                let iftrue = self.visit_block(iftrue, wrapper, f_ret_struct);
                let iffalse = self.visit_block(iffalse, wrapper, f_ret_struct);

                quote! {
                    if *(#cond) #iftrue else #iffalse
                }
            }
            MatchE(box matched, branches) => {
                let matched = self.visit_expr(matched, Wrapper::Cow, f_ret_struct);
                let branches = branches.iter().map(|(pattern, branch)| {
                    let (pattern, intros) = self.visit_pattern(pattern);
                    let branch = self.visit_expr(branch, wrapper, f_ret_struct);
                    quote!(| #pattern => {
                        #intros
                        #branch
                    })
                });

                quote! {
                    match &**#matched {
                        #(#branches)*
                        _ => ::anyhow::bail!("Matching failed"),
                    }
                }
            }
            BlockE(box block) => self.visit_block(block, wrapper, f_ret_struct),
            HoleE => {
                panic!("Cannot compile code with holes; your code probably even did not typecheck")
            }
            RangeE(box start, box end) => {
                let start = self.visit_expr(start, Wrapper::Cow, f_ret_struct);
                let end = self.visit_expr(end, Wrapper::Cow, f_ret_struct);
                quote!(#wrapper_tkn::owned(__Range::new(#start,#end)))
            }
        }
    }

    /// Compiles a function argument.
    fn visit_arg(
        &mut self,
        arg: &Arg<'ctx>,
        wrapper: Wrapper,
        f_ret_struct: &syn::Ident,
    ) -> TokenStream {
        match &arg.kind {
            ArgKind::Standard(arg) => self.visit_expr(arg, wrapper, f_ret_struct),
            ArgKind::InPlace(place) => self.visit_rhs_place(place, wrapper, f_ret_struct),
            ArgKind::Binding(arg, _) => self.visit_expr(arg, wrapper, f_ret_struct),
        }
    }

    /// Compiles a pattern.
    ///
    /// Returns the compiled pattern and the necessary introductions in the
    /// branch.
    ///
    /// These introductions are a workaround so that the items of a matched
    /// elements be `Cow` and not `&Cow` (and mutable Cow in particular).
    #[allow(clippy::only_used_in_recursion)]
    fn visit_pattern(&mut self, pat: &Pat<'ctx>) -> (TokenStream, TokenStream) {
        match &pat.kind {
            BoolM(b) => (quote!(#b), quote!()),
            IntegerM(i) => {
                let i = i
                    .to_u128()
                    .expect("Integer {i} is too big to be represented in source code");
                (quote!(#i), quote!())
            }
            StringM(s) => (quote!(#s), quote!()),
            IdentM(i) => {
                let i = format_ident!("{}", i.name().as_str());
                (quote!(#i), quote!(let mut #i = Cow::clone(&#i);))
            }
            VariantM {
                enun,
                variant,
                args,
            } => {
                let enun = format_ident!("{enun}");
                let variant = format_ident!("{variant}");
                let (args, intros): (Vec<_>, Vec<_>) =
                    args.iter().map(|arg| self.visit_pattern(arg)).unzip();
                (quote!(#enun::#variant(#(#args)*)), quote!(#(#intros)*))
            }
        }
    }

    /// Compiles a single statement.
    fn visit_stmt(&mut self, stmt: &Stmt<'ctx>, f_ret_struct: &syn::Ident) -> TokenStream {
        match &stmt.kind {
            AssignS(lhs, rhs) => {
                let rhs = self.visit_expr(rhs, Wrapper::Cow, f_ret_struct);
                let lhs = self.visit_lhs_place(lhs, f_ret_struct);

                // For fields, add a `Field` wrapper
                quote! {
                    #lhs = #rhs;
                }
            }
            SwapS(place1, place2) => {
                let place1 = self.visit_rhs_place(place1, Wrapper::Cow, f_ret_struct);
                let place2 = self.visit_rhs_place(place2, Wrapper::Cow, f_ret_struct);
                quote! {
                    let __p1: *mut _ = #place1;
                    let __p2: *mut _ = #place2;
                    unsafe {
                        ::std::ptr::swap(__p1, __p2);
                    }
                }
            }
            ExprS(expr) => {
                let expr = self.visit_expr(expr, Wrapper::Cow, f_ret_struct);
                quote! {
                    #expr;
                }
            }
            WhileS { cond, body } => {
                let cond = self.visit_expr(cond, Wrapper::Cow, f_ret_struct);
                let body = self.visit_block(body, Wrapper::Cow, f_ret_struct);
                quote! {
                    while *#cond #body
                }
            }

            ForS { item, iter, body } => {
                let item = format_ident!("{}", item.name().as_str());
                let iter = self.visit_expr(iter, Wrapper::Cow, f_ret_struct);
                let body = self.visit_block(body, Wrapper::Cow, f_ret_struct);
                quote! {
                    for #item in #iter #body
                }
            }
            HoleS => unreachable!(),
            BreakS => quote!(break;),
            ContinueS => quote!(continue;),
            ReturnS(ret) => {
                let ret = self.visit_expr(ret, Wrapper::Cow, f_ret_struct);
                quote!(return __Ret::ok(#ret, #f_ret_struct { });)
            }
        }
    }

    /// Compiles a block.
    fn visit_block(
        &mut self,
        block: &Block<'ctx>,
        wrapper: Wrapper,
        f_ret_struct: &syn::Ident,
    ) -> TokenStream {
        let stmts: Vec<_> = block
            .stmts
            .iter()
            .map(|stmt| self.visit_stmt(stmt, f_ret_struct))
            .collect();
        let ret = if let ExprKind::UnitE = block.ret.kind {
            default()
        } else {
            self.visit_expr(&block.ret, wrapper, f_ret_struct)
        };
        quote! {
            {
                #(#stmts);*
                #ret
            }
        }
    }

    /// Compiles a function signature.
    /// I.e., `fn test(x: T) -> T;`.
    ///
    /// * `mut_params`: mutable parameters
    /// * `ret_struct_name`: name of the structure to hold the result of the
    ///   in-place parameters
    ///
    /// Returns:
    /// * the structure to hold the result of the in-place parameters
    /// * the function signature
    fn visit_fun_sig(
        &mut self,
        f: &FunSig<'ctx>,
        mut_params: bool,
        ret_struct_name: syn::Ident,
    ) -> (TokenStream, TokenStream) {
        let name = format_ident!("{}", f.name);

        let ty_params = f.ty_params.iter().map(|param| match param {
            TyVar::Named(name) => {
                let name = format_ident!("{}", name);
                quote!(#name: ::std::fmt::Debug + ::std::clone::Clone)
            }
            TyVar::Gen(..) => unreachable!(),
        });

        // Generate two lifetimes for our structure.
        let mut lft_generator = LftGenerator::new();
        let a = lft_generator.generate();
        let b = lft_generator.generate();

        // Tokenize all the parameters
        let params: Vec<TokenStream> = f
            .params
            .iter()
            .map(|param| {
                let name = format_ident!("{}", param.var.name().as_str());
                // `Var` for pass-by-reference, `Cow` for pass-by-value
                let ty = if param.byref {
                    Self::translate_type(&param.ty(), &[&a, &b], Wrapper::Var)
                } else {
                    Self::translate_type(&param.ty(), &[&b], Wrapper::Cow)
                };
                if mut_params {
                    quote!(mut #name: #ty)
                } else {
                    quote!(#name: #ty)
                }
            })
            .collect();

        // Tokenize the user-facing return type of the function
        let ret_ty = match f.ret_ty.kind {
            UnitT => quote!(()),
            ty => Self::translate_type(&ty, &[&b], Wrapper::Cow), /* DO NOT add a `Var` on the
                                                                   * return
                                                                   * type!! */
        };

        // Get the list of in-place parameters
        let ret_params: Vec<_> = f.params.iter().filter(|param| param.byref).collect();
        // Their type
        let ret_params_ty: Vec<_> = ret_params
            .iter()
            .map(|param| Self::translate_type(&param.ty(), &[&b], Wrapper::Cow))
            .collect();
        // Their name
        let ret_vars: Vec<_> = ret_params
            .iter()
            .map(|param| format_ident!("{}", param.var.name().as_str()))
            .collect();

        // Create a tailored structure to hold the result of the in-place parameters
        let ret_struct_full_name = if ret_vars.is_empty() {
            quote!(#ret_struct_name)
        } else {
            quote!(#ret_struct_name<#b>)
        };
        let ret_struct_fields = ret_vars
            .iter()
            .zip(ret_params_ty.iter())
            .map(|(name, ty)| quote!(#name: Option<#ty>));
        let ret_struct = quote! {
            pub struct #ret_struct_full_name {
                #(#ret_struct_fields),*
            }
        };

        // Thanks to that struct, we now know what the full return type of the function
        // is
        let full_ret_ty = quote!(__Result<__Ret<#ret_ty, #ret_struct_full_name>>);

        (
            quote!(#ret_struct),
            quote!(fn #name<#(#ty_params,)* #a, #b>(#(#params),*) -> #full_ret_ty;),
        )
    }

    /// Compiles a function.
    fn visit_fun(&mut self, f: Fun<'ctx>) -> TokenStream {
        let name = format_ident!("{}", f.name);

        let ty_params = f.ty_params.into_iter().map(|param| match param {
            TyVar::Named(name) => {
                let name = format_ident!("{}", name);
                quote!(#name: ::std::fmt::Debug + ::std::clone::Clone)
            }
            TyVar::Gen(..) => unreachable!(),
        });

        // Generate two lifetimes for our structure.
        let mut lft_generator = LftGenerator::new();
        let a = lft_generator.generate();
        let b = lft_generator.generate();

        // Tokenize all the parameters
        let params: Vec<TokenStream> = f
            .params
            .iter()
            .map(|param| {
                let name = format_ident!("{}", param.name().as_str());
                // `Var` for pass-by-reference, `Cow` for pass-by-value
                let ty = if param.byref {
                    Self::translate_type(&param.ty(), &[&a, &b], Wrapper::Var)
                } else {
                    Self::translate_type(&param.ty(), &[&b], Wrapper::Cow)
                };
                quote! {
                    mut #name: #ty
                }
            })
            .collect();

        // Tokenize the user-facing return type of the function
        let ret_ty = match f.ret_ty.kind {
            UnitT => quote!(()),
            ty => Self::translate_type(&ty, &[&b], Wrapper::Cow), /* DO NOT add a `Var` on the
                                                                   * return
                                                                   * type!! */
        };

        // Get the list of in-place parameters
        let ret_params: Vec<_> = f.params.iter().filter(|param| param.byref).collect();
        // Their type
        let ret_params_ty: Vec<_> = ret_params
            .iter()
            .map(|param| Self::translate_type(&param.ty(), &[&b], Wrapper::Cow))
            .collect();
        // Their name
        let ret_vars: Vec<_> = ret_params
            .iter()
            .map(|param| format_ident!("{}", param.name().as_str()))
            .collect();

        // Create a tailored structure to hold the result of the in-place parameters
        let ret_struct_name = format_ident!("__{name}Ret");
        let ret_struct_full_name = if ret_vars.is_empty() {
            quote!(#ret_struct_name)
        } else {
            quote!(#ret_struct_name<#b>)
        };
        let ret_struct_fields = ret_vars
            .iter()
            .zip(ret_params_ty.iter())
            .map(|(name, ty)| quote!(#name: Option<#ty>));
        let ret_struct = quote! {
            pub struct #ret_struct_full_name {
                #(#ret_struct_fields),*
            }
        };

        // Thanks to that struct, we now know what the full return type of the function
        // is
        let full_ret_ty = quote!(__Result<__Ret<#ret_ty, #ret_struct_full_name>>);

        // And we can also construct the return expression of the body
        let body = self.visit_block(&f.body, Wrapper::Cow, &ret_struct_name);
        let body = quote! {{
            let __res: __Result<#ret_ty> = try {
                #body
            };
            __Ret::ok(__res?, #ret_struct_name { #(#ret_vars: #ret_vars.try_to_cow().ok()),* })
        }};

        if params.is_empty() && f.body.ret.ty == UnitT {
            // Do not show lifetimes in the function signature if we have no reason to
            quote! {
                #ret_struct
                pub fn #name<#(#ty_params),*>(#(#params),*) -> #full_ret_ty #body
            }
        } else {
            quote! {
                #ret_struct
                pub fn #name<#(#ty_params,)* #a, #b>(#(#params),*) -> #full_ret_ty #body
            }
        }
    }

    /// Compiles a program.
    pub fn visit_program(&mut self, p: Program<'ctx>) -> TokenStream {
        let Program {
            funs,
            structs,
            enums,
            traits,
            arena: _,
        } = p;
        let funs: Vec<TokenStream> = funs.into_values().map(|f| self.visit_fun(f)).collect();
        let structs: Vec<TokenStream> = structs.values().map(|s| self.visit_struct(s)).collect();
        let enums: Vec<TokenStream> = enums.values().map(|s| self.visit_enum(s)).collect();
        let traits: Vec<TokenStream> = traits.values().map(|t| self.visit_trait(t)).collect();
        quote! {
            #(#structs)*
            #(#enums)*
            #(#traits)*
            #(#funs)*
        }
    }

    /// Visit a namespaced element.
    fn visit_namespaced(&self, namespaced: &Namespaced<'ctx>) -> TokenStream {
        namespaced.path().fold(quote!(self), |quote, name| {
            let name = format_ident!("{}", name);
            quote!(#quote::#name)
        })
    }
}
