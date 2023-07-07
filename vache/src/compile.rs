//! Compiler code.

use std::default::default;

use itertools::Itertools;
use num_traits::ToPrimitive;
use proc_macro2::TokenStream;
use string_builder::Builder as StringBuilder;
use ExprKind::*;
use PatKind::*;
use PlaceKind::*;
use StmtKind::*;
use Ty::*;

use crate::tast::{
    Arg, ArgKind, Block, Enum, Expr, ExprKind, Fun, LhsMode, LhsPlace, LineCol, Mode, Pat, PatKind,
    Place, PlaceKind, Program, Stmt, StmtKind, Struct, Ty, TyVar, Varname,
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
    pub fn translate_type(ty: &Ty, show_lifetime: bool, wrap_var: bool) -> TokenStream {
        let wrapper = if wrap_var { quote!(Var) } else { quote!(Cow) };
        let lft = if show_lifetime {
            if wrap_var {
                quote!('a, 'b, ) // two lifetimes are needed for `Var`
            } else {
                quote!('b, ) // only one is needed for `Cow`
            }
        } else {
            quote!() // no lifetime needed then
        };
        match ty {
            UnitT => quote!(()),
            BoolT => quote!(#wrapper<#lft bool>),
            IntT => quote!(#wrapper<#lft __Integer>),
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
                let ty = Self::translate_type(ty, show_lifetime, true);
                quote!(#wrapper<#lft __Vec<#ty>>)
            }
            TupleT(items) => {
                let items = items
                    .iter()
                    .map(|item| Self::translate_type(item, show_lifetime, true));
                quote!(#wrapper<#lft (#(#items),*)>)
            }
            IterT(ty) => {
                let ty = Self::translate_type(ty, show_lifetime, true);
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
                let ty = Self::translate_type(&place.ty, false, true);
                match place.mode {
                    LhsMode::Assigning => quote!(*(#var)),
                    LhsMode::Declaring => quote!(let mut #var: #ty),
                }
            }
            IndexP(box array, box index) => {
                let LineCol { line, col } = index.span.start_line_col(self.ctx.files);
                let filename = self.ctx.files.name();
                let codespan = format!("Out of bounds indexing at {filename}:{line}:{col}");
                let array = self.visit_expr(array, false, f_ret_struct);
                let index = self.visit_expr(index, false, f_ret_struct);
                let index = quote!((#index).to_usize().unwrap());
                match place.mode {
                    LhsMode::Assigning => quote!(**(#array).get_mut(#index).context(#codespan)?),
                    LhsMode::Declaring => unreachable!(),
                }
            }
            FieldP(box strukt, field) => {
                let strukt = self.visit_expr(strukt, false, f_ret_struct);
                let field = format_ident!("{}", field);
                match place.mode {
                    LhsMode::Assigning => quote!(*(#strukt).#field),
                    LhsMode::Declaring => unreachable!(),
                }
            }
            ElemP(box tuple, elem) => {
                let tuple = self.visit_expr(tuple, false, f_ret_struct);
                let elem = syn::Index::from(*elem);
                match place.mode {
                    LhsMode::Assigning => quote!(*(#tuple).#elem),
                    LhsMode::Declaring => unreachable!(),
                }
            }
        }
    }

    /// Compiles a place.
    fn visit_place(
        &mut self,
        place: &Place<'ctx>,
        wrap_var: bool,
        f_ret_struct: &syn::Ident,
    ) -> TokenStream {
        match &place.kind {
            VarP(var) => {
                let var = self.visit_var(var);
                match place.mode {
                    Mode::Cloned => {
                        if wrap_var {
                            quote!(#var.clone())
                        } else {
                            quote!(Var::to_cow(&mut #var.clone()))
                        }
                    }
                    Mode::Borrowed => {
                        if wrap_var {
                            quote!(__ref(&#var))
                        } else {
                            quote!(__borrow(&mut *#var))
                        }
                    }
                    Mode::SBorrowed => quote!(#var),
                    Mode::MutBorrowed => {
                        if wrap_var {
                            quote!(__ref_mut(&mut #var))
                        } else {
                            quote!(__borrow_mut(&mut *#var))
                        }
                    }
                    Mode::SMutBorrowed => quote!(#var),
                    Mode::Moved => {
                        if wrap_var {
                            quote!(Var::__take(&mut #var))
                        } else {
                            quote!(Cow::__take(&mut #var))
                        }
                    }
                }
            }
            IndexP(box array, box index) => {
                let LineCol { line, col } = index.span.start_line_col(self.ctx.files);
                let filename = self.ctx.files.name();
                let codespan = format!("Out of bounds indexing at {filename}:{line}:{col}");
                let array = self.visit_expr(array, false, f_ret_struct);
                let index = self.visit_expr(index, false, f_ret_struct);
                let index = quote!((#index).to_usize().unwrap());
                match place.mode {
                    Mode::Borrowed => {
                        if wrap_var {
                            quote!(__ref((#array).get(#index).context(#codespan)?))
                        } else {
                            unimplemented!()
                        }
                    }
                    Mode::SBorrowed => quote!((#array).get(#index).context(#codespan)?),
                    Mode::SMutBorrowed => {
                        quote!(#array.get_mut(#index).context(#codespan)?)
                    }
                    Mode::MutBorrowed => {
                        if wrap_var {
                            quote!(__ref_mut((#array).get_mut(#index).context(#codespan)?))
                        } else {
                            unimplemented!()
                        }
                    }
                    Mode::Cloned => {
                        if wrap_var {
                            quote!(Var::clone((#array).get(#index).context(#codespan)?))
                        } else {
                            quote!(Cow::clone(&*(#array).get(#index).context(#codespan)?))
                        }
                    }
                    Mode::Moved => {
                        if wrap_var {
                            quote!(#array.remove(#index).context(#codespan)?)
                        } else {
                            quote!(Cow::remove(#array.__take(), #index).context(#codespan)?)
                        }
                    }
                }
            }
            FieldP(box strukt, field) => {
                let strukt = self.visit_expr(strukt, false, f_ret_struct);
                let field = format_ident!("{}", field);
                match place.mode {
                    Mode::Borrowed => {
                        if wrap_var {
                            quote!(__ref(&(#strukt).#field))
                        } else {
                            quote!(__borrow(&*(#strukt).#field))
                        }
                    }
                    Mode::SBorrowed => unimplemented!(),
                    Mode::MutBorrowed => {
                        if wrap_var {
                            quote!(__ref_mut(&mut (#strukt).#field))
                        } else {
                            quote!(__borrow_mut(&mut *(#strukt).#field))
                        }
                    }
                    Mode::SMutBorrowed => unimplemented!(),
                    Mode::Cloned => {
                        if wrap_var {
                            quote!(Var::clone(&(#strukt).#field))
                        } else {
                            quote!(Cow::clone(&*(#strukt).#field))
                        }
                    }
                    Mode::Moved => {
                        if wrap_var {
                            quote!(Var::__take(&mut (#strukt).#field))
                        } else {
                            quote!(Cow::__take(&mut (#strukt).#field))
                        }
                    }
                }
            }
            ElemP(box tuple, elem) => {
                let tuple = self.visit_expr(tuple, false, f_ret_struct);
                let elem = syn::Index::from(*elem);
                match place.mode {
                    Mode::Borrowed => {
                        if wrap_var {
                            quote!(__ref(&(#tuple).#elem))
                        } else {
                            quote!(__borrow(&(#tuple).#elem))
                        }
                    }
                    Mode::SBorrowed => unimplemented!(),
                    Mode::MutBorrowed => {
                        if wrap_var {
                            quote!(__ref_mut(&mut (#tuple).#elem))
                        } else {
                            quote!(__borrow_mut(&mut *(#tuple).#elem))
                        }
                    }
                    Mode::SMutBorrowed => unimplemented!(),
                    Mode::Cloned => {
                        if wrap_var {
                            quote!(Var::clone(&(#tuple).#elem))
                        } else {
                            quote!(Cow::clone(&*(#tuple).#elem))
                        }
                    }
                    Mode::Moved => {
                        if wrap_var {
                            quote!(Var::__take(&mut (#tuple).#elem))
                        } else {
                            quote!(Cow::__take(&mut (#tuple).#elem))
                        }
                    }
                }
            }
        }
    }

    /// Compiles a struct.
    fn visit_struct(&mut self, strukt: &Struct) -> TokenStream {
        let name = format_ident!("{}", strukt.name);

        let ty_params = strukt.ty_params.iter().map(|param| match param {
            TyVar::Named(name) => {
                let name = format_ident!("{}", name);
                quote!(#name: ::std::fmt::Debug + ::std::fmt::Display + ::std::clone::Clone)
            }
            TyVar::Gen(..) => unreachable!(),
        });

        let fields: TokenStream = strukt
            .fields
            .iter()
            .map(|(field, ty)| {
                let field = format_ident!("{field}");
                let ty = Self::translate_type(&ty.kind, true, true);
                quote!(#field: #ty,)
            })
            .collect();

        quote!(
            #[derive(Debug, Clone)]
            pub struct #name<#(#ty_params,)* 'a, 'b> {
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
                    let args = args
                        .iter()
                        .map(|arg| Self::translate_type(&arg.kind, true, true));
                    quote!(#variant(#(#args),*),)
                }
            })
            .collect();

        // Add an implementation of `Display` for enums
        // For that, create the branches of the match we'll use in the impl of
        // `Display`.
        let variants_display: Vec<_> = enun
            .variants
            .iter()
            .map(|(&variant, args)| {
                let variant_str = variant;
                let variant_ident = format_ident!("{}", variant);
                if !args.is_empty() {
                    // Create the list of left-hand side arguments, the one we're going to introduce with the pattern matching
                    // and then use again on the other side of the `=>`, in the `write!`
                    let args = (0..args.len()).map(|i| format_ident!("__arg{i}")).collect::<Vec<_>>();
                    // Compute the format string for the arguments of that variant in the display
                    // Overall, some `{} {} {} {} ...`
                    let variant_str = format!("{variant_str}({})", (0..args.len()).map(|_| "{}").join(", "));
                    quote!(#name::#variant_ident(#(#args),*) => write!(f, #variant_str, #(#args),*),)
                } else {
                    quote!(#name::#variant_ident => write!(f, #variant_str),)
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
                    quote!(#name: ::std::fmt::Debug + ::std::fmt::Display + ::std::clone::Clone)
                }
                TyVar::Gen(..) => unreachable!(),
            })
            .collect();

        // Type parameters list without trait bounds
        let ty_params_wo_bounds: Vec<_> = enun
            .ty_params
            .iter()
            .map(|param| match param {
                TyVar::Named(name) => format_ident!("{}", name),
                TyVar::Gen(..) => unreachable!(),
            })
            .collect();

        quote!(
            #[derive(Debug, Clone)]
            pub enum #name<#(#ty_params_w_bounds,)* 'a, 'b> {
                #variants
            }

            impl<#(#ty_params_w_bounds,)* 'a, 'b> ::std::fmt::Display for #name<#(#ty_params_wo_bounds,)* 'a, 'b> {
                fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                    match self {
                        #(#variants_display)*
                    }
                }
            }
        )
    }

    /// Compiles a expression.
    fn visit_expr(
        &mut self,
        expr: &Expr<'ctx>,
        wrap_var: bool,
        f_ret_struct: &syn::Ident,
    ) -> TokenStream {
        let wrapper = if wrap_var { quote!(Var) } else { quote!(Cow) };
        match &expr.kind {
            UnitE => quote!(()),
            BoolE(b) => quote!(#wrapper::owned(#b)),
            IntegerE(i) => {
                let i = i
                    .to_u128()
                    .expect("Integer {i} is too big to be represented in source code");
                quote!(#wrapper::owned( __Integer::try_from(#i).unwrap()))
            }
            StringE(s) => {
                quote!(#wrapper::owned(__String::from(#s)))
            }
            PlaceE(p) => self.visit_place(p, wrap_var, f_ret_struct),
            StructE { name, fields } => {
                let name = format_ident!("{name}");
                let fields = fields.iter().map(|(name, expr)| {
                    let name = format_ident!("{name}");
                    let expr = self.visit_expr(expr, true, f_ret_struct);
                    quote!(#name: #expr)
                });
                quote!(#wrapper::owned(#name {
                    #(#fields),*
                }))
            }
            ArrayE(array) => {
                let items = array
                    .iter()
                    .map(|item| self.visit_expr(item, true, f_ret_struct));
                quote!(#wrapper::owned(__Vec(vec![#(#items),*])))
            }
            TupleE(items) => {
                let items = items
                    .iter()
                    .map(|item| self.visit_expr(item, true, f_ret_struct));
                quote!(#wrapper::owned((#(#items),*)))
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

                    let args = args
                        .iter()
                        .map(|arg| self.visit_arg(arg, true, f_ret_struct));
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
                        .collect::<Vec<_>>();

                    // Tokenize the arguments and the real name of the function
                    let args = args
                        .iter()
                        .map(|arg| self.visit_arg(arg, true, f_ret_struct));
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
                        "push" => quote!(__push),
                        _ => {
                            let ident = format_ident!("{}", name.name);
                            quote!(#ident)
                        }
                    };

                    // If needed, wrap in a `final_res`
                    let final_res = if wrap_var {
                        quote!(Var::Owned(__res.0))
                    } else {
                        quote!(__res.0)
                    };

                    quote!({
                        let __res = #name(#(#args),*)?;
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
                    quote!(#wrapper::owned(#enun::#variant))
                } else {
                    let args = args
                        .iter()
                        .map(|arg| self.visit_expr(arg, true, f_ret_struct));
                    quote!(#wrapper::owned(#enun::#variant(#(#args),*)))
                }
            }
            IfE(box cond, box iftrue, box iffalse) => {
                let cond = self.visit_expr(cond, false, f_ret_struct);
                let iftrue = self.visit_block(iftrue, wrap_var, f_ret_struct);
                let iffalse = self.visit_block(iffalse, wrap_var, f_ret_struct);

                quote! {
                    if *(#cond) #iftrue else #iffalse
                }
            }
            MatchE(box matched, branches) => {
                let matched = self.visit_expr(matched, false, f_ret_struct);
                let branches = branches.iter().map(|(pattern, branch)| {
                    let pattern = self.visit_pattern(pattern);
                    let branch = self.visit_expr(branch, wrap_var, f_ret_struct);
                    quote!(| #pattern => #branch)
                });

                quote! {
                    match &*#matched {
                        #(#branches)*
                        _ => ::anyhow::bail!("Matching failed"),
                    }
                }
            }
            BlockE(box block) => self.visit_block(block, wrap_var, f_ret_struct),
            HoleE => {
                panic!("Cannot compile code with holes; your code probably even did not typecheck")
            }
            RangeE(box start, box end) => {
                let start = self.visit_expr(start, true, f_ret_struct);
                let end = self.visit_expr(end, true, f_ret_struct);
                quote!(#wrapper::owned(__Range::new(#start,#end)))
            }
        }
    }

    /// Compiles a function argument.
    fn visit_arg(
        &mut self,
        arg: &Arg<'ctx>,
        wrap_var: bool,
        f_ret_struct: &syn::Ident,
    ) -> TokenStream {
        match &arg.kind {
            ArgKind::Standard(arg) => self.visit_expr(arg, wrap_var, f_ret_struct),
            ArgKind::InPlace(place) => self.visit_place(place, wrap_var, f_ret_struct),
            ArgKind::Binding(arg, _) => self.visit_expr(arg, wrap_var, f_ret_struct),
        }
    }

    /// Compiles a pattern.
    #[allow(clippy::only_used_in_recursion)]
    fn visit_pattern(&mut self, pat: &Pat<'ctx>) -> TokenStream {
        match &pat.kind {
            BoolM(b) => quote!(#b),
            IntegerM(i) => {
                let i = i
                    .to_u128()
                    .expect("Integer {i} is too big to be represented in source code");
                quote!(#i)
            }
            StringM(s) => quote!(#s),
            IdentM(i) => {
                let i = format_ident!("{}", i.name().as_str());
                quote!(#i)
            }
            VariantM {
                enun,
                variant,
                args,
            } => {
                let enun = format_ident!("{enun}");
                let variant = format_ident!("{variant}");
                let args = args.iter().map(|arg| self.visit_pattern(arg));
                quote!(#enun::#variant(#(#args)*))
            }
        }
    }

    /// Compiles a single statement.
    fn visit_stmt(&mut self, stmt: &Stmt<'ctx>, f_ret_struct: &syn::Ident) -> TokenStream {
        match &stmt.kind {
            AssignS(lhs, rhs) => {
                let lhs_mode = lhs.mode;
                let rhs =
                    self.visit_expr(rhs, matches!(lhs_mode, LhsMode::Declaring), f_ret_struct);
                let lhs = self.visit_lhs_place(lhs, f_ret_struct);

                // For fields, add a `Field` wrapper
                quote! {
                    #lhs = #rhs;
                }
            }
            ExprS(expr) => {
                let expr = self.visit_expr(expr, false, f_ret_struct);
                quote! {
                    #expr;
                }
            }
            WhileS { cond, body } => {
                let cond = self.visit_expr(cond, false, f_ret_struct);
                let body = self.visit_block(body, false, f_ret_struct);
                quote! {
                    while *#cond #body
                }
            }

            ForS { item, iter, body } => {
                let item = format_ident!("{}", item.name().as_str());
                let iter = self.visit_expr(iter, false, f_ret_struct);
                let body = self.visit_block(body, false, f_ret_struct);
                quote! {
                    for #item in #iter #body
                }
            }
            HoleS => unreachable!(),
            BreakS => quote!(break;),
            ContinueS => quote!(continue;),
            ReturnS(ret) => {
                let ret = self.visit_expr(ret, false, f_ret_struct);
                quote!(return __Ret::ok(#ret, #f_ret_struct { });)
            }
        }
    }

    /// Compiles a block.
    fn visit_block(
        &mut self,
        block: &Block<'ctx>,
        wrap_var: bool,
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
            self.visit_expr(&block.ret, wrap_var, f_ret_struct)
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

        let ty_params = f.ty_params.into_iter().map(|param| match param {
            TyVar::Named(name) => {
                let name = format_ident!("{}", name);
                quote!(#name: ::std::fmt::Debug + ::std::fmt::Display + ::std::clone::Clone)
            }
            TyVar::Gen(..) => unreachable!(),
        });

        // Tokenize all the parameters
        let params: Vec<TokenStream> = f
            .params
            .iter()
            .map(|param| {
                let name = format_ident!("{}", param.name().as_str());
                let ty = Self::translate_type(&param.ty(), true, true);
                quote! {
                    mut #name: #ty
                }
            })
            .collect();

        // Tokenize the user-facing return type of the function
        let ret_ty = match f.ret_ty.kind {
            UnitT => quote!(()),
            ty => Self::translate_type(&ty, true, false), // DO NOT add a `Var` on the return type!!
        };

        // Get the list of in-place parameters
        let ret_params: Vec<_> = f.params.iter().filter(|param| param.byref).collect();
        // Their type
        let ret_params_ty: Vec<_> = ret_params
            .iter()
            .map(|param| Self::translate_type(&param.ty(), true, false))
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
            quote!(#ret_struct_name<'b>)
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
        let body = self.visit_block(&f.body, false, &ret_struct_name);
        let body = quote! {{
            let __res: __Result<#ret_ty> = try {
                #body
            };
            __Ret::ok(__res?, #ret_struct_name { #(#ret_vars: #ret_vars.try_to_cow().ok()),* })
        }};

        if params.is_empty() {
            quote! {
                #ret_struct
                pub fn #name<#(#ty_params),*>(#(#params),*) -> #full_ret_ty #body
            }
        } else {
            quote! {
                #ret_struct
                pub fn #name<#(#ty_params,)* 'a, 'b>(#(#params),*) -> #full_ret_ty #body
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
