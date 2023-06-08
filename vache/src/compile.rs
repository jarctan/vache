//! Compiler code.

use std::default::default;
use std::marker::PhantomData;

use num_traits::ToPrimitive;
use proc_macro2::TokenStream;
use string_builder::Builder as StringBuilder;
use ExprKind::*;
use PlaceKind::*;
use Ty::*;

use crate::tast::{
    Block, Expr, ExprKind, Fun, Mode, Place, PlaceKind, Program, Stmt, Struct, Ty, Var,
};

/// Compiler, that turns our language into source code for an
/// executable language.
pub(crate) struct Compiler<'ctx> {
    /// Phantom argument to bind to the `'ctx` lifetime.
    _lifetime: PhantomData<&'ctx ()>,
}
impl<'ctx> Compiler<'ctx> {
    /// Creates a new compiler.
    pub fn new() -> Self {
        Self {
            _lifetime: PhantomData,
        }
    }

    /// Producing the necessary prelude for all our outputs.
    fn prelude() -> TokenStream {
        quote!(
            #![allow(clippy::needless_late_init)]
            #![allow(unused_mut)]
            #![allow(dead_code)]
            #[warn(unused_imports)]
            #[warn(unused_parens)]

            use std::borrow::{Borrow, BorrowMut};
            use std::fmt;
            use std::ops::{Add, Deref, DerefMut, Div, Mul, Rem, Sub};
            use num_traits::ToPrimitive;

            pub enum Cow<'a, B>
            where
                B: 'a + Clone,
            {
                Borrowed(&'a B),
                MutBorrowed(&'a mut B),
                Owned(B),
            }

            impl<B: Clone> fmt::Debug for Cow<'_, B>
            where
                B: fmt::Debug,
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    match *self {
                        Cow::Borrowed(ref b) => write!(f, "&{:?}", b),
                        Cow::MutBorrowed(ref b) => write!(f, "&mut {:?}", b),
                        Cow::Owned(ref o) => write!(f, "{:?}", o),
                    }
                }
            }

            impl<B: Clone> fmt::Display for Cow<'_, B>
            where
                B: fmt::Display,
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    match *self {
                        Cow::Borrowed(ref b) => fmt::Display::fmt(b, f),
                        Cow::MutBorrowed(ref b) => fmt::Display::fmt(b, f),
                        Cow::Owned(ref o) => fmt::Display::fmt(o, f),
                    }
                }
            }

            impl<'a, B: 'a + Clone> Clone for Cow<'a, B> {
                fn clone(&self) -> Self {
                    match self {
                        Cow::Borrowed(b) => Cow::Borrowed(b),
                        Cow::MutBorrowed(b) => {
                            let b: &B = b.borrow();
                            Cow::Owned(b.to_owned())
                        }
                        Cow::Owned(ref b) => {
                            let b: &B = b.borrow();
                            Cow::Owned(b.to_owned())
                        }
                    }
                }

                fn clone_from(&mut self, source: &Self) {
                    match (self, source) {
                        (&mut Cow::Owned(ref mut dest), Cow::Owned(ref o)) => o.borrow().clone_into(dest),
                        (t, s) => *t = s.clone(),
                    }
                }
            }

            impl<B: Clone> Cow<'_, B> {
                pub fn into_owned(self) -> B {
                    match self {
                        Cow::Borrowed(borrowed) => borrowed.clone(),
                        Cow::MutBorrowed(borrowed) => (*borrowed).clone(),
                        Cow::Owned(owned) => owned,
                    }
                }
            }

            impl<B: Clone> Deref for Cow<'_, B> {
                type Target = B;

                fn deref(&self) -> &B {
                    match self {
                        Cow::Borrowed(borrowed) => borrowed,
                        Cow::MutBorrowed(borrowed) => borrowed,
                        Cow::Owned(ref owned) => owned.borrow(),
                    }
                }
            }

            impl<B: Clone> DerefMut for Cow<'_, B> {
                fn deref_mut(&mut self) -> &mut B {
                    match self {
                        Cow::Borrowed(borrowed) => {
                            *self = Cow::Owned(borrowed.clone());
                            match self {
                                Cow::Owned(ref mut owned) => owned,
                                Cow::Borrowed(..) | Cow::MutBorrowed(..) => unreachable!(),
                            }
                        }
                        Cow::MutBorrowed(borrowed) => borrowed,
                        Cow::Owned(ref mut owned) => owned,
                    }
                }
            }

            /// Prelude function.
            pub(crate) fn __eq<B: PartialEq + Clone>(x: Cow<B>, y: Cow<B>) -> bool {
                let b1: &B = x.borrow();
                let b2: &B = y.borrow();
                b1 == b2
            }

            /// Prelude function.
            pub(crate) fn __add<'a, B: Add<Output = B> + Clone>(x: Cow<B>, y: Cow<B>) -> Cow<'a, B> {
                Cow::Owned(x.into_owned() + y.into_owned())
            }

            /// Prelude function.
            pub(crate) fn __sub<'a, B: Sub<Output = B> + Clone>(x: Cow<B>, y: Cow<B>) -> Cow<'a, B> {
                Cow::Owned(x.into_owned() - y.into_owned())
            }

            /// Prelude function.
            pub(crate) fn __mul<'a, B: Mul<Output = B> + Clone>(x: Cow<B>, y: Cow<B>) -> Cow<'a, B> {
                Cow::Owned(x.into_owned() * y.into_owned())
            }

            /// Prelude function.
            pub(crate) fn __div<'a, B: Div<Output = B> + Clone>(x: Cow<B>, y: Cow<B>) -> Cow<'a, B> {
                Cow::Owned(x.into_owned() / y.into_owned())
            }

            /// Prelude function.
            pub(crate) fn __rem<'a, B: Rem<Output = B> + Clone>(x: Cow<B>, y: Cow<B>) -> Cow<'a, B> {
                Cow::Owned(x.into_owned() % y.into_owned())
            }

            /// Prelude function.
            pub(crate) fn __ge<B: PartialOrd + Clone>(x: Cow<B>, y: Cow<B>) -> bool {
                let b1: &B = x.borrow();
                let b2: &B = y.borrow();
                b1 >= b2
            }

            /// Prelude function.
            #[allow(clippy::ptr_arg)]
            pub(crate) fn __borrow<'b, 'c: 'b, 'a, B: Clone>(cow: &'c Cow<'a, B>) -> Cow<'b, B> {
                match cow {
                    Cow::Borrowed(b) => Cow::Borrowed(b),
                    Cow::MutBorrowed(b) => Cow::Borrowed(b),
                    Cow::Owned(ref o) => {
                        let b: &'c B = o.borrow();
                        Cow::Borrowed(b)
                    }
                }
            }

            /// Prelude function.
            #[allow(clippy::ptr_arg)]
            pub(crate) fn __borrow_mut<'b, 'c: 'b, 'a: 'b, B: Clone>(cow: &'c mut Cow<'a, B>) -> Cow<'b, B> {
                match cow {
                    Cow::Borrowed(b) => Cow::Borrowed(b),
                    Cow::MutBorrowed(b) => Cow::Borrowed(b),
                    Cow::Owned(ref mut o) => {
                        let b: &'c mut B = o.borrow_mut();
                        Cow::MutBorrowed(b)
                    }
                }
            }

            impl<'a, B: Clone> Borrow<B> for Cow<'a, B> {
                fn borrow(&self) -> &B {
                    self
                }
            }

            impl<'a, B: Clone> BorrowMut<B> for Cow<'a, B> {
                fn borrow_mut(&mut self) -> &mut B {
                    self
                }
            }

            impl<'a, 'c, 'b: 'c, B: Clone> Cow<'a, Vec<Cow<'b, B>>> {
                pub fn remove(self, index: usize) -> Cow<'c, B> {
                    match self {
                        Cow::Borrowed(b) => b[index].clone(),
                        Cow::MutBorrowed(array) => array.remove(index),
                        Cow::Owned(mut array) => array.swap_remove(index),
                    }
                }
            }
        )
    }

    /// Compiles a program in our language into an executable source code.
    pub fn compile(&mut self, p: Program<'ctx>) -> String {
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
                    quote!(Cow<'a, String>)
                } else {
                    quote!(Cow<String>)
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
                    quote!(Cow<'a, Vec<#ty>>)
                } else {
                    quote!(Cow<Vec<#ty>>)
                }
            }
        }
    }

    /// Compiles a variable.
    fn visit_var(&mut self, var: impl Into<Var<'ctx>>) -> TokenStream {
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
                    Mode::SBorrowed => quote!((&#var)),
                    Mode::MutBorrowed => quote!((&mut #var)),
                    Mode::Moved => quote!(#var),
                    Mode::Assigning => quote!(#var),
                }
            }
            IndexP(box array, box index) => {
                let array = self.visit_expr(array);
                let index = self.visit_expr(index);
                let index = quote!((#index).to_usize().unwrap());
                match place.mode {
                    Mode::Borrowed => quote!(__borrow(&#array[#index])),
                    Mode::SBorrowed => quote!((&#array[#index])),
                    Mode::MutBorrowed => quote!(__borrow_mut(&mut (#array)[#index])),
                    Mode::Cloned => quote!(#array[#index].clone()),
                    Mode::Moved => quote!(#array.remove(#index)),
                    Mode::Assigning => quote!(#array[#index]),
                }
            }
            FieldP(box strukt, field) => {
                let strukt = self.visit_expr(strukt);
                let field = format_ident!("{}", field);
                quote!(#strukt.#field)
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
                let ty = Self::translate_type(&ty, true);
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
                quote!(Cow::Owned(::std::string::String::from(#s)))
            }
            PlaceE(p) => self.visit_place(p),
            StructE { name, fields } => {
                let name = format_ident!("{name}");
                let fields = fields.into_iter().map(|(name, expr)| {
                    let name = format_ident!("{name}");
                    let expr = self.visit_expr(expr);
                    quote!(#name: #expr)
                });
                quote!(Cow::Owned(#name {
                    #(#fields),*
                }))
            }
            ArrayE(array) => {
                let items = array.into_iter().map(|item| self.visit_expr(item));
                quote!(Cow::Owned(vec![#(#items),*]))
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
                    quote!(#name(#(#args),*))
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
        }
    }

    /// Compiles a single statement.
    fn visit_stmt(&mut self, stmt: Stmt<'ctx>) -> TokenStream {
        match stmt {
            Stmt::Declare(lhs, rhs) => {
                let name = format_ident!("{}", lhs.name.as_str());
                let ty = Self::translate_type(&lhs.ty, false);
                let rhs = self.visit_expr(rhs);
                quote! {
                    let mut #name: #ty = #rhs;
                }
            }
            Stmt::Assign(lhs, rhs) => {
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
            Stmt::While { cond, body } => {
                let cond = self.visit_expr(cond);
                let body = self.visit_block(body);
                quote! {
                    while #cond #body
                }
            }
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
                let name = format_ident!("{}", param.name.as_str());
                let ty = Self::translate_type(&param.ty, true);
                quote! {
                    #name: #ty
                }
            })
            .collect();

        let body = self.visit_block(f.body);

        let ret_ty = match f.ret_ty {
            UnitT => quote!(),
            ty => {
                let ty = Self::translate_type(&ty, true);
                quote!(-> #ty)
            }
        };

        if params.is_empty() {
            quote! {
                pub fn #name(#(#params),*) #ret_ty #body
            }
        } else {
            quote! {
                pub fn #name<'a>(#(#params),*) #ret_ty #body
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
