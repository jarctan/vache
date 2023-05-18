//! Compiler code.

use proc_macro2::TokenStream;
use string_builder::Builder as StringBuilder;
use Place::*;
use Ty::*;

use crate::mir::{Fun, Instr, Mode, Place, Program, RValue, Struct, Ty, VarMode};

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
            #![allow(clippy::needless_late_init)]
            #![allow(unused_mut)]
            #![allow(dead_code)]

            use std::borrow::{Borrow, BorrowMut};
            use std::fmt;
            use std::ops::{Add, Deref, DerefMut, Div, Mul, Rem, Sub};

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
                        Cow::Owned(mut array) => array.remove(index),
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
    pub fn translate_type(ty: &Ty, show_lifetime: bool) -> TokenStream {
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
            ArrayT(box ty) => {
                let ty = Self::translate_type(ty, show_lifetime);
                quote!(Cow<Vec<#ty>>)
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

    /// Compiles a variable (with its addressing mode).
    fn visit_var(&mut self, var: VarMode) -> TokenStream {
        let name = format_ident!("{}", var.var.as_str());
        match var.mode {
            Mode::Cloned => quote!(#name.clone()),
            Mode::Borrowed => quote!(__borrow(&#name)),
            Mode::MutBorrowed => quote!(__borrow_mut(&mut #name)),
            Mode::Moved => quote!(#name),
        }
    }

    /// Compiles a right value.
    fn visit_rvalue(&mut self, rvalue: RValue) -> TokenStream {
        match rvalue {
            RValue::Unit => quote!(()),
            RValue::Integer(i) => {
                let i = i.to_f64();
                quote!(Cow::Owned(::rug::Integer::from_f64(#i).unwrap()))
            }
            RValue::String(s) => {
                quote!(Cow::Owned(::std::string::String::from(#s)))
            }
            RValue::Var(v) => self.visit_var(v),
            RValue::Field(s, field) => {
                let s = self.visit_var(s);
                let field = format_ident!("{field}");
                quote!(#s.#field)
            }
            RValue::Index(array, index, mode) => {
                let index = format_ident!("{}", index.var.as_str());
                let index = quote!(#index.to_usize().unwrap());
                let array = self.visit_var(array);
                match mode {
                    Mode::Borrowed => quote!(__borrow(#array[#index])),
                    Mode::MutBorrowed => quote!(__borrow_mut(&mut #array[#index])),
                    Mode::Cloned => quote!(#array[#index].clone()),
                    Mode::Moved => quote!(Cow::remove(#array, #index)),
                }
            }
            RValue::Struct { .. } => todo!(),
            RValue::Array(array) => {
                let items: Vec<TokenStream> =
                    array.into_iter().map(|item| self.visit_var(item)).collect();
                quote!(Cow::Owned(vec![#(#items),*]))
            }
        }
    }

    /// Compiles a single CFG instruction.
    fn visit_instr(&mut self, instr: &Instr) -> TokenStream {
        match &instr.kind {
            crate::mir::InstrKind::Noop => quote!(),
            crate::mir::InstrKind::Declare(v) => {
                let name = format_ident!("{}", v.name.as_str());
                let ty = Self::translate_type(&v.ty, false);
                quote! {
                    let mut #name: #ty;
                }
            }
            crate::mir::InstrKind::Assign(VarP(v), rhs) => {
                let lhs = format_ident!("{}", v.as_str());
                let rhs = self.visit_rvalue(rhs.clone());
                quote! {
                    #lhs = #rhs;
                }
            }
            crate::mir::InstrKind::Assign(IndexP(array, index), rhs) => {
                let array = format_ident!("{}", array.as_str());
                let index = format_ident!("{}", index.as_str());
                let index = quote!(#index.to_usize().unwrap());
                let rhs = self.visit_rvalue(rhs.clone());
                quote! {
                    #array[#index] = #rhs;
                }
            }
            crate::mir::InstrKind::Assign(_, _) => todo!(),
            crate::mir::InstrKind::Call {
                name,
                args,
                destination,
            } => {
                let prefix = destination
                    .as_ref()
                    .map(|dest| {
                        let destination = format_ident!("{}", dest.as_str());
                        quote!(#destination = )
                    })
                    .unwrap_or_default();
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

                    let args = args.iter().map(|arg| self.visit_var(arg.clone()));
                    let fmt_str = builder.string().unwrap();
                    quote!(#prefix println!(#fmt_str, #(#args),*);)
                } else {
                    let args = args.iter().map(|arg| self.visit_var(arg.clone()));
                    let name = match name.as_str() {
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
                    quote!(#prefix #name(#(#args),*);)
                }
            }
            crate::mir::InstrKind::Branch(_) => todo!(),
        }
    }

    /// Compiles a function.
    fn visit_fun(&mut self, f: Fun) -> TokenStream {
        let name = format_ident!("{}", f.name);
        let params: Vec<TokenStream> = f
            .params
            .into_iter()
            .map(|param| {
                let name = format_ident!("{}", String::from(param.name));
                let ty = Self::translate_type(&param.ty, true);
                quote! {
                    #name: #ty
                }
            })
            .collect();

        let body: TokenStream = f
            .body
            .bfs(&f.entry_l, false)
            .map(|(_, instr)| self.visit_instr(instr))
            .collect();

        let ret_ty = f
            .ret_v
            .as_ref()
            .map(|v| {
                let ty = Self::translate_type(&v.ty, true);
                quote!(-> #ty)
            })
            .unwrap_or_default();

        let ret_vardef = f
            .ret_v
            .as_ref()
            .map(|v| {
                let vardef = format_ident!("{}", v.name.as_str());
                quote!(let mut #vardef;)
            })
            .unwrap_or_default();

        let final_return = f
            .ret_v
            .as_ref()
            .map(|v| {
                let v = format_ident!("{}", v.name.as_str());
                quote!(#v)
            })
            .unwrap_or_default();

        if params.is_empty() {
            quote! {
                pub fn #name(#(#params),*) #ret_ty {
                    #ret_vardef
                    #body;
                    #final_return
                }
            }
        } else {
            quote! {
                pub fn #name<'a>(#(#params),*) #ret_ty {
                    #ret_vardef
                    #body;
                    #final_return
                }
            }
        }
    }

    /// Compiles a program.
    pub fn visit_program(&mut self, p: Program) -> TokenStream {
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
}
