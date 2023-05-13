//! Compiler code.

use proc_macro2::TokenStream;
use string_builder::Builder as StringBuilder;
use Ty::*;

use crate::mir::{Fun, Instr, Program, RValue, Struct, Ty, VarMode};

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
            pub(crate) fn __borrow<'a, B: ?Sized + ToOwned>(cow: &'a Cow<'a, B>) -> Cow<'a, B> {
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
    pub fn translate_type(&self, ty: &Ty, show_lifetime: bool) -> TokenStream {
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

    /// Compiles a struct.
    fn visit_struct(&mut self, strukt: Struct) -> TokenStream {
        let name = format_ident!("{}", strukt.name);
        let fields: TokenStream = strukt
            .fields
            .into_iter()
            .map(|(k, ty)| {
                let k = format_ident!("{k}");
                let ty = self.translate_type(&ty, true);
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
        if var.owned {
            quote!(#name.clone())
        } else {
            quote!(__borrow(&#name))
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
        }
    }

    /// Compiles a single CFG instruction.
    fn visit_instr(&mut self, instr: &Instr) -> TokenStream {
        match &instr.kind {
            crate::mir::InstrKind::Noop => quote!(),
            crate::mir::InstrKind::Declare(v) => {
                let name = format_ident!("{}", v.name.as_str());
                let ty = self.translate_type(&v.ty, false);
                quote! {
                    let mut #name: #ty;
                }
            }
            crate::mir::InstrKind::Assign(lhs, rhs) => {
                let lhs = format_ident!("{}", lhs.as_str());
                let rhs = self.visit_rvalue(rhs.clone());
                quote! {
                    #lhs = #rhs;
                }
            }
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
            crate::mir::InstrKind::Struct { .. } => todo!(),
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
                let ty = self.translate_type(&param.ty, true);
                quote! {
                    #name: #ty
                }
            })
            .collect();

        let body: TokenStream = f
            .body
            .bfs(&f.entry_l)
            .map(|(_, instr)| self.visit_instr(instr))
            .collect();

        let ret_ty = f
            .ret_v
            .as_ref()
            .map(|v| {
                let ty = self.translate_type(&v.ty, true);
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
