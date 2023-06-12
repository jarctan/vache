//! Redefining vectors/arrays.
//!
//! The current reason for redefining them is that we to implement `Display` for
//! them. We want vectors to be pretty printed nicely when used in
//! `println!("{}")`s.

use proc_macro2::TokenStream;

#[allow(clippy::missing_docs_in_private_items)]
pub fn vec() -> TokenStream {
    quote!(
        #[derive(Clone)]
        pub struct __Vec<T>(::std::vec::Vec<T>);

        impl<T> __Vec<T> {
            pub fn remove(&mut self, index: usize) -> __Result<T> {
                if index < self.0.len() {
                    Ok(self.0.remove(index))
                } else {
                    ::anyhow::bail!("index {index} is out of bounds")
                }
            }

            pub fn consume(mut self, index: usize) -> __Result<T> {
                if index < self.0.len() {
                    Ok(self.0.swap_remove(index))
                } else {
                    ::anyhow::bail!("index {index} is out of bounds")
                }
            }

            pub fn get(&self, index: usize) -> __Result<&T> {
                self.0
                    .get(index)
                    .with_context(|| format!("index {index} is out of bounds"))
            }

            pub fn get_mut(&mut self, index: usize) -> __Result<&mut T> {
                self.0
                    .get_mut(index)
                    .with_context(|| format!("index {index} is out of bounds"))
            }
        }

        impl<T: fmt::Display> fmt::Display for __Vec<T> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "[")?;
                let mut iter = self.0.iter();
                if let Some(i) = iter.next() {
                    write!(f, "{}", i);
                }
                for i in iter {
                    write!(f, ", {}", i);
                }
                write!(f, "]")
            }
        }
    )
}
