//! Utilities to generate lifetime names.

/// Lifetime name generator.
pub struct LftGenerator {
    /// Next name to be returned.
    current_name: Vec<char>,
}

impl LftGenerator {
    /// Creates a new lifetime generator.
    pub fn new() -> Self {
        LftGenerator {
            current_name: vec!['a'],
        }
    }

    /// Generates a new lifetime name.
    pub fn generate(&mut self) -> syn::Lifetime {
        let name = self.current_name.iter().collect::<String>();
        self.update_current();
        syn::Lifetime::new(&format!("'{}", name), proc_macro2::Span::call_site())
    }

    /// Updates the current stored name.
    fn update_current(&mut self) {
        let mut carry = true;
        for c in self.current_name.iter_mut().rev() {
            if carry {
                if *c == 'z' {
                    *c = 'a';
                    carry = true;
                } else {
                    *c = (*c as u8 + 1) as char;
                    carry = false;
                }
            }
        }

        if carry {
            self.current_name.push('a');
        }
    }
}

impl Default for LftGenerator {
    fn default() -> Self {
        Self::new()
    }
}
