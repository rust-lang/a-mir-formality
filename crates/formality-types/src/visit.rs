use formality_core::visit::CoreVisit;

use crate::grammar::{Lt, Ty, Variable};

use crate::FormalityLang as Rust;

impl CoreVisit<Rust> for Ty {
    fn free_variables(&self) -> Vec<Variable> {
        self.data().free_variables()
    }

    fn size(&self) -> usize {
        self.data().size()
    }

    fn assert_valid(&self) {
        self.data().assert_valid()
    }
}

impl CoreVisit<Rust> for Lt {
    fn free_variables(&self) -> Vec<Variable> {
        self.data().free_variables()
    }

    fn size(&self) -> usize {
        self.data().size()
    }

    fn assert_valid(&self) {
        self.data().assert_valid()
    }
}
