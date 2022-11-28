use formality_types::grammar::Invariant;

use crate::grammar::{CrateItem, Program};

impl CrateItem {
    pub(super) fn to_clauses(
        &self,
        program: &crate::grammar::Program,
    ) -> Vec<formality_types::grammar::ProgramClause> {
        match self {
            CrateItem::Adt(v) => v.to_clauses(program),
            CrateItem::Trait(v) => v.to_clauses(program),
            CrateItem::TraitImpl(v) => v.to_clauses(program),
            CrateItem::Fn(v) => v.to_clauses(&[], program),
        }
    }

    pub(super) fn to_invariants(&self, program: &Program) -> Vec<Invariant> {
        match self {
            CrateItem::Adt(v) => v.to_invariants(program),
            CrateItem::Trait(v) => v.to_invariants(program),
            CrateItem::TraitImpl(_) => vec![],
            CrateItem::Fn(_) => vec![],
        }
    }
}
