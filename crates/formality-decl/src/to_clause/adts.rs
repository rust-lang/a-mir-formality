use formality_types::grammar::{Hypothesis, Invariant, ProgramClause};

use crate::grammar::{Adt, AdtBoundData, Program};

impl Adt {
    pub(super) fn to_clauses(&self, _program: &Program) -> Vec<ProgramClause> {
        let (
            kinded_var_ids,
            AdtBoundData {
                where_clauses,
                variants: _,
            },
        ) = self.binder.open();

        vec![Hypothesis::for_all(
            &kinded_var_ids,
            Hypothesis::implies(&where_clauses, self.id.well_formed(&kinded_var_ids)),
        )]
    }

    pub(super) fn to_invariants(&self, _program: &Program) -> Vec<Invariant> {
        vec![]
    }
}
