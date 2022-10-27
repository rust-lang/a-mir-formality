use formality_types::{
    cast::{To, Upcast},
    grammar::{Hypothesis, Invariant, ProgramClause, RigidTy, Ty},
};

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

        let self_ty: Ty = RigidTy {
            name: self.id.to(),
            parameters: kinded_var_ids.to(),
        }
        .upcast();

        vec![Hypothesis::for_all(
            &kinded_var_ids,
            Hypothesis::implies(&where_clauses, self_ty.well_formed()),
        )]
    }

    pub(super) fn to_invariants(&self, _program: &Program) -> Vec<Invariant> {
        vec![]
    }
}
