use formality_types::grammar::{Goal, Hypothesis, Invariant, Lt, ProgramClause, Ty};

use crate::grammar::Program;

mod adts;
mod associated_tys;
mod crate_items;
mod crates;
mod fns;
mod impls;
mod test;
mod traits;

impl Program {
    pub fn to_clauses(&self) -> Vec<ProgramClause> {
        let Program { crates } = self;

        let hardcoded_clauses = vec![
            Hypothesis::for_all_f(|(l, t): (Lt, Ty)| {
                let ref_ty = t.ref_ty(&l);
                Hypothesis::implies(vec![Goal::outlives(t, l)], ref_ty.well_formed())
            }),
            Hypothesis::for_all_f(|(l, t): (Lt, Ty)| {
                let ref_ty = t.ref_mut_ty(&l);
                Hypothesis::implies(vec![Goal::outlives(t, l)], ref_ty.well_formed())
            }),
        ];

        crates
            .iter()
            .flat_map(|c| c.to_clauses(self))
            .chain(hardcoded_clauses)
            .collect()
    }

    pub fn to_invariants(&self) -> Vec<Invariant> {
        let Program { crates } = self;
        crates.iter().flat_map(|c| c.to_invariants(self)).collect()
    }
}
