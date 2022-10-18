use formality_types::grammar::ProgramClause;

use crate::grammar::Program;

mod adts;
mod associated_tys;
mod crate_items;
mod crates;
mod fns;
mod impls;
mod test;
mod traits;

pub trait ToClause {
    fn to_clauses(&self, program: &Program) -> Vec<ProgramClause>;
}

impl<T> ToClause for Vec<T>
where
    T: ToClause,
{
    fn to_clauses(&self, program: &Program) -> Vec<ProgramClause> {
        self.iter().flat_map(|e| e.to_clauses(program)).collect()
    }
}

impl<T> ToClause for Option<T>
where
    T: ToClause,
{
    fn to_clauses(&self, program: &Program) -> Vec<ProgramClause> {
        self.iter().flat_map(|e| e.to_clauses(program)).collect()
    }
}
