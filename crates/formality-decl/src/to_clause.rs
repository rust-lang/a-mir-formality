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

impl Program {
    pub fn to_clauses(&self) -> Vec<ProgramClause> {
        let Program { crates } = self;
        crates.iter().flat_map(|c| c.to_clauses(self)).collect()
    }
}
