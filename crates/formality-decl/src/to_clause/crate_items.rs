use crate::grammar::CrateItem;

use super::ToClause;

impl ToClause for CrateItem {
    fn to_clauses(
        &self,
        program: &crate::grammar::Program,
    ) -> Vec<formality_types::grammar::ProgramClause> {
        match self {
            CrateItem::Adt(v) => v.to_clauses(program),
            CrateItem::Trait(v) => v.to_clauses(program),
            CrateItem::TraitImpl(v) => v.to_clauses(program),
        }
    }
}
