use crate::grammar::{self as rust};

use formality_types::cast::Upcast;
use formality_types::grammar::{self as types, Parameter, Predicate, TraitRef};
use formality_types::grammar::{AtomicRelation, Fallible};
use formality_types::seq;

impl rust::WhereClause {
    pub fn to_decl(&self) -> Fallible<types::Predicate> {
        match self.data() {
            rust::WhereClauseData::IsImplemented(ty, trait_id, parameters) => {
                let trait_ref = TraitRef::new(trait_id, (seq![ty], parameters));
                Ok(trait_ref.is_implemented().upcast())
            }

            rust::WhereClauseData::Outlives(parameter, lt) => {
                Ok(AtomicRelation::Outlives(parameter.upcast(), lt.upcast()).upcast())
            }

            rust::WhereClauseData::ForAll(binder) => {
                let (names, where_clause) = binder.open();
                Ok(Predicate::for_all(&names, where_clause.to_decl()?))
            }
        }
    }
}

impl rust::WhereBound {
    pub fn to_decl(&self, this: &Parameter) -> Fallible<types::Predicate> {
        match self.data() {
            rust::WhereBoundData::IsImplemented(trait_id, parameters) => {
                let trait_ref = TraitRef::new(trait_id, (seq![this], parameters));
                Ok(trait_ref.is_implemented().upcast())
            }
            rust::WhereBoundData::Outlives(lt) => {
                Ok(AtomicRelation::Outlives(this.upcast(), lt.upcast()).upcast())
            }
            rust::WhereBoundData::ForAll(binder) => {
                let (names, where_bound) = binder.open();
                Ok(Predicate::for_all(&names, where_bound.to_decl(this)?))
            }
        }
    }
}
