use crate::grammar::{Binder, CrateId};
use crate::grammar::{Enum, Fn, NegTraitImpl, Struct, Trait, TraitImpl, WhereClause};
use formality_core::term;

use crate::grammar::feature::FeatureGate;

#[term(crate $id { $*items })]
pub struct Crate {
    pub id: CrateId,
    pub items: Vec<CrateItem>,
}

#[term]
pub enum CrateItem {
    #[cast]
    FeatureGate(FeatureGate),
    #[cast]
    Struct(Struct),
    #[cast]
    Enum(Enum),
    #[cast]
    Trait(Trait),
    #[cast]
    TraitImpl(TraitImpl),
    #[cast]
    NegTraitImpl(NegTraitImpl),
    #[cast]
    Fn(Fn),
    #[cast]
    Test(Test),
}

#[term(test $binder)]
pub struct Test {
    pub binder: Binder<TestBoundData>,
}

#[term($:where $,assumptions { $,goals })]
pub struct TestBoundData {
    pub assumptions: Vec<WhereClause>,
    pub goals: Vec<WhereClause>,
}
