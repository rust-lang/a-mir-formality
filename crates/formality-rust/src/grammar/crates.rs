use crate::grammar::{Adt, AdtId, Binder, CrateId};
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
    AdtItem(AdtItem),
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

#[term]
pub enum AdtItem {
    #[cast]
    Struct(Struct),
    #[cast]
    Enum(Enum),
}

impl AdtItem {
    pub fn name(&self) -> AdtId {
        match self {
            AdtItem::Struct(s) => s.id.clone(),
            AdtItem::Enum(e) => e.id.clone(),
        }
    }

    pub fn to_adt(&self) -> Adt {
        match self {
            AdtItem::Struct(s) => s.to_adt(),
            AdtItem::Enum(e) => e.to_adt(),
        }
    }
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
