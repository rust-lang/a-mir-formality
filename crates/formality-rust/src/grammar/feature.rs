use formality_core::term;

#[term(#![feature($name)])]
pub struct FeatureGate {
    pub name: FeatureGateName,
}

#[term]
#[derive(Copy)]
pub enum FeatureGateName {
    #[grammar(polonius_unlocked)]
    PoloniusUnlocked,
    #[grammar(polonius_alpha)]
    PoloniusAlpha,
    #[grammar(non_lifetime_binders)]
    NonLifetimeBinders,
    // #![feature(negative_impls)]
    #[grammar(negative_impls)]
    NegativeImpls,
    // #![feature(adt_const_params)]
    #[grammar(adt_const_params)]
    AdtConstParams,
    // #![feature(unsized_const_params)]
    #[grammar(unsized_const_params)]
    UnsizedConstParams,
}
