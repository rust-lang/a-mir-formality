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
}
