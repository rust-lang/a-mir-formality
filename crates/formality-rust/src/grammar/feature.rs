use formality_core::term;

#[term(#![feature($name)])]
pub struct FeatureGate {
    pub name: FeatureGateName,
}

#[term]
#[derive(Copy)]
pub enum FeatureGateName {
    #[grammar(polonius_alpha)]
    PoloniusAlpha,
}
