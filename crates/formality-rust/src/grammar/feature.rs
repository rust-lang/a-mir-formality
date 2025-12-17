use formality_core::term;
use formality_types::grammar::FeatureGateName;

#[term(#![feature($name)])]
pub struct FeatureGate {
    pub name: FeatureGateName,
}
