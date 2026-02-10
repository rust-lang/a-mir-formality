use crate::types::grammar::FeatureGateName;
use formality_core::term;

#[term(#![feature($name)])]
pub struct FeatureGate {
    pub name: FeatureGateName,
}
