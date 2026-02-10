use formality_core::term;
use crate::types::grammar::FeatureGateName;

#[term(#![feature($name)])]
pub struct FeatureGate {
    pub name: FeatureGateName,
}
