use crate::grammar::{Fallible, FeatureGate, FeatureGateName};

use crate::to_rust::syntax;

pub fn lower_feature_gate(gate: &FeatureGate) -> Fallible<syntax::Attr> {
    let name = match gate.name {
        FeatureGateName::PoloniusAlpha => "polonius_alpha",
    };
    Ok(syntax::Attr::Feature(name.to_owned()))
}
