use crate::grammar::{Fallible, FeatureGate, FeatureGateName};

use crate::to_rust::syntax;

pub fn lower_feature_gate(gate: &FeatureGate) -> Fallible<syntax::Attr> {
    let name = match gate.name {
        FeatureGateName::PoloniusUnlocked => "polonius_unlocked",
        FeatureGateName::PoloniusAlpha => "polonius_alpha",
        FeatureGateName::NonLifetimeBinders => "non_lifetime_binders",
    };
    Ok(syntax::Attr::Feature(name.to_owned()))
}
