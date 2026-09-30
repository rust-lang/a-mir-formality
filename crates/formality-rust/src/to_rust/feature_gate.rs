use crate::grammar::{Fallible, FeatureGate, FeatureGateName};

use crate::to_rust::syntax;

/// Lower a feature gate to a `#![feature(..)]` attribute. A feature of the
/// model with no rustc counterpart lowers to a pseudo-feature of the same
/// name, like `polonius_alpha`.
pub fn lower_feature_gate(gate: &FeatureGate) -> Fallible<syntax::Attr> {
    let name = match gate.name {
        FeatureGateName::PoloniusUnlocked => "polonius_unlocked",
        FeatureGateName::PoloniusAlpha => "polonius_alpha",
        FeatureGateName::NonLifetimeBinders => "non_lifetime_binders",
        FeatureGateName::NegativeImpls => "negative_impls",
        FeatureGateName::BranchSpecialization => "branch_specialization",
        FeatureGateName::SpecCommitAndVerify => "spec_commit_and_verify",
        FeatureGateName::SpecBailOnRegions => "spec_bail_on_regions",
        FeatureGateName::SpecAlwaysApplicable => "spec_always_applicable",
    };
    Ok(syntax::Attr::Feature(name.to_owned()))
}
