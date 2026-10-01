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
    /// `#![feature(branch_specialization)]`: enables `may_spec(..)`
    /// where-clauses and `if impls .. { } else { }` statements.
    #[grammar(branch_specialization)]
    BranchSpecialization,
    /// `#![feature(spec_commit_and_verify)]`: a decision may leave region
    /// constraints to the borrow checker (strict, the default, may not).
    #[grammar(spec_commit_and_verify)]
    SpecCommitAndVerify,
    /// `#![feature(spec_bail_on_regions)]` (`try_as_dyn`): no `may_spec`;
    /// codegen decides per monomorphization, "yes" only if the bound holds for
    /// every choice of its erased lifetimes.
    #[grammar(spec_bail_on_regions)]
    SpecBailOnRegions,
    /// `#![feature(spec_always_applicable)]`: a bound is decided as holding
    /// only if it holds for every choice of its lifetimes; and
    /// `always_applicable` traits.
    #[grammar(spec_always_applicable)]
    SpecAlwaysApplicable,
}
