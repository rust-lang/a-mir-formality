use formality_core::term;

#[term(#![feature($name)])]
pub struct FeatureGate {
    pub name: FeatureGateName,
}

#[term]
#[derive(Copy)]
pub enum FeatureGateName {
    /// The most permissive borrow-check mode: it should accept everything we
    /// ever intend to accept, and exists to describe the target the other
    /// modes are approximations of.
    ///
    /// Deliberately *not* a model of rustc's `-Z polonius=legacy`. The datalog
    /// implementation is a dead end, and some of what it rejects it rejects
    /// because of how it derives errors rather than because the analysis
    /// requires it; reproducing that would mean building in imprecision we
    /// have no intention of keeping. Where the two disagree, the difference is
    /// documented on the test (see `loan_added_after_subset_edge` in
    /// `tests/borrowck.rs`).
    #[grammar(polonius_unlocked)]
    PoloniusUnlocked,

    /// Analogous to rustc's `-Z polonius=next`: location-sensitive loan
    /// liveness, but universal-region errors are still checked over the whole
    /// body.
    #[grammar(polonius_alpha)]
    PoloniusAlpha,

    #[grammar(non_lifetime_binders)]
    NonLifetimeBinders,
    // #![feature(negative_impls)]
    #[grammar(negative_impls)]
    NegativeImpls,
}
