//! Tests for the `#[reject]` attribute described in wip.md.
//!
//! `#[reject(PATTERN)]` on a variant or struct silently drops parse results
//! that match the given pattern. Multiple `#[reject]` attributes are OR'd.

use formality_core::parse::{self, CoreParse};
use formality_core::{term, test};
use std::sync::Arc;

// ============================================================================
// Grammar: reuse the Perm/Ty pattern from left_recursion_seed_bias,
// but with #[reject] to disambiguate.
// ============================================================================

// ANCHOR: reject_example
#[term]
pub enum Perm {
    #[grammar(leaf)]
    Leaf,

    #[grammar(given)]
    Given,

    #[cast]
    Id(PermId),

    #[grammar($v0 $v1)]
    Apply(Arc<Perm>, Arc<Perm>),
}

formality_core::id!(PermId);

#[term]
pub enum Ty {
    #[grammar($v0)]
    Named(TyId),

    #[grammar($v0 $v1)]
    #[reject(Perm::Apply(..), _)]
    ApplyPerm(Perm, Arc<Ty>),
}

formality_core::id!(TyId);
// ANCHOR_END: reject_example

/// Collect all complete parses (no remaining input), sorted for deterministic output.
fn complete_parses<T: CoreParse<crate::ptt::FormalityLang> + Ord>(input: &str) -> Vec<T>
where
    T: Clone,
{
    let scope: formality_core::parse::Scope<crate::FormalityLang> = Default::default();
    let mut results: Vec<T> = match T::parse(&scope, input) {
        Ok(parses) => parses
            .into_iter()
            .filter(|p| parse::skip_whitespace(p.text()).is_empty())
            .map(|p| p.finish().0)
            .collect(),
        Err(_) => vec![],
    };
    results.sort();
    results
}

// ============================================================================
// Test 1: Basic #[reject] — compound perm rejected in ApplyPerm
// ============================================================================

/// `leaf x Data` should produce exactly 1 parse when compound perms are rejected.
/// Without #[reject], it produces 2 (the cross-type ambiguity from wip.md).
#[test]
fn reject_cross_type_ambiguity() {
    let results = complete_parses::<Ty>("leaf x Data");
    assert_eq!(results.len(), 1);
    // With #[reject(Perm::Apply(..), _)], the interpretation where
    // perm = Apply(Leaf, Id(x)) is rejected, leaving only
    // perm = Leaf, ty = ApplyPerm(Id(x), Named(Data)).
    expect_test::expect![[r#"
        [
            ApplyPerm(
                Leaf,
                ApplyPerm(
                    Id(
                        x,
                    ),
                    Named(
                        Data,
                    ),
                ),
            ),
        ]
    "#]]
    .assert_debug_eq(&results);
}

/// Simple case with no ambiguity — reject doesn't affect valid parses.
#[test]
fn reject_no_false_positive() {
    let results = complete_parses::<Ty>("leaf Data");
    assert_eq!(results.len(), 1);
    expect_test::expect![[r#"
        [
            ApplyPerm(
                Leaf,
                Named(
                    Data,
                ),
            ),
        ]
    "#]]
    .assert_debug_eq(&results);
}

// ============================================================================
// Test 2: Unlisted fields are implicitly unconstrained
// ============================================================================

#[term]
pub enum TyShort {
    #[grammar($v0)]
    Named(TyId),

    #[grammar($v0 $v1)]
    #[reject(Perm::Apply(..), ..)]
    ApplyPerm(Perm, Arc<TyShort>),
}

/// Only the first field is listed in #[reject]; the second is skipped with
/// trailing `..`. Should behave the same as #[reject(Perm::Apply(..), _)].
#[test]
fn reject_unlisted_fields_unconstrained() {
    let results = complete_parses::<TyShort>("leaf x Data");
    assert_eq!(results.len(), 1);
    expect_test::expect![[r#"
        [
            ApplyPerm(
                Leaf,
                ApplyPerm(
                    Id(
                        x,
                    ),
                    Named(
                        Data,
                    ),
                ),
            ),
        ]
    "#]]
    .assert_debug_eq(&results);
}

// ============================================================================
// Test 3: Multiple #[reject] (OR semantics)
// ============================================================================

#[term]
pub enum Ty2 {
    #[grammar($v0)]
    Named(TyId),

    #[grammar($v0 $v1)]
    #[reject(Perm::Apply(..), _)]
    #[reject(Perm::Leaf, _)]
    ApplyPerm(Perm, Arc<Ty2>),
}

/// Both compound perms AND `Leaf` are rejected — only ids pass as the perm.
#[test]
fn reject_multiple_or() {
    // `leaf Data` should be rejected because Perm::Leaf matches the second reject
    let results = complete_parses::<Ty2>("leaf Data");
    assert_eq!(results.len(), 0);
    expect_test::expect![[r#"
        []
    "#]]
    .assert_debug_eq(&results);

    // `x Data` should still work — x is a PermId, neither reject matches
    let results = complete_parses::<Ty2>("x Data");
    assert_eq!(results.len(), 1);
    expect_test::expect![[r#"
        [
            ApplyPerm(
                Id(
                    x,
                ),
                Named(
                    Data,
                ),
            ),
        ]
    "#]]
    .assert_debug_eq(&results);
}

// ============================================================================
// Test 3: Named fields
// ============================================================================

#[term($perm $ty)]
#[reject(perm: Perm::Apply(..), ty: _)]
pub struct ApplyPermStruct {
    perm: Perm,
    ty: Arc<Ty>,
}

#[test]
fn reject_named_fields() {
    let results = complete_parses::<ApplyPermStruct>("leaf x Data");
    assert_eq!(results.len(), 1);
    expect_test::expect![[r#"
        [
            ApplyPermStruct {
                perm: Leaf,
                ty: ApplyPerm(
                    Id(
                        x,
                    ),
                    Named(
                        Data,
                    ),
                ),
            },
        ]
    "#]]
    .assert_debug_eq(&results);
}

// ============================================================================
// Test 5: Arc-wrapped fields (DowncastFrom interaction)
// ============================================================================

/// `Perm::Apply(..)` works even though field 0 is `Perm` (not `Arc<Perm>`),
/// because DowncastFrom unwraps the outer layer. This test confirms that
/// the basic DowncastFrom path works correctly for the reject check.
#[test]
fn reject_arc_wrapped_field() {
    // Ty already has Arc<Ty> as field 1 — the #[reject] on Ty::ApplyPerm
    // uses `_` for field 1 so it doesn't test the Arc unwrapping directly.
    // The key test is that field 0 (Perm, not Arc) works via DowncastFrom.
    let results = complete_parses::<Ty>("leaf x Data");
    assert_eq!(results.len(), 1);
    expect_test::expect![[r#"
        [
            ApplyPerm(
                Leaf,
                ApplyPerm(
                    Id(
                        x,
                    ),
                    Named(
                        Data,
                    ),
                ),
            ),
        ]
    "#]]
    .assert_debug_eq(&results);
}

// ============================================================================
// Test 6: Wildcard _ passes everything
// ============================================================================

#[term]
pub enum Ty6 {
    #[grammar($v0)]
    Named(TyId),

    #[grammar($v0 $v1)]
    #[reject(_, _)]
    ApplyPerm(Perm, Arc<Ty6>),
}

/// `#[reject(_, _)]` — all fields are wildcards, so nothing is constrained
/// and every parse through this variant is rejected.
#[test]
fn reject_all_wildcards_rejects_everything() {
    let results = complete_parses::<Ty6>("leaf Data");
    assert_eq!(results.len(), 0);
    expect_test::expect![[r#"
        []
    "#]]
    .assert_debug_eq(&results);
}

// ============================================================================
// Test 7: Rejection + left-recursion interaction
// ============================================================================

/// When #[reject] is on the same type doing left-recursion, rejected values
/// should not become seeds.
#[term]
pub enum PermRestricted {
    #[grammar(leaf)]
    Leaf,

    #[grammar(given)]
    Given,

    #[grammar($v0 $v1)]
    #[reject(PermRestricted::Leaf, PermRestricted::Given)]
    Apply(Arc<PermRestricted>, Arc<PermRestricted>),
}

#[test]
fn reject_same_type_left_recursion() {
    // `leaf given` — Apply(Leaf, Given) matches the reject pattern, so rejected.
    let results = complete_parses::<PermRestricted>("leaf given");
    assert_eq!(results.len(), 0);
    expect_test::expect![[r#"
        []
    "#]]
    .assert_debug_eq(&results);

    // `given leaf` — Apply(Given, Leaf) does NOT match the reject pattern
    // (reject requires field 0 = Leaf AND field 1 = Given).
    let results = complete_parses::<PermRestricted>("given leaf");
    assert_eq!(results.len(), 1);
    expect_test::expect![[r#"
        [
            Apply(
                Given,
                Leaf,
            ),
        ]
    "#]]
    .assert_debug_eq(&results);
}
