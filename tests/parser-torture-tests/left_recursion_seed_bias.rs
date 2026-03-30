//! Tests for left-recursion seed behavior.
//!
//! The left-recursion fixed-point loop accumulates all parses as seeds,
//! so both left- and right-associative compositions are discovered.

use formality_core::parse::{self, CoreParse};
use formality_core::{term, test};
use std::sync::Arc;

// ============================================================================
// Grammar modeling dada-model's Perm enum (simplified)
// ============================================================================

/// A simplified version of dada-model's `Perm`:
///
/// ```text
/// Perm = "leaf"               // a leaf permission (like `given`, `shared`)
///      | $id                  // a variable (like Q)
///      | Perm Perm            // application / composition (left-recursive)
/// ```
///
/// The key property: `Apply(Perm, Perm)` has no delimiter between the two
/// operands, so `leaf leaf leaf` is ambiguous between left- and right-association.
#[term]
pub enum Perm {
    #[grammar(leaf)]
    Leaf,

    #[cast]
    Id(PermId),

    #[grammar($v0 $v1)]
    Apply(Arc<Perm>, Arc<Perm>),
}

formality_core::id!(PermId);

/// Collect all complete parses (no remaining input), sorted for deterministic output.
fn all_parses<T: CoreParse<crate::ptt::FormalityLang> + Ord>(input: &str) -> Vec<T>
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
// Test 1: Two leaves — should be unambiguous
// ============================================================================

#[test]
fn two_leaves() {
    let results = all_parses::<Perm>("leaf leaf");
    assert_eq!(results.len(), 1);
    expect_test::expect![[r#"
        [
            Apply(
                Leaf,
                Leaf,
            ),
        ]
    "#]]
    .assert_debug_eq(&results);
}

// ============================================================================
// Test 2: Three leaves — should produce BOTH associativities
// ============================================================================

/// Parsing `leaf leaf leaf` as `Perm` should produce both:
/// - `Apply(Leaf, Apply(Leaf, Leaf))` (right-associative)
/// - `Apply(Apply(Leaf, Leaf), Leaf)` (left-associative)
#[test]
fn three_leaves_both_associativities() {
    let results = all_parses::<Perm>("leaf leaf leaf");
    assert_eq!(results.len(), 2);
    expect_test::expect![[r#"
        [
            Apply(
                Leaf,
                Apply(
                    Leaf,
                    Leaf,
                ),
            ),
            Apply(
                Apply(
                    Leaf,
                    Leaf,
                ),
                Leaf,
            ),
        ]
    "#]]
    .assert_debug_eq(&results);
}

// ============================================================================
// Test 3: Four leaves — Catalan(3) = 5 distinct binary trees
// ============================================================================

#[test]
fn four_leaves_all_trees() {
    let results = all_parses::<Perm>("leaf leaf leaf leaf");
    assert_eq!(results.len(), 5);
    expect_test::expect![[r#"
        [
            Apply(
                Leaf,
                Apply(
                    Leaf,
                    Apply(
                        Leaf,
                        Leaf,
                    ),
                ),
            ),
            Apply(
                Leaf,
                Apply(
                    Apply(
                        Leaf,
                        Leaf,
                    ),
                    Leaf,
                ),
            ),
            Apply(
                Apply(
                    Leaf,
                    Leaf,
                ),
                Apply(
                    Leaf,
                    Leaf,
                ),
            ),
            Apply(
                Apply(
                    Leaf,
                    Apply(
                        Leaf,
                        Leaf,
                    ),
                ),
                Leaf,
            ),
            Apply(
                Apply(
                    Apply(
                        Leaf,
                        Leaf,
                    ),
                    Leaf,
                ),
                Leaf,
            ),
        ]
    "#]]
    .assert_debug_eq(&results);
}

// ============================================================================
// Test 4: Cross-type ambiguity (Ty using Perm)
// ============================================================================

/// Models dada-model's Ty, which can apply a Perm to a Ty.
///
/// ```text
/// Ty = Perm Ty              // apply_perm: apply a permission to a type
///    | $id                   // named type
/// ```
///
/// When parsing `leaf x Data`:
/// - Interpretation 1: `ApplyPerm(Leaf, ApplyPerm(Id(x), Named(Data)))` — perm is just `leaf`
/// - Interpretation 2: `ApplyPerm(Apply(Leaf, Id(x)), Named(Data))` — perm is `leaf x`
///
/// Both consume all input but have different structure.
#[term]
pub enum Ty {
    #[grammar($v0)]
    Named(TyId),

    #[grammar($v0 $v1)]
    ApplyPerm(Perm, Arc<Ty>),
}

formality_core::id!(TyId);

/// With just one leaf perm and a named type, no ambiguity.
#[test]
fn ty_simple_perm_application() {
    let results = all_parses::<Ty>("leaf Data");
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

/// `leaf x Data` exposes the cross-type ambiguity.
#[test]
fn ty_cross_type_ambiguity() {
    let results = all_parses::<Ty>("leaf x Data");
    assert_eq!(results.len(), 2);
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
            ApplyPerm(
                Apply(
                    Leaf,
                    Id(
                        x,
                    ),
                ),
                Named(
                    Data,
                ),
            ),
        ]
    "#]]
    .assert_debug_eq(&results);
}

/// Two perm-variables before a named type: `leaf x y Data`.
/// Even more splitting possibilities.
#[test]
fn ty_deeper_cross_type_ambiguity() {
    let results = all_parses::<Ty>("leaf x y Data");
    assert_eq!(results.len(), 5);
    expect_test::expect![[r#"
        [
            ApplyPerm(
                Leaf,
                ApplyPerm(
                    Id(
                        x,
                    ),
                    ApplyPerm(
                        Id(
                            y,
                        ),
                        Named(
                            Data,
                        ),
                    ),
                ),
            ),
            ApplyPerm(
                Leaf,
                ApplyPerm(
                    Apply(
                        Id(
                            x,
                        ),
                        Id(
                            y,
                        ),
                    ),
                    Named(
                        Data,
                    ),
                ),
            ),
            ApplyPerm(
                Apply(
                    Leaf,
                    Id(
                        x,
                    ),
                ),
                ApplyPerm(
                    Id(
                        y,
                    ),
                    Named(
                        Data,
                    ),
                ),
            ),
            ApplyPerm(
                Apply(
                    Leaf,
                    Apply(
                        Id(
                            x,
                        ),
                        Id(
                            y,
                        ),
                    ),
                ),
                Named(
                    Data,
                ),
            ),
            ApplyPerm(
                Apply(
                    Apply(
                        Leaf,
                        Id(
                            x,
                        ),
                    ),
                    Id(
                        y,
                    ),
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
// Test 5: Verify both associativities are present
// ============================================================================

#[test]
fn verify_seed_trace() {
    let results = all_parses::<Perm>("leaf leaf leaf");
    assert_eq!(results.len(), 2);
    expect_test::expect![[r#"
        [
            Apply(
                Leaf,
                Apply(
                    Leaf,
                    Leaf,
                ),
            ),
            Apply(
                Apply(
                    Leaf,
                    Leaf,
                ),
                Leaf,
            ),
        ]
    "#]]
    .assert_debug_eq(&results);
}
