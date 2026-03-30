use formality_core::parse::CoreParse;
use formality_core::{parse, term, test};
use std::sync::Arc;

#[test]
fn reduce_reduce_ok() {
    #[term]
    pub enum Root {
        #[cast]
        ClassTy(ClassTy),
        #[cast]
        Perm(Perm),
    }

    #[term($perm $class_id)]
    pub struct ClassTy {
        perm: Perm,
        class_id: Id,
    }

    #[term]
    pub enum Perm {
        My,
        Our,
    }

    formality_core::id!(Id);

    let term: Root = crate::ptt::term("my String");
    expect_test::expect![[r#"
        ClassTy(
            ClassTy {
                perm: My,
                class_id: String,
            },
        )
    "#]]
    .assert_debug_eq(&term);
}

#[test]
fn reduce_reduce_ambig() {
    #[term]
    pub enum Root {
        #[grammar($v0)]
        OneId(Id),
        #[grammar($v0 $v1)]
        TwoId(Id, Id),
        #[grammar($v0 $v1)]
        TwoRr(Arc<Root>, Arc<Root>),
    }

    formality_core::id!(Id);

    // This can be parsed in multiple ways (using a variant of Reverse Polish Notation)
    // and none is obviously better than the other:
    //
    // Root = ((Id Root::OneId) (Id Id Root::TwoId) Root::TwoRr)
    // Root = (Id Id Root::TwoId) (Id Root::OneId) Root::TwoRr)
    // Root = ((Id Root::OneId) (Id Root::OneId) (Id Root::OneId) Root::TwoRr)
    //
    // The parser finds multiple complete parses.
    // Use raw parse to inspect all results rather than `term()` which panics on ambiguity.
    let scope: formality_core::parse::Scope<crate::FormalityLang> = Default::default();
    let parses = Root::parse(&scope, "a b c").unwrap();
    let mut complete: Vec<Root> = parses
        .into_iter()
        .filter(|p| parse::skip_whitespace(p.text()).is_empty())
        .map(|p| p.finish().0)
        .collect();
    complete.sort();

    assert_eq!(complete.len(), 4);
    expect_test::expect![[r#"
        [
            TwoRr(
                OneId(
                    a,
                ),
                TwoId(
                    b,
                    c,
                ),
            ),
            TwoRr(
                OneId(
                    a,
                ),
                TwoRr(
                    OneId(
                        b,
                    ),
                    OneId(
                        c,
                    ),
                ),
            ),
            TwoRr(
                TwoId(
                    a,
                    b,
                ),
                OneId(
                    c,
                ),
            ),
            TwoRr(
                TwoRr(
                    OneId(
                        a,
                    ),
                    OneId(
                        b,
                    ),
                ),
                OneId(
                    c,
                ),
            ),
        ]
    "#]]
    .assert_debug_eq(&complete);
}

/// Test that deep ambiguity propagates through child nonterminals.
///
/// Here, `Inner` is ambiguous: the input `a` can be parsed as either
/// `InnerA(a)` or `InnerB(a)` — same input consumed, same length,
/// different reduction paths. The parent `Outer` wraps an `Inner`,
/// so the ambiguity should propagate upward, producing two complete
/// parses of `Outer`.
#[test]
fn deep_ambiguity_propagates() {
    #[term]
    pub enum Inner {
        #[grammar($v0)]
        InnerA(Id),
        #[grammar($v0)]
        InnerB(Id),
    }

    #[term($inner)]
    pub struct Outer {
        inner: Inner,
    }

    formality_core::id!(Id);

    // Parse at the raw level to see all successful parses.
    let scope = Default::default();
    let parses = Outer::parse(&scope, "a").unwrap();

    // Filter to complete parses (consumed all input).
    let complete: Vec<_> = parses
        .into_iter()
        .filter(|p| parse::skip_whitespace(p.text()).is_empty())
        .collect();

    // We should get exactly 2 complete parses: one via InnerA, one via InnerB.
    assert_eq!(
        complete.len(),
        2,
        "expected 2 ambiguous parses, got {}: {complete:#?}",
        complete.len()
    );
}

/// Test that deep ambiguity through a sequence of two nonterminals
/// produces the full cross-product of possibilities.
///
/// `Child` has two parses for `a` (ChildX and ChildY).
/// `Parent` is `$left . $right` where both are `Child`.
/// So `a . a` should produce 2 x 2 = 4 complete parses.
#[test]
fn deep_ambiguity_cross_product() {
    #[term]
    pub enum Child {
        #[grammar($v0)]
        ChildX(Id),
        #[grammar($v0)]
        ChildY(Id),
    }

    #[term($left . $right)]
    pub struct Parent {
        left: Child,
        right: Child,
    }

    formality_core::id!(Id);

    let scope = Default::default();
    let parses = Parent::parse(&scope, "a . a").unwrap();

    let complete: Vec<_> = parses
        .into_iter()
        .filter(|p| parse::skip_whitespace(p.text()).is_empty())
        .collect();

    // 2 parses for left * 2 parses for right = 4 total
    assert_eq!(
        complete.len(),
        4,
        "expected 4 ambiguous parses (2x2 cross product), got {}: {complete:#?}",
        complete.len()
    );
}
