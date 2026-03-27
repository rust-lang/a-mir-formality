//! Focused ambiguity tests for each of the `each_*` parser helpers.
//!
//! We define a reusable ambiguous nonterminal `Amb` that always
//! produces exactly 2 parses for the same input, then test how
//! each collection helper (`$?`, `$*`, `$,`, `$<>`, `$()`, `$[]`)
//! propagates that ambiguity.

use expect_test::expect;
use formality_core::parse::{self, CoreParse};
use formality_core::Set;
use formality_core::{term, test};

// ---- Shared ambiguous nonterminal ----

/// An ambiguous nonterminal: `a` can be parsed as either `AmbX(a)` or `AmbY(a)`.
/// Both consume the same input — same length, different reduction paths.
#[term]
pub enum Amb {
    #[grammar($v0)]
    AmbX(Id),
    #[grammar($v0)]
    AmbY(Id),
}

formality_core::id!(Id);

/// Checks both the number of complete parses and their textual representation.
///
/// The `expected_count` parameter guards against accidental changes even when
/// running `UPDATE_EXPECT=1` (which would silently update the snapshot but
/// not the count).
fn count_complete_parses<T: CoreParse<crate::ptt::FormalityLang> + std::fmt::Debug>(
    input: &str,
    expected_count: usize,
    expect: &expect_test::Expect,
) {
    let scope = Default::default();
    let parses = T::parse(&scope, input).unwrap();
    let complete: Vec<_> = parses
        .into_iter()
        .filter(|p| parse::skip_whitespace(p.text()).is_empty())
        .collect();
    let values: Vec<T> = complete
        .into_iter()
        .map(|p| p.finish().0)
        .collect();
    assert_eq!(
        values.len(),
        expected_count,
        "unexpected number of complete parses for {:?}",
        input,
    );
    expect.assert_debug_eq(&values);
}

// ========================================================================
// $? (each_opt_nonterminal)
// ========================================================================

/// `$?amb` — optional ambiguous child with input present.
/// Should produce 2 parses (one per Amb variant), both `Some`.
#[test]
fn opt_ambiguous_present() {
    #[term(opt $?amb)]
    pub struct OptAmb {
        amb: Option<Amb>,
    }

    count_complete_parses::<OptAmb>("opt a", 2, &expect![[r#"
        [
            OptAmb {
                amb: Some(
                    AmbX(
                        a,
                    ),
                ),
            },
            OptAmb {
                amb: Some(
                    AmbY(
                        a,
                    ),
                ),
            },
        ]
    "#]]);
}

/// `$?amb` — optional ambiguous child with input absent.
/// Should produce exactly 1 parse: `None`.
#[test]
fn opt_ambiguous_absent() {
    #[term(opt $?amb end)]
    pub struct OptAmbEnd {
        amb: Option<Amb>,
    }

    count_complete_parses::<OptAmbEnd>("opt end", 1, &expect![[r#"
        [
            OptAmbEnd {
                amb: None,
            },
        ]
    "#]]);
}

/// `$?amb` with a trailing required token — ambiguity from `$?` should
/// propagate through the rest of the parse.
#[test]
fn opt_ambiguous_with_suffix() {
    #[term(opt $?amb done)]
    pub struct OptAmbDone {
        amb: Option<Amb>,
    }

    count_complete_parses::<OptAmbDone>("opt a done", 2, &expect![[r#"
        [
            OptAmbDone {
                amb: Some(
                    AmbX(
                        a,
                    ),
                ),
            },
            OptAmbDone {
                amb: Some(
                    AmbY(
                        a,
                    ),
                ),
            },
        ]
    "#]]);
}

// ========================================================================
// $* (each_many_nonterminal)
// ========================================================================

/// `$*items` where each item is ambiguous.
/// One ambiguous item → 2 parses.
#[test]
fn many_one_ambiguous() {
    #[term(many $*items end)]
    pub struct ManyAmb {
        items: Vec<Amb>,
    }

    count_complete_parses::<ManyAmb>("many a end", 2, &expect![[r#"
        [
            ManyAmb {
                items: [
                    AmbX(
                        a,
                    ),
                ],
            },
            ManyAmb {
                items: [
                    AmbY(
                        a,
                    ),
                ],
            },
        ]
    "#]]);
}

/// `$*items` where two items are each ambiguous.
/// Two ambiguous items → 2 * 2 = 4 parses.
#[test]
fn many_two_ambiguous() {
    #[term(many $*items end)]
    pub struct ManyAmb {
        items: Vec<Amb>,
    }

    count_complete_parses::<ManyAmb>("many a b end", 4, &expect![[r#"
        [
            ManyAmb {
                items: [
                    AmbX(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                ],
            },
            ManyAmb {
                items: [
                    AmbX(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                ],
            },
            ManyAmb {
                items: [
                    AmbY(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                ],
            },
            ManyAmb {
                items: [
                    AmbY(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                ],
            },
        ]
    "#]]);
}

/// `$*items` where three items are each ambiguous.
/// Three ambiguous items → 2^3 = 8 parses.
#[test]
fn many_three_ambiguous() {
    #[term(many $*items end)]
    pub struct ManyAmb {
        items: Vec<Amb>,
    }

    count_complete_parses::<ManyAmb>("many a b c end", 8, &expect![[r#"
        [
            ManyAmb {
                items: [
                    AmbX(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                    AmbX(
                        c,
                    ),
                ],
            },
            ManyAmb {
                items: [
                    AmbX(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                    AmbY(
                        c,
                    ),
                ],
            },
            ManyAmb {
                items: [
                    AmbX(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                    AmbX(
                        c,
                    ),
                ],
            },
            ManyAmb {
                items: [
                    AmbX(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                    AmbY(
                        c,
                    ),
                ],
            },
            ManyAmb {
                items: [
                    AmbY(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                    AmbX(
                        c,
                    ),
                ],
            },
            ManyAmb {
                items: [
                    AmbY(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                    AmbY(
                        c,
                    ),
                ],
            },
            ManyAmb {
                items: [
                    AmbY(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                    AmbX(
                        c,
                    ),
                ],
            },
            ManyAmb {
                items: [
                    AmbY(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                    AmbY(
                        c,
                    ),
                ],
            },
        ]
    "#]]);
}

/// `$*items` with zero items should produce exactly 1 parse (empty vec).
#[test]
fn many_zero_items() {
    #[term(many $*items end)]
    pub struct ManyAmb {
        items: Vec<Amb>,
    }

    count_complete_parses::<ManyAmb>("many end", 1, &expect![[r#"
        [
            ManyAmb {
                items: [],
            },
        ]
    "#]]);
}

// ========================================================================
// $, (each_comma_nonterminal)
// ========================================================================

/// `$,items` — single ambiguous comma-separated item → 2 parses.
#[test]
fn comma_one_ambiguous() {
    #[term(comma $,items end)]
    pub struct CommaAmb {
        items: Vec<Amb>,
    }

    count_complete_parses::<CommaAmb>("comma a end", 2, &expect![[r#"
        [
            CommaAmb {
                items: [
                    AmbX(
                        a,
                    ),
                ],
            },
            CommaAmb {
                items: [
                    AmbY(
                        a,
                    ),
                ],
            },
        ]
    "#]]);
}

/// `$,items` — two ambiguous comma-separated items → 2 * 2 = 4 parses.
#[test]
fn comma_two_ambiguous() {
    #[term(comma $,items end)]
    pub struct CommaAmb {
        items: Vec<Amb>,
    }

    count_complete_parses::<CommaAmb>("comma a, b end", 4, &expect![[r#"
        [
            CommaAmb {
                items: [
                    AmbX(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                ],
            },
            CommaAmb {
                items: [
                    AmbX(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                ],
            },
            CommaAmb {
                items: [
                    AmbY(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                ],
            },
            CommaAmb {
                items: [
                    AmbY(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                ],
            },
        ]
    "#]]);
}

/// `$,items` — zero items → 1 parse.
#[test]
fn comma_zero_items() {
    #[term(comma $,items end)]
    pub struct CommaAmb {
        items: Vec<Amb>,
    }

    count_complete_parses::<CommaAmb>("comma end", 1, &expect![[r#"
        [
            CommaAmb {
                items: [],
            },
        ]
    "#]]);
}

// ========================================================================
// $<> (each_delimited_nonterminal — angle brackets)
// ========================================================================

/// `$<items>` — single ambiguous item in angle brackets → 2 parses.
#[test]
fn delimited_angle_one_ambiguous() {
    #[term(delim $<items>)]
    pub struct DelimAngle {
        items: Vec<Amb>,
    }

    count_complete_parses::<DelimAngle>("delim <a>", 2, &expect![[r#"
        [
            DelimAngle {
                items: [
                    AmbX(
                        a,
                    ),
                ],
            },
            DelimAngle {
                items: [
                    AmbY(
                        a,
                    ),
                ],
            },
        ]
    "#]]);
}

/// `$<items>` — two comma-separated ambiguous items → 4 parses.
#[test]
fn delimited_angle_two_ambiguous() {
    #[term(delim $<items>)]
    pub struct DelimAngle {
        items: Vec<Amb>,
    }

    count_complete_parses::<DelimAngle>("delim <a, b>", 4, &expect![[r#"
        [
            DelimAngle {
                items: [
                    AmbX(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                ],
            },
            DelimAngle {
                items: [
                    AmbX(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                ],
            },
            DelimAngle {
                items: [
                    AmbY(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                ],
            },
            DelimAngle {
                items: [
                    AmbY(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                ],
            },
        ]
    "#]]);
}

/// `$<items>` — empty angle brackets → 1 parse.
#[test]
fn delimited_angle_empty() {
    #[term(delim $<items>)]
    pub struct DelimAngle {
        items: Vec<Amb>,
    }

    count_complete_parses::<DelimAngle>("delim <>", 1, &expect![[r#"
        [
            DelimAngle {
                items: [],
            },
        ]
    "#]]);
}

/// `$<?items>` is optional — missing delimiters → 1 parse (empty vec).
#[test]
fn delimited_angle_absent() {
    #[term(delim $<?items> end)]
    pub struct DelimAngle {
        items: Vec<Amb>,
    }

    count_complete_parses::<DelimAngle>("delim end", 1, &expect![[r#"
        [
            DelimAngle {
                items: [],
            },
        ]
    "#]]);
    // When present, still propagates ambiguity
    count_complete_parses::<DelimAngle>("delim <a> end", 2, &expect![[r#"
        [
            DelimAngle {
                items: [
                    AmbX(
                        a,
                    ),
                ],
            },
            DelimAngle {
                items: [
                    AmbY(
                        a,
                    ),
                ],
            },
        ]
    "#]]);
}

// ========================================================================
// $() (each_delimited_nonterminal — parens)
// ========================================================================

/// `$(items)` — two ambiguous items in parens → 4 parses.
#[test]
fn delimited_paren_two_ambiguous() {
    #[term(delim $(items))]
    pub struct DelimParen {
        items: Vec<Amb>,
    }

    count_complete_parses::<DelimParen>("delim (a, b)", 4, &expect![[r#"
        [
            DelimParen {
                items: [
                    AmbX(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                ],
            },
            DelimParen {
                items: [
                    AmbX(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                ],
            },
            DelimParen {
                items: [
                    AmbY(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                ],
            },
            DelimParen {
                items: [
                    AmbY(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                ],
            },
        ]
    "#]]);
}

// ========================================================================
// $[] (each_delimited_nonterminal — square brackets)
// ========================================================================

/// `$[items]` — two ambiguous items in square brackets → 4 parses.
#[test]
fn delimited_bracket_two_ambiguous() {
    #[term(delim $[items])]
    pub struct DelimBracket {
        items: Vec<Amb>,
    }

    count_complete_parses::<DelimBracket>("delim [a, b]", 4, &expect![[r#"
        [
            DelimBracket {
                items: [
                    AmbX(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                ],
            },
            DelimBracket {
                items: [
                    AmbX(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                ],
            },
            DelimBracket {
                items: [
                    AmbY(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                ],
            },
            DelimBracket {
                items: [
                    AmbY(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                ],
            },
        ]
    "#]]);
}

// ========================================================================
// Combinations: multiple field modes in one struct
// ========================================================================

/// An ambiguous optional followed by an ambiguous required field.
/// With present optional: 2 (opt) * 2 (req) = 4 parses.
#[test]
fn opt_then_required_both_ambiguous() {
    #[term(start $?opt_item $req_item end)]
    pub struct OptThenReq {
        opt_item: Option<Amb>,
        req_item: Amb,
    }

    // "start a b end" — opt consumes `a` (2 ways), req consumes `b` (2 ways) → 4
    count_complete_parses::<OptThenReq>("start a b end", 4, &expect![[r#"
        [
            OptThenReq {
                opt_item: Some(
                    AmbX(
                        a,
                    ),
                ),
                req_item: AmbX(
                    b,
                ),
            },
            OptThenReq {
                opt_item: Some(
                    AmbX(
                        a,
                    ),
                ),
                req_item: AmbY(
                    b,
                ),
            },
            OptThenReq {
                opt_item: Some(
                    AmbY(
                        a,
                    ),
                ),
                req_item: AmbX(
                    b,
                ),
            },
            OptThenReq {
                opt_item: Some(
                    AmbY(
                        a,
                    ),
                ),
                req_item: AmbY(
                    b,
                ),
            },
        ]
    "#]]);
}

/// Two ambiguous fields where opt is absent.
/// 1 (None) * 2 (req) = 2 parses.
#[test]
fn opt_absent_then_required_ambiguous() {
    #[term(start $?opt_item then $req_item end)]
    pub struct OptThenReq {
        opt_item: Option<Amb>,
        req_item: Amb,
    }

    count_complete_parses::<OptThenReq>("start then a end", 2, &expect![[r#"
        [
            OptThenReq {
                opt_item: None,
                req_item: AmbX(
                    a,
                ),
            },
            OptThenReq {
                opt_item: None,
                req_item: AmbY(
                    a,
                ),
            },
        ]
    "#]]);
}

/// Ambiguous items inside a `$*` followed by an ambiguous required item.
/// `$*` gets one item (2 ways) + required (2 ways) = 4 total.
#[test]
fn many_then_required_both_ambiguous() {
    #[term(start $*items then $last end)]
    pub struct ManyThenReq {
        items: Vec<Amb>,
        last: Amb,
    }

    // "start a then b end" — many gets [a] (2 ways), last gets b (2 ways) → 4
    count_complete_parses::<ManyThenReq>("start a then b end", 4, &expect![[r#"
        [
            ManyThenReq {
                items: [
                    AmbX(
                        a,
                    ),
                ],
                last: AmbX(
                    b,
                ),
            },
            ManyThenReq {
                items: [
                    AmbX(
                        a,
                    ),
                ],
                last: AmbY(
                    b,
                ),
            },
            ManyThenReq {
                items: [
                    AmbY(
                        a,
                    ),
                ],
                last: AmbX(
                    b,
                ),
            },
            ManyThenReq {
                items: [
                    AmbY(
                        a,
                    ),
                ],
                last: AmbY(
                    b,
                ),
            },
        ]
    "#]]);
}

/// Ambiguous items in angle brackets followed by another ambiguous item.
/// `<a, b>` (4 ways) * `c` (2 ways) = 8 total.
#[test]
fn delimited_then_required_both_ambiguous() {
    #[term(start $<items> $last)]
    pub struct DelimThenReq {
        items: Vec<Amb>,
        last: Amb,
    }

    count_complete_parses::<DelimThenReq>("start <a, b> c", 8, &expect![[r#"
        [
            DelimThenReq {
                items: [
                    AmbX(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                ],
                last: AmbX(
                    c,
                ),
            },
            DelimThenReq {
                items: [
                    AmbX(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                ],
                last: AmbY(
                    c,
                ),
            },
            DelimThenReq {
                items: [
                    AmbX(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                ],
                last: AmbX(
                    c,
                ),
            },
            DelimThenReq {
                items: [
                    AmbX(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                ],
                last: AmbY(
                    c,
                ),
            },
            DelimThenReq {
                items: [
                    AmbY(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                ],
                last: AmbX(
                    c,
                ),
            },
            DelimThenReq {
                items: [
                    AmbY(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                ],
                last: AmbY(
                    c,
                ),
            },
            DelimThenReq {
                items: [
                    AmbY(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                ],
                last: AmbX(
                    c,
                ),
            },
            DelimThenReq {
                items: [
                    AmbY(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                ],
                last: AmbY(
                    c,
                ),
            },
        ]
    "#]]);
}

// ========================================================================
// Ambiguity in enum variants with collections
// ========================================================================

/// Enum where one variant uses `$*` and another uses `$,` —
/// each should independently propagate ambiguity.
#[test]
fn enum_variants_with_collections() {
    #[term]
    pub enum Wrapper {
        #[grammar(many $*v0 end)]
        Many(Vec<Amb>),

        #[grammar(comma $,v0 end)]
        Comma(Vec<Amb>),
    }

    // "many a b end" → Many variant, 2 items, 2^2 = 4
    count_complete_parses::<Wrapper>("many a b end", 4, &expect![[r#"
        [
            Many(
                [
                    AmbX(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                ],
            ),
            Many(
                [
                    AmbX(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                ],
            ),
            Many(
                [
                    AmbY(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                ],
            ),
            Many(
                [
                    AmbY(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                ],
            ),
        ]
    "#]]);

    // "comma a, b end" → Comma variant, 2 items, 2^2 = 4
    count_complete_parses::<Wrapper>("comma a, b end", 4, &expect![[r#"
        [
            Comma(
                [
                    AmbX(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                ],
            ),
            Comma(
                [
                    AmbX(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                ],
            ),
            Comma(
                [
                    AmbY(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                ],
            ),
            Comma(
                [
                    AmbY(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                ],
            ),
        ]
    "#]]);
}

// ========================================================================
// Nested ambiguity — ambiguous wrapper inside a collection
// ========================================================================

/// A wrapper that is itself ambiguous, used inside `$*`.
/// Each element has 2 parses, so n elements → 2^n total.
#[test]
fn many_of_ambiguous_wrapper() {
    // AmbWrap is ambiguous for the same reason as Amb:
    // two variants that parse identically.
    #[term]
    pub enum AmbWrap {
        #[grammar(( $v0 ))]
        WrapA(Amb),
        #[grammar(( $v0 ))]
        WrapB(Amb),
    }

    #[term(list $*items end)]
    pub struct ListWrap {
        items: Vec<AmbWrap>,
    }

    // One element `(a)`: AmbWrap has 2 parses (WrapA/WrapB) * Amb has 2 parses each = 4
    count_complete_parses::<ListWrap>("list (a) end", 4, &expect![[r#"
        [
            ListWrap {
                items: [
                    WrapA(
                        AmbX(
                            a,
                        ),
                    ),
                ],
            },
            ListWrap {
                items: [
                    WrapA(
                        AmbY(
                            a,
                        ),
                    ),
                ],
            },
            ListWrap {
                items: [
                    WrapB(
                        AmbX(
                            a,
                        ),
                    ),
                ],
            },
            ListWrap {
                items: [
                    WrapB(
                        AmbY(
                            a,
                        ),
                    ),
                ],
            },
        ]
    "#]]);
}

// ========================================================================
// Unambiguous nonterminal — baseline: all helpers produce exactly 1 parse
// ========================================================================

/// Unambiguous nonterminal — exactly one parse regardless of helper.
#[term]
pub enum Unamb {
    #[grammar($v0)]
    Only(Id),
}

#[test]
fn opt_unambiguous() {
    #[term(opt $?item end)]
    pub struct OptUnamb {
        item: Option<Unamb>,
    }

    count_complete_parses::<OptUnamb>("opt a end", 1, &expect![[r#"
        [
            OptUnamb {
                item: Some(
                    Only(
                        a,
                    ),
                ),
            },
        ]
    "#]]);
    count_complete_parses::<OptUnamb>("opt end", 1, &expect![[r#"
        [
            OptUnamb {
                item: None,
            },
        ]
    "#]]);
}

#[test]
fn many_unambiguous() {
    #[term(many $*items end)]
    pub struct ManyUnamb {
        items: Vec<Unamb>,
    }

    count_complete_parses::<ManyUnamb>("many a b c end", 1, &expect![[r#"
        [
            ManyUnamb {
                items: [
                    Only(
                        a,
                    ),
                    Only(
                        b,
                    ),
                    Only(
                        c,
                    ),
                ],
            },
        ]
    "#]]);
}

#[test]
fn comma_unambiguous() {
    #[term(comma $,items end)]
    pub struct CommaUnamb {
        items: Vec<Unamb>,
    }

    count_complete_parses::<CommaUnamb>("comma a, b, c end", 1, &expect![[r#"
        [
            CommaUnamb {
                items: [
                    Only(
                        a,
                    ),
                    Only(
                        b,
                    ),
                    Only(
                        c,
                    ),
                ],
            },
        ]
    "#]]);
}

#[test]
fn delimited_unambiguous() {
    #[term(delim $<items>)]
    pub struct DelimUnamb {
        items: Vec<Unamb>,
    }

    count_complete_parses::<DelimUnamb>("delim <a, b, c>", 1, &expect![[r#"
        [
            DelimUnamb {
                items: [
                    Only(
                        a,
                    ),
                    Only(
                        b,
                    ),
                    Only(
                        c,
                    ),
                ],
            },
        ]
    "#]]);
}

// ========================================================================
// Set<T> collection type (each_comma_nonterminal / each_delimited_nonterminal
// generalized over FromIterator — regression tests for collection genericity)
// ========================================================================

/// `$,items` into a `Set<Unamb>` — two comma-separated items → 1 parse.
#[test]
fn comma_into_set_two_items() {
    #[term(comma $,items end)]
    pub struct CommaSet {
        items: Set<Unamb>,
    }

    count_complete_parses::<CommaSet>("comma a, b end", 1, &expect![[r#"
        [
            CommaSet {
                items: {
                    Only(
                        a,
                    ),
                    Only(
                        b,
                    ),
                },
            },
        ]
    "#]]);
}

/// `$,items` into a `Set<Unamb>` — zero items → 1 parse (empty set).
#[test]
fn comma_into_set_zero_items() {
    #[term(comma $,items end)]
    pub struct CommaSet {
        items: Set<Unamb>,
    }

    count_complete_parses::<CommaSet>("comma end", 1, &expect![[r#"
        [
            CommaSet {
                items: {},
            },
        ]
    "#]]);
}

/// `$<items>` into a `Set<Unamb>` — angle-bracket delimited items.
#[test]
fn delimited_angle_into_set() {
    #[term(delim $<items>)]
    pub struct DelimAngleSet {
        items: Set<Unamb>,
    }

    count_complete_parses::<DelimAngleSet>("delim <a, b, c>", 1, &expect![[r#"
        [
            DelimAngleSet {
                items: {
                    Only(
                        a,
                    ),
                    Only(
                        b,
                    ),
                    Only(
                        c,
                    ),
                },
            },
        ]
    "#]]);
}

/// `$<items>` into a `Set<Unamb>` — empty angle brackets → 1 parse.
#[test]
fn delimited_angle_into_set_empty() {
    #[term(delim $<items>)]
    pub struct DelimAngleSet {
        items: Set<Unamb>,
    }

    count_complete_parses::<DelimAngleSet>("delim <>", 1, &expect![[r#"
        [
            DelimAngleSet {
                items: {},
            },
        ]
    "#]]);
}

/// `$<?items>` into a `Set<Unamb>` — optional, absent → 1 parse (empty set).
#[test]
fn delimited_angle_into_set_optional_absent() {
    #[term(delim $<?items> end)]
    pub struct DelimAngleSet {
        items: Set<Unamb>,
    }

    count_complete_parses::<DelimAngleSet>("delim end", 1, &expect![[r#"
        [
            DelimAngleSet {
                items: {},
            },
        ]
    "#]]);
    count_complete_parses::<DelimAngleSet>("delim <a, b> end", 1, &expect![[r#"
        [
            DelimAngleSet {
                items: {
                    Only(
                        a,
                    ),
                    Only(
                        b,
                    ),
                },
            },
        ]
    "#]]);
}

/// `$(items)` into a `Set<Unamb>` — paren-delimited into Set.
#[test]
fn delimited_paren_into_set() {
    #[term(delim $(items))]
    pub struct DelimParenSet {
        items: Set<Unamb>,
    }

    count_complete_parses::<DelimParenSet>("delim (a, b)", 1, &expect![[r#"
        [
            DelimParenSet {
                items: {
                    Only(
                        a,
                    ),
                    Only(
                        b,
                    ),
                },
            },
        ]
    "#]]);
}

/// `$[items]` into a `Set<Unamb>` — bracket-delimited into Set.
#[test]
fn delimited_bracket_into_set() {
    #[term(delim $[items])]
    pub struct DelimBracketSet {
        items: Set<Unamb>,
    }

    count_complete_parses::<DelimBracketSet>("delim [a, b]", 1, &expect![[r#"
        [
            DelimBracketSet {
                items: {
                    Only(
                        a,
                    ),
                    Only(
                        b,
                    ),
                },
            },
        ]
    "#]]);
}

/// `$,items` into `Set<Amb>` — ambiguous elements into Set.
/// Two ambiguous items → 2 * 2 = 4 parses (Set doesn't collapse ambiguity
/// because AmbX(a) ≠ AmbY(a), so both variants remain distinct in the set).
#[test]
fn comma_into_set_ambiguous() {
    #[term(comma $,items end)]
    pub struct CommaSetAmb {
        items: Set<Amb>,
    }

    count_complete_parses::<CommaSetAmb>("comma a end", 2, &expect![[r#"
        [
            CommaSetAmb {
                items: {
                    AmbX(
                        a,
                    ),
                },
            },
            CommaSetAmb {
                items: {
                    AmbY(
                        a,
                    ),
                },
            },
        ]
    "#]]);
    count_complete_parses::<CommaSetAmb>("comma a, b end", 4, &expect![[r#"
        [
            CommaSetAmb {
                items: {
                    AmbX(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                },
            },
            CommaSetAmb {
                items: {
                    AmbX(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                },
            },
            CommaSetAmb {
                items: {
                    AmbX(
                        b,
                    ),
                    AmbY(
                        a,
                    ),
                },
            },
            CommaSetAmb {
                items: {
                    AmbY(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                },
            },
        ]
    "#]]);
}

/// `$<items>` into `Set<Amb>` — ambiguous elements in angle brackets into Set.
#[test]
fn delimited_angle_into_set_ambiguous() {
    #[term(delim $<items>)]
    pub struct DelimAngleSetAmb {
        items: Set<Amb>,
    }

    count_complete_parses::<DelimAngleSetAmb>("delim <a, b>", 4, &expect![[r#"
        [
            DelimAngleSetAmb {
                items: {
                    AmbX(
                        a,
                    ),
                    AmbX(
                        b,
                    ),
                },
            },
            DelimAngleSetAmb {
                items: {
                    AmbX(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                },
            },
            DelimAngleSetAmb {
                items: {
                    AmbX(
                        b,
                    ),
                    AmbY(
                        a,
                    ),
                },
            },
            DelimAngleSetAmb {
                items: {
                    AmbY(
                        a,
                    ),
                    AmbY(
                        b,
                    ),
                },
            },
        ]
    "#]]);
}
