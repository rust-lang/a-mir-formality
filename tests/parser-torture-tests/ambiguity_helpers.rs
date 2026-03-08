//! Focused ambiguity tests for each of the `each_*` parser helpers.
//!
//! We define a reusable ambiguous nonterminal `Amb` that always
//! produces exactly 2 parses for the same input, then test how
//! each collection helper (`$?`, `$*`, `$,`, `$<>`, `$()`, `$[]`)
//! propagates that ambiguity.

use formality_core::parse::{self, CoreParse};
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

/// Count the number of complete parses (no remaining input).
fn count_complete_parses<T: CoreParse<crate::ptt::FormalityLang>>(input: &str) -> usize {
    let scope = Default::default();
    let parses = T::parse(&scope, input).unwrap();
    parses
        .into_iter()
        .filter(|p| parse::skip_whitespace(p.text()).is_empty())
        .count()
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

    assert_eq!(count_complete_parses::<OptAmb>("opt a"), 2);
}

/// `$?amb` — optional ambiguous child with input absent.
/// Should produce exactly 1 parse: `None`.
#[test]
fn opt_ambiguous_absent() {
    #[term(opt $?amb end)]
    pub struct OptAmbEnd {
        amb: Option<Amb>,
    }

    assert_eq!(count_complete_parses::<OptAmbEnd>("opt end"), 1);
}

/// `$?amb` with a trailing required token — ambiguity from `$?` should
/// propagate through the rest of the parse.
#[test]
fn opt_ambiguous_with_suffix() {
    #[term(opt $?amb done)]
    pub struct OptAmbDone {
        amb: Option<Amb>,
    }

    assert_eq!(count_complete_parses::<OptAmbDone>("opt a done"), 2);
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

    assert_eq!(count_complete_parses::<ManyAmb>("many a end"), 2);
}

/// `$*items` where two items are each ambiguous.
/// Two ambiguous items → 2 * 2 = 4 parses.
#[test]
fn many_two_ambiguous() {
    #[term(many $*items end)]
    pub struct ManyAmb {
        items: Vec<Amb>,
    }

    assert_eq!(count_complete_parses::<ManyAmb>("many a b end"), 4);
}

/// `$*items` where three items are each ambiguous.
/// Three ambiguous items → 2^3 = 8 parses.
#[test]
fn many_three_ambiguous() {
    #[term(many $*items end)]
    pub struct ManyAmb {
        items: Vec<Amb>,
    }

    assert_eq!(count_complete_parses::<ManyAmb>("many a b c end"), 8);
}

/// `$*items` with zero items should produce exactly 1 parse (empty vec).
#[test]
fn many_zero_items() {
    #[term(many $*items end)]
    pub struct ManyAmb {
        items: Vec<Amb>,
    }

    assert_eq!(count_complete_parses::<ManyAmb>("many end"), 1);
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

    assert_eq!(count_complete_parses::<CommaAmb>("comma a end"), 2);
}

/// `$,items` — two ambiguous comma-separated items → 2 * 2 = 4 parses.
#[test]
fn comma_two_ambiguous() {
    #[term(comma $,items end)]
    pub struct CommaAmb {
        items: Vec<Amb>,
    }

    assert_eq!(count_complete_parses::<CommaAmb>("comma a, b end"), 4);
}

/// `$,items` — zero items → 1 parse.
#[test]
fn comma_zero_items() {
    #[term(comma $,items end)]
    pub struct CommaAmb {
        items: Vec<Amb>,
    }

    assert_eq!(count_complete_parses::<CommaAmb>("comma end"), 1);
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

    assert_eq!(count_complete_parses::<DelimAngle>("delim <a>"), 2);
}

/// `$<items>` — two comma-separated ambiguous items → 4 parses.
#[test]
fn delimited_angle_two_ambiguous() {
    #[term(delim $<items>)]
    pub struct DelimAngle {
        items: Vec<Amb>,
    }

    assert_eq!(count_complete_parses::<DelimAngle>("delim <a, b>"), 4);
}

/// `$<items>` — empty angle brackets → 1 parse.
#[test]
fn delimited_angle_empty() {
    #[term(delim $<items>)]
    pub struct DelimAngle {
        items: Vec<Amb>,
    }

    assert_eq!(count_complete_parses::<DelimAngle>("delim <>"), 1);
}

/// `$<?items>` is optional — missing delimiters → 1 parse (empty vec).
#[test]
fn delimited_angle_absent() {
    #[term(delim $<?items> end)]
    pub struct DelimAngle {
        items: Vec<Amb>,
    }

    assert_eq!(count_complete_parses::<DelimAngle>("delim end"), 1);
    // When present, still propagates ambiguity
    assert_eq!(count_complete_parses::<DelimAngle>("delim <a> end"), 2);
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

    assert_eq!(count_complete_parses::<DelimParen>("delim (a, b)"), 4);
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

    assert_eq!(count_complete_parses::<DelimBracket>("delim [a, b]"), 4);
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
    assert_eq!(count_complete_parses::<OptThenReq>("start a b end"), 4);
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

    assert_eq!(count_complete_parses::<OptThenReq>("start then a end"), 2);
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
    assert_eq!(
        count_complete_parses::<ManyThenReq>("start a then b end"),
        4
    );
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

    assert_eq!(count_complete_parses::<DelimThenReq>("start <a, b> c"), 8);
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
    assert_eq!(count_complete_parses::<Wrapper>("many a b end"), 4);

    // "comma a, b end" → Comma variant, 2 items, 2^2 = 4
    assert_eq!(count_complete_parses::<Wrapper>("comma a, b end"), 4);
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
    assert_eq!(count_complete_parses::<ListWrap>("list (a) end"), 4);
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

    assert_eq!(count_complete_parses::<OptUnamb>("opt a end"), 1);
    assert_eq!(count_complete_parses::<OptUnamb>("opt end"), 1);
}

#[test]
fn many_unambiguous() {
    #[term(many $*items end)]
    pub struct ManyUnamb {
        items: Vec<Unamb>,
    }

    assert_eq!(count_complete_parses::<ManyUnamb>("many a b c end"), 1);
}

#[test]
fn comma_unambiguous() {
    #[term(comma $,items end)]
    pub struct CommaUnamb {
        items: Vec<Unamb>,
    }

    assert_eq!(count_complete_parses::<CommaUnamb>("comma a, b, c end"), 1);
}

#[test]
fn delimited_unambiguous() {
    #[term(delim $<items>)]
    pub struct DelimUnamb {
        items: Vec<Unamb>,
    }

    assert_eq!(count_complete_parses::<DelimUnamb>("delim <a, b, c>"), 1);
}
