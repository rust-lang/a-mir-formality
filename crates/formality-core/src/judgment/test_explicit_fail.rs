#![cfg(test)]

//! Tests for the `(fail "message")` form in `judgment_fn` rules.
//! This form explicitly fails a rule with a descriptive message,
//! documenting intentionally unsupported cases and producing clear errors.

use crate::cast_impl;
use crate::judgment_fn;

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Debug, Hash)]
enum Place {
    Local(String),
    Deref(Box<Place>),
}

cast_impl!(Place);

judgment_fn!(
    fn check_place_move(place: Place) => () {
        debug(place)

        (
            (if let Place::Local(_) = place)!
            --------------------------------------- ("local-ok")
            (check_place_move(place) => ())
        )

        (
            (if let Place::Deref(_) = place)!
            (fail "cannot move out of `{:?}`, as it is behind a reference", place)
            --------------------------------------- ("deref-fail")
            (check_place_move(place) => ())
        )
    }
);

#[test]
fn local_can_be_moved() {
    check_place_move(Place::Local("x".into())).assert_ok(expect_test::expect!["{()}"]);
}

#[test]
fn deref_explicit_failure_message() {
    check_place_move(Place::Deref(Box::new(Place::Local("x".into())))).assert_err(
        expect_test::expect![[r#"
            the rule "deref-fail" at (test_explicit_fail.rs) failed because
              cannot move out of `Deref(Local("x"))`, as it is behind a reference"#]],
    );
}
