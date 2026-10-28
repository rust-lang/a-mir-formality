#![cfg(test)]

use crate::{cast_impl, judgment_fn, Fallible};
use formality_macros::test;

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Debug, Hash)]
struct Num(u32);

cast_impl!(Num);

fn is_even(n: &Num) -> Fallible<()> {
    if n.0 % 2 == 0 {
        Ok(())
    } else {
        Err(anyhow::anyhow!("{} is not even", n.0))
    }
}

judgment_fn! {
    fn all_even(
        nums: Vec<Num>,
    ) => () {
        debug(nums)

        (
            (for_all(n in nums)
                (if is_even(n).is_ok()))
            --------------------------------------- ("all_even")
            (all_even(nums) => ())
        )
    }
}

#[test]
fn test_for_all_success() {
    let nums = vec![Num(2), Num(4), Num(6)];
    all_even(nums).assert_ok(expect_test::expect!["{()}"]);
}

#[test]
fn test_for_all_failure() {
    let nums = vec![Num(2), Num(3), Num(6)];
    all_even(nums).assert_err(expect_test::expect![[r#"
        the rule "all_even" at (test_for_all.rs) failed because
          condition evaluated to false: `is_even(n).is_ok()`"#]]);
}

#[test]
fn test_for_all_empty() {
    let nums: Vec<Num> = vec![];
    all_even(nums).assert_ok(expect_test::expect!["{()}"]);
}

// Test for_all with accumulator
judgment_fn! {
    fn sum_all(
        nums: Vec<Num>,
    ) => Num {
        debug(nums)

        (
            (let acc: Num = Num(0))
            (for_all(n in nums) with(acc)
                (let acc: Num = Num(acc.0 + n.0)))
            --------------------------------------- ("sum")
            (sum_all(nums) => acc)
        )
    }
}

#[test]
fn test_for_all_with_accumulator() {
    let nums = vec![Num(1), Num(2), Num(3)];
    sum_all(nums).assert_ok(expect_test::expect!["{Num(6)}"]);
}

#[test]
fn test_for_all_with_accumulator_empty() {
    let nums: Vec<Num> = vec![];
    sum_all(nums).assert_ok(expect_test::expect!["{Num(0)}"]);
}

// A standalone search problem: step 0 can produce either 1 or 2, but only
// state 1 can complete step 1. The loop must retain both intermediate states.
judgment_fn! {
    fn advance(step: Num, state: Num) => Num {
        debug(step, state)

        (
            --- ("first alternative")
            (advance(Num(0), Num(0)) => Num(1))
        )

        (
            --- ("second alternative")
            (advance(Num(0), Num(0)) => Num(2))
        )

        (
            --- ("only the first alternative continues")
            (advance(Num(1), Num(1)) => Num(3))
        )
    }
}

judgment_fn! {
    fn advance_with_for_all(steps: Vec<Num>) => Num {
        debug(steps)

        (
            (let state = Num(0))
            (for_all(step in steps) with(state)
                (advance(step, state) => state))
            --- ("advance with accumulator")
            (advance_with_for_all(steps) => state)
        )
    }
}

judgment_fn! {
    fn advance_with_two_premises() => Num {
        debug()

        (
            (advance(Num(0), Num(0)) => state)
            (advance(Num(1), state) => state)
            --- ("advance with ordinary premises")
            (advance_with_two_premises() => state)
        )
    }
}

#[test]
fn test_ordinary_premises_preserve_alternatives() {
    advance(Num(0), Num(0)).assert_ok(expect_test::expect!["{Num(1), Num(2)}"]);
    advance_with_two_premises().assert_ok(expect_test::expect!["{Num(3)}"]);
}

#[test]
fn test_for_all_with_accumulator_preserves_alternatives() {
    advance_with_for_all(vec![Num(0), Num(1)]).assert_ok(expect_test::expect!["{Num(3)}"]);
}

#[test]
fn test_for_all_with_accumulator_returns_all_alternatives() {
    advance_with_for_all(vec![Num(0)]).assert_ok(expect_test::expect!["{Num(1), Num(2)}"]);
}

thread_local! {
    static CONTINUATIONS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

fn count_continuation() {
    CONTINUATIONS.with(|n| n.set(n.get() + 1));
}

judgment_fn! {
    fn check_choices(steps: Vec<Num>) => () {
        debug(steps)
        (
            (for_all(step in steps)
                (advance(step, Num(0)) => _choice))
            (let () = count_continuation())
            --- ("check choices")
            (check_choices(steps) => ())
        )
    }
}

#[test]
fn test_for_all_merges_equivalent_states() {
    CONTINUATIONS.with(|n| n.set(0));
    // Two choices at each of eight iterations must not run the identical
    // continuation 256 times: no values are carried out of the loop body.
    check_choices(vec![Num(0); 8]).assert_ok(expect_test::expect!["{()}"]);
    assert_eq!(CONTINUATIONS.with(|n| n.get()), 1);
}

judgment_fn! {
    fn filter_after_loop() => Num {
        debug()
        (
            (let state = Num(0))
            (for_all(step in vec![Num(0)]) with(state)
                (advance(step, state) => state))
            (if state == &Num(1))
            --- ("filter after loop")
            (filter_after_loop() => state)
        )
    }
}

#[test]
fn test_for_all_continuation_filters_alternatives_and_preserves_proof() {
    filter_after_loop().assert_ok(expect_test::expect!["{Num(1)}"]);
    let proofs = filter_after_loop().into_map().unwrap();
    let proof = format!("{:?}", proofs.get(&Num(1)).unwrap());
    assert!(proof.contains("first alternative"));
    assert!(!proof.contains("second alternative"));
}
