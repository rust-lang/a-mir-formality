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
            (for_all(n in &nums)
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
          expression evaluated to an empty collection: `is_even(n)`"#]]);
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
            (for_all(n in &nums) with(acc)
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
