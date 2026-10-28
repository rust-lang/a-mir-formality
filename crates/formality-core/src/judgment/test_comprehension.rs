#![cfg(test)]

use crate::{cast_impl, judgment_fn, Fallible, Map, Set, UpcastFrom};
use formality_macros::test;

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Debug, Hash)]
struct Num(u32);

cast_impl!(Num);

impl UpcastFrom<u32> for Num {
    fn upcast_from(value: u32) -> Self {
        Num(value)
    }
}

judgment_fn! {
    fn as_sequence(values: Vec<u32>) => Vec<Num> {
        debug(values)

        (
            (let result: Vec<Num> = for_all value in values => value)
            --- ("sequence")
            (as_sequence(values) => result)
        )
    }
}

judgment_fn! {
    fn as_set(values: Vec<u32>) => Set<Num> {
        debug(values)

        (
            (let result: Set<Num> = for_all value in values => value)
            --- ("set")
            (as_set(values) => result)
        )
    }
}

#[test]
fn sequence_preserves_order_and_duplicates_while_set_deduplicates() {
    let values = vec![3u32, 1, 3, 2];
    as_sequence(&values).assert_ok(expect_test::expect!["{[Num(3), Num(1), Num(3), Num(2)]}"]);
    as_set(&values).assert_ok(expect_test::expect!["{{Num(1), Num(2), Num(3)}}"]);
}

#[test]
fn empty_collection_succeeds() {
    as_sequence(Vec::<u32>::new()).assert_ok(expect_test::expect!["{[]}"]);
    as_set(Vec::<u32>::new()).assert_ok(expect_test::expect!["{{}}"]);
}

judgment_fn! {
    fn as_map(values: Vec<(u32, u32)>) => Vec<(Num, Num)> {
        debug(values)

        (
            (let result: Map<Num, Num> = for_all pair in values => (pair.0, pair.1))
            // Maps do not have an identity upcast, so expose their entries.
            (let entries: Vec<_> = result.iter().map(|(k, v)| (k.clone(), v.clone())).collect())
            --- ("map")
            (as_map(values) => entries)
        )
    }
}

#[test]
fn map_upcasts_both_components_and_replaces_duplicate_keys() {
    as_map(vec![(2u32, 20u32), (1, 10), (2, 21)]).assert_ok(expect_test::expect![
        "{[(Num(1), Num(10)), (Num(2), Num(21))]}"
    ]);
    as_map(Vec::<(u32, u32)>::new()).assert_ok(expect_test::expect!["{[]}"]);
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Debug, Hash)]
struct Named {
    name: String,
}

cast_impl!(Named);

judgment_fn! {
    fn names(values: Vec<Named>) => Vec<String> {
        debug(values)

        (
            (let result: Vec<String> = for_all value in values => value.name)
            --- ("names")
            (names(values) => result)
        )
    }
}

#[test]
fn field_projection_implicitly_borrows_and_clones() {
    names(vec![
        Named {
            name: "first".into(),
        },
        Named {
            name: "second".into(),
        },
    ])
    .assert_ok(expect_test::expect![[r#"{["first", "second"]}"#]]);
}

judgment_fn! {
    fn inferred_collection(values: Vec<u32>) => Vec<Num> {
        debug(values)

        (
            (let result = for_all value in values => value)
            (let result: &Vec<Num> = result)
            --- ("inferred")
            (inferred_collection(values) => result)
        )
    }
}

#[test]
fn later_use_infers_collection_and_element_types() {
    inferred_collection(vec![2u32, 1]).assert_ok(expect_test::expect!["{[Num(2), Num(1)]}"]);
}

judgment_fn! {
    fn half_even(n: u32) => u32 {
        debug(n)

        (
            (if n % 2 == 0)
            --- ("even")
            (half_even(n) => n / 2)
        )
    }
}

judgment_fn! {
    fn halves(values: Vec<u32>) => Vec<u32> {
        debug(values)

        (
            (let result: Vec<u32> = for_all value in values =>
                (half_even(value) => half)
                (if *half > 0)
                half
            )
            --- ("halves")
            (halves(values) => result)
        )
    }
}

#[test]
fn premises_preserve_each_successful_subproof() {
    let result = halves(vec![2u32, 4, 6]);
    result.assert_ok(expect_test::expect!["{[1, 2, 3]}"]);
    let (_, tree) = result.into_singleton().unwrap();

    fn collect_inputs(tree: &crate::judgment::ProofTree, inputs: &mut Vec<String>) {
        if tree.judgment_name == "half_even" {
            inputs.extend(
                tree.attributes
                    .iter()
                    .filter(|(key, _)| key == "n")
                    .map(|(_, value)| value.clone()),
            );
        }
        for child in &tree.children {
            collect_inputs(child, inputs);
        }
    }

    let mut inputs = vec![];
    collect_inputs(&tree, &mut inputs);
    assert_eq!(inputs, ["2", "4", "6"]);
}

#[test]
fn failed_premises_fail_instead_of_filtering() {
    halves(vec![2u32, 3, 4]).assert_err(expect_test::expect![[r#"
        the rule "even" at (test_comprehension.rs) failed because
          condition evaluated to false: `n % 2 == 0`"#]]);
    halves(vec![2u32, 0, 4]).assert_err(expect_test::expect![[r#"
        the rule "halves" at (test_comprehension.rs) failed because
          condition evaluated to false: `*half > 0`"#]]);
    halves(Vec::<u32>::new()).assert_ok(expect_test::expect!["{[]}"]);
}

judgment_fn! {
    fn terminal_judgment(values: Vec<u32>) => Vec<Num> {
        debug(values)

        (
            (let result: Vec<Num> = for_all value in values =>
                (half_even(value) => half)
            )
            --- ("terminal judgment")
            (terminal_judgment(values) => result)
        )
    }
}

#[test]
fn terminal_judgment_implicitly_upcasts_its_result_and_propagates_failure() {
    terminal_judgment(vec![2u32, 4]).assert_ok(expect_test::expect!["{[Num(1), Num(2)]}"]);
    terminal_judgment(vec![2u32, 3]).assert_err(expect_test::expect![[r#"
        the rule "even" at (test_comprehension.rs) failed because
          condition evaluated to false: `n % 2 == 0`"#]]);
}

judgment_fn! {
    fn computation_then_judgment(values: Vec<u32>) => Vec<Num> {
        debug(values)

        (
            (let result: Vec<Num> = for_all value in values =>
                (let shifted = value + 1)
                (half_even(shifted) => half)
            )
            --- ("computation then judgment")
            (computation_then_judgment(values) => result)
        )
    }
}

#[test]
fn terminal_judgment_can_use_preceding_bindings() {
    computation_then_judgment(vec![1u32, 3]).assert_ok(expect_test::expect!["{[Num(1), Num(2)]}"]);
    computation_then_judgment(vec![1u32, 2]).assert_err(expect_test::expect![[r#"
        the rule "even" at (test_comprehension.rs) failed because
          condition evaluated to false: `n % 2 == 0`"#]]);
}

judgment_fn! {
    fn with_half(value: u32) => (u32, u32) {
        debug(value)

        (
            (half_even(value) => half)
            --- ("with half")
            (with_half(value) => (value, half))
        )
    }
}

judgment_fn! {
    fn destructured_premise(values: Vec<u32>) => Vec<Num> {
        debug(values)

        (
            (let result: Vec<Num> = for_all value in values =>
                (with_half(value) => (original, half))
                original - half
            )
            --- ("destructured premise")
            (destructured_premise(values) => result)
        )
    }
}

#[test]
fn destructured_judgment_result_can_feed_an_explicit_final_expression() {
    destructured_premise(vec![2u32, 4]).assert_ok(expect_test::expect!["{[Num(1), Num(2)]}"]);
}

judgment_fn! {
    fn collection_and_body_expressions(values: Vec<u32>) => (Vec<Num>, Vec<Num>) {
        debug(values)

        (
            (let reversed: Vec<Num> = for_all value in values.iter().rev() => {
                let doubled = value * 2;
                doubled + 1
            })
            (let skipped: Vec<Num> = for_all value in { values.iter().skip(1) } => value)
            --- ("collection and body expressions")
            (collection_and_body_expressions(values) => (reversed, skipped))
        )
    }
}

#[test]
fn collection_and_final_value_accept_compound_expressions() {
    collection_and_body_expressions(vec![1u32, 2, 3]).assert_ok(expect_test::expect![
        "{([Num(7), Num(5), Num(3)], [Num(2), Num(3)])}"
    ]);
}

fn checked_half(value: &u32) -> Fallible<u32> {
    anyhow::ensure!(value.is_multiple_of(2), "odd input: {value}");
    Ok(value / 2)
}

judgment_fn! {
    fn fallible_yield(values: Vec<u32>) => Vec<u32> {
        debug(values)

        (
            (let result: Vec<u32> = for_all value in values => checked_half(value)?)
            --- ("fallible yield")
            (fallible_yield(values) => result)
        )
    }
}

#[test]
fn question_mark_in_yield_propagates_failure() {
    fallible_yield(vec![2u32, 4]).assert_ok(expect_test::expect!["{[1, 2]}"]);
    fallible_yield(vec![2u32, 3]).assert_err(expect_test::expect![[r#"
        the rule "fallible yield" at (test_comprehension.rs) failed because
          odd input: 3"#]]);
}

judgment_fn! {
    fn wrapped_tail_expressions(values: Vec<u32>) => (Vec<Num>, Vec<Num>) {
        debug(values)

        (
            (let parenthesized: Vec<Num> = for_all value in values =>
                ((checked_half(value)?) * 3)
            )
            (let blocked: Vec<Num> = for_all value in values => {
                (checked_half(value)?) * 3
            })
            --- ("wrapped tails")
            (wrapped_tail_expressions(values) => (parenthesized, blocked))
        )
    }
}

#[test]
fn parentheses_or_a_block_can_wrap_a_tail_that_starts_with_parentheses() {
    wrapped_tail_expressions(vec![2u32, 4]).assert_ok(expect_test::expect![
        "{([Num(3), Num(6)], [Num(3), Num(6)])}"
    ]);
}

judgment_fn! {
    fn tuple_after_premises(values: Vec<u32>) => Vec<(Num, Num)> {
        debug(values)

        (
            (let result: Vec<(Num, Num)> = for_all value in values =>
                (half_even(value) => half)
                (value, half)
            )
            --- ("tuple tail")
            (tuple_after_premises(values) => result)
        )
    }
}

#[test]
fn tuple_tail_after_premises_needs_only_its_own_parentheses() {
    tuple_after_premises(vec![2u32, 4]).assert_ok(expect_test::expect![
        "{[(Num(2), Num(1)), (Num(4), Num(2))]}"
    ]);
}

judgment_fn! {
    fn conditional_tail(values: Vec<u32>) => Vec<Num> {
        debug(values)

        (
            (let result: Vec<Num> = for_all value in values =>
                (let offset = value + 1)
                if value.is_multiple_of(2) { offset * 2 } else { offset * 3 }
            )
            --- ("conditional tail")
            (conditional_tail(values) => result)
        )
    }
}

#[test]
fn conditional_tail_is_a_rust_expression() {
    conditional_tail(vec![1u32, 2, 3])
        .assert_ok(expect_test::expect!["{[Num(6), Num(6), Num(12)]}"]);
}

judgment_fn! {
    fn unit_and_negated_tails(values: Vec<u32>) => (Vec<()>, Vec<bool>) {
        debug(values)

        (
            (let units: Vec<()> = for_all value in values =>
                (if *value < 3)
                ()
            )
            (let negated: Vec<bool> = for_all value in values =>
                (let flag = *value > 0)
                (!flag)
            )
            --- ("unit and negated tails")
            (unit_and_negated_tails(values) => (units, negated))
        )
    }
}

#[test]
fn unit_and_wrapped_negation_are_valid_final_expressions() {
    unit_and_negated_tails(vec![0u32, 1, 2]).assert_ok(expect_test::expect![
        "{([(), (), ()], [true, false, false])}"
    ]);
}

judgment_fn! {
    fn committed_premise(values: Vec<u32>) => Vec<Num> {
        debug(values)

        (
            (let result: Vec<Num> = for_all value in values =>
                (if *value > 0)!
                (half_even(value) => half)
                half * 2
            )
            --- ("committed premise")
            (committed_premise(values) => result)
        )
    }
}

#[test]
fn premise_commit_marker_is_preserved_before_a_tail() {
    committed_premise(vec![2u32, 4]).assert_ok(expect_test::expect!["{[Num(2), Num(4)]}"]);
    committed_premise(vec![2u32, 3]).assert_err(expect_test::expect![[r#"
        the rule "even" at (test_comprehension.rs) failed because
          condition evaluated to false: `n % 2 == 0`"#]]);
}

judgment_fn! {
    fn nested(collected: Vec<Vec<u32>>) => Vec<Vec<Num>> {
        debug(collected)

        (
            (let result: Vec<Vec<Num>> = for_all row in collected =>
                (let collected: Vec<Num> = for_all value in row => value)
                collected
            )
            --- ("nested")
            (nested(collected) => result)
        )
    }
}

#[test]
fn nested_comprehensions_have_independent_accumulators() {
    nested(vec![vec![1u32, 2], vec![], vec![3]])
        .assert_ok(expect_test::expect!["{[[Num(1), Num(2)], [], [Num(3)]]}"]);
}

judgment_fn! {
    fn alternatives(value: u32) => u32 {
        debug(value)

        (
            --- ("original")
            (alternatives(value) => value)
        )

        (
            --- ("offset")
            (alternatives(value) => value + 10)
        )
    }
}

judgment_fn! {
    fn collect_alternatives(values: Vec<u32>) => Vec<u32> {
        debug(values)

        (
            (let result: Vec<u32> = for_all value in values =>
                (alternatives(value) => choice)
            )
            --- ("comprehension")
            (collect_alternatives(values) => result)
        )
    }
}

judgment_fn! {
    fn accumulate_alternatives(values: Vec<u32>) => Vec<u32> {
        debug(values)

        (
            (let result: Vec<u32> = vec![])
            (for_all(value in values) with(result)
                (alternatives(value) => choice)
                (let result: Vec<u32> = crate::seq![..result.iter().copied(), *choice]))
            --- ("accumulator")
            (accumulate_alternatives(values) => result)
        )
    }
}

#[test]
fn multiple_derivations_preserve_all_combinations() {
    let values = vec![1u32, 2];
    let collected: Vec<_> = collect_alternatives(&values)
        .into_map()
        .unwrap()
        .into_keys()
        .collect();
    let accumulated: Vec<_> = accumulate_alternatives(&values)
        .into_map()
        .unwrap()
        .into_keys()
        .collect();

    // For input 1, `alternatives` returns 1 or 11; for input 2, it returns
    // 2 or 12. Each result picks one alternative per input, in input order.
    // That gives four possible vectors. [1, 11] and [2, 12] are not results:
    // each picks two alternatives for one input and none for the other.
    // The outer list is sorted by the result map; order *within* each vector
    // matters. The comprehension and explicit accumulator should agree.
    assert_eq!(
        collected,
        vec![vec![1, 2], vec![1, 12], vec![11, 2], vec![11, 12]]
    );
    assert_eq!(collected, accumulated);
}

fn for_all() -> u32 {
    7
}

judgment_fn! {
    fn ordinary_arrays() => Vec<u32> {
        debug()

        (
            (let function_calls = [for_all(), 8])
            (let for_all = 9u32)
            (let identifiers = [for_all, &10])
            (let repeated: [u32; 2] = [*for_all; 2])
            (let empty: [u32; 0] = [])
            (let result: Vec<u32> = function_calls.iter().copied()
                .chain(identifiers.iter().copied().copied())
                .chain(repeated.iter().copied())
                .chain(empty.iter().copied())
                .collect())
            --- ("arrays")
            (ordinary_arrays() => result)
        )
    }
}

#[test]
fn ordinary_array_expressions_still_work() {
    ordinary_arrays().assert_ok(expect_test::expect!["{[7, 8, 9, 10, 9, 9]}"]);
}
