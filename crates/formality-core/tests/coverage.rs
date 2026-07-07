//! Integration test for the coverage tracker. Uses a tiny "dummy language"
//! (just judgments over plain `Num`s — no `declare_language!` needed since
//! the coverage tracker is language-independent) to exercise the recording
//! path end-to-end without depending on `formality-rust`.

use std::path::PathBuf;
use std::sync::Mutex;

use formality_core::judgment::coverage::record_negative_coverage;
use formality_core::{cast_impl, judgment_fn, Fallible};

/// `FORMALITY_COVERAGE_DIR` is process-global, so the two coverage tests must
/// not set it concurrently. Serialize them through this lock.
static COVERAGE_ENV_LOCK: Mutex<()> = Mutex::new(());

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
    fn all_even(nums: Vec<Num>) => () {
        debug(nums)

        (
            (for_all(n in nums) (if is_even(n).is_ok()))
            --------------------------------------- ("all_even")
            (all_even(nums) => ())
        )
    }
}

judgment_fn! {
    fn is_zero(n: Num) => () {
        debug(n)

        (
            (if n.0 == 0)
            --------------------------------------- ("is_zero")
            (is_zero(n) => ())
        )
    }
}

judgment_fn! {
    // The conclusion only pattern-matches `Num(1)`, so `is_one(Num(2))`
    // fails with no applicable rule (rather than a failed premise).
    fn is_one(n: Num) => () {
        debug(n)

        (
            --------------------------------------- ("is_one")
            (is_one(Num(1)) => ())
        )
    }
}

#[test]
fn coverage_records_rules_from_positive_tests() {
    let _guard = COVERAGE_ENV_LOCK.lock().unwrap();
    let tmp = std::env::temp_dir().join(format!("formality-coverage-test-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&tmp);
    std::env::set_var("FORMALITY_COVERAGE_DIR", &tmp);

    let all_even_line = line!() + 1;
    all_even(vec![Num(2), Num(4)]).assert_ok(expect_test::expect!["{()}"]);
    let is_zero_line = line!() + 1;
    is_zero(Num(0)).assert_ok(expect_test::expect!["{()}"]);

    let file: PathBuf = tmp.join("test-coverage.jsonl");
    let contents = std::fs::read_to_string(&file).expect("coverage file written");

    assert!(
        contents.contains("tests/coverage.rs"),
        "missing test file in {contents}",
    );
    // The proof tree carries the judgment's arguments and result as attributes.
    assert!(
        contents.contains(r#""attributes":[["nums","[Num(2), Num(4)]"],["result","()"]]"#),
        "missing argument/result attributes in {contents}",
    );
    assert!(
        contents.contains(&format!("\"test_line\":{all_even_line}")),
        "missing all_even call line {all_even_line} in {contents}",
    );
    assert!(
        contents.contains(&format!("\"test_line\":{is_zero_line}")),
        "missing is_zero call line {is_zero_line} in {contents}",
    );
    assert!(
        contents.contains("\"judgment\":\"all_even\""),
        "missing all_even judgment in {contents}",
    );
    assert!(
        contents.contains("\"rule\":\"all_even\""),
        "missing all_even rule in {contents}",
    );
    assert!(
        contents.contains("\"judgment\":\"is_zero\""),
        "missing is_zero judgment in {contents}",
    );
    assert!(
        contents.contains("\"rule\":\"is_zero\""),
        "missing is_zero rule in {contents}",
    );

    // Each positive `assert_ok` should produce exactly one JSONL line.
    let line_count = contents.lines().filter(|l| !l.is_empty()).count();
    assert_eq!(line_count, 2, "expected 2 JSONL lines, got:\n{contents}");

    let _ = std::fs::remove_dir_all(&tmp);
}

#[test]
fn negative_coverage_records_failure_reasons() {
    let _guard = COVERAGE_ENV_LOCK.lock().unwrap();
    let tmp = std::env::temp_dir().join(format!("formality-coverage-neg-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&tmp);
    std::env::set_var("FORMALITY_COVERAGE_DIR", &tmp);

    // A failed premise: `is_zero(Num(1))` matches the rule but its
    // `(if n.0 == 0)` premise evaluates to false.
    let err = is_zero(Num(1))
        .into_map()
        .expect_err("is_zero(1) should fail");
    record_negative_coverage(std::iter::once(err.as_ref()));

    // No applicable rule: `is_one`'s only conclusion pattern is `Num(1)`, so
    // `Num(2)` matches nothing.
    let err = is_one(Num(2))
        .into_map()
        .expect_err("is_one(2) should fail");
    record_negative_coverage(std::iter::once(err.as_ref()));

    let file: PathBuf = tmp.join("test-coverage.jsonl");
    let contents = std::fs::read_to_string(&file).expect("coverage file written");

    assert!(
        contents.contains("\"kind\":\"negative\""),
        "missing negative kind in {contents}",
    );
    // The failed tree carries the arguments the judgment was applied to.
    assert!(
        contents.contains(r#""args":"{ n: Num(1) }""#),
        "missing failing-judgment args in {contents}",
    );
    // Premise-failure reason for the `is_zero` rule.
    assert!(
        contents.contains("\"reason\":\"premise\""),
        "missing premise reason in {contents}",
    );
    assert!(
        contents.contains("\"rule\":\"is_zero\""),
        "missing is_zero rule in {contents}",
    );
    assert!(
        contents.contains("\"cause\":\"if_false\""),
        "missing if_false cause in {contents}",
    );
    // No-applicable-rule reason for `is_one(Num(2))`.
    assert!(
        contents.contains("\"reason\":\"no_applicable_rule\""),
        "missing no_applicable_rule reason in {contents}",
    );
    assert!(
        contents.contains("\"judgment\":\"is_one\""),
        "missing is_one judgment in {contents}",
    );

    let line_count = contents.lines().filter(|l| !l.is_empty()).count();
    assert_eq!(line_count, 2, "expected 2 JSONL lines, got:\n{contents}");

    let _ = std::fs::remove_dir_all(&tmp);
}
