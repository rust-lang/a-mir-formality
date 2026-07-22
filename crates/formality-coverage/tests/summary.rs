//! Tests for the compact coverage summary and the baseline diff.

use formality_coverage::diff;
use formality_coverage::jsonl::{Coverage, CoveredRule, TestLoc};
use formality_coverage::scrape::{Judgment, Premise, PremiseKind, Rule};
use formality_coverage::summary;

fn rule(name: &str) -> Rule {
    Rule {
        name: name.into(),
        raw_text: format!("(if true)\n----- (\"{name}\")\n(prove(x) => ())"),
        line: 10,
        premises: vec![Premise {
            raw_text: "if true".into(),
            kind: PremiseKind::If,
            fallible: true,
            line: 9,
        }],
    }
}

fn judgment(rules: Vec<Rule>) -> Judgment {
    Judgment {
        name: "prove".into(),
        doc_comment: String::new(),
        signature: "prove(x) => ()".into(),
        file: "crates/x/src/lib.rs".into(),
        line: 1,
        rules,
    }
}

fn cover_positive(cov: &mut Coverage, rule: &str) {
    cov.positive
        .entry(CoveredRule {
            judgment: "prove".into(),
            rule: rule.into(),
        })
        .or_default()
        .insert(TestLoc {
            file: "tests/t.rs".into(),
            line: 5,
        });
}

#[test]
fn build_counts_positive_coverage() {
    let judgments = vec![judgment(vec![rule("r_covered"), rule("r_uncovered")])];
    let mut cov = Coverage::default();
    cover_positive(&mut cov, "r_covered");

    let s = summary::build(&judgments, &cov);

    assert_eq!(s.totals.rules, 2);
    assert_eq!(s.totals.positive_covered, 1);
    assert_eq!(s.totals.premises_fallible, 2);
    assert_eq!(s.totals.premises_covered, 0);

    let rules = &s.judgments["prove"].rules;
    assert!(rules["r_covered"].positive_covered);
    assert!(!rules["r_uncovered"].positive_covered);
}

#[test]
fn duplicate_named_judgments_do_not_inflate_totals() {
    // The CLI's `scrape_dir` keeps both of two same-named judgments; the map
    // collapses them (first wins). Totals must reflect the collapsed map, so a
    // summary built from `scrape_dir` matches one built from the preprocessor's
    // name-keyed index instead of double-counting the duplicate's rules.
    let mut first = judgment(vec![rule("only")]);
    first.line = 1;
    let mut dup = judgment(vec![rule("only"), rule("extra")]);
    dup.line = 2;

    let s = summary::build(&[first, dup], &Coverage::default());

    assert_eq!(s.judgments.len(), 1);
    assert_eq!(s.totals.rules, 1);
    assert!(s.judgments["prove"].rules.contains_key("only"));
    assert!(!s.judgments["prove"].rules.contains_key("extra"));
}

#[test]
fn build_is_byte_stable() {
    let judgments = vec![judgment(vec![rule("b"), rule("a")])];
    let cov = Coverage::default();

    let a = serde_json::to_string(&summary::build(&judgments, &cov)).unwrap();
    let b = serde_json::to_string(&summary::build(&judgments, &cov)).unwrap();
    assert_eq!(a, b);
    // Ordered output: rule "a" sorts before "b" regardless of source order.
    assert!(a.find("\"a\"").unwrap() < a.find("\"b\"").unwrap());
}

#[test]
fn diff_flags_regression_and_new_uncovered() {
    // Baseline: r_keep covered, r_drop covered.
    let base_judgments = vec![judgment(vec![rule("r_keep"), rule("r_drop")])];
    let mut base_cov = Coverage::default();
    cover_positive(&mut base_cov, "r_keep");
    cover_positive(&mut base_cov, "r_drop");
    let base = summary::build(&base_judgments, &base_cov);

    // Head: r_drop lost its test; r_new added with no test.
    let head_judgments = vec![judgment(vec![
        rule("r_keep"),
        rule("r_drop"),
        rule("r_new"),
    ])];
    let mut head_cov = Coverage::default();
    cover_positive(&mut head_cov, "r_keep");
    let head = summary::build(&head_judgments, &head_cov);

    let report = diff::diff(&base, &head);

    assert_eq!(report.regressed, vec![("prove".into(), "r_drop".into())]);
    assert_eq!(report.new_uncovered, vec![("prove".into(), "r_new".into())]);
    assert!(report.newly_covered.is_empty());
    assert!(!report.is_clean());

    let md = report.to_markdown();
    assert!(md.contains("Rules that lost coverage"));
    assert!(md.contains("`prove` / `r_drop`"));
    assert!(md.contains("`prove` / `r_new`"));
}

#[test]
fn diff_clean_when_nothing_regresses() {
    let judgments = vec![judgment(vec![rule("r")])];
    let mut cov = Coverage::default();
    cover_positive(&mut cov, "r");
    let s = summary::build(&judgments, &cov);

    let report = diff::diff(&s, &s);
    assert!(report.is_clean());
    assert!(report.to_markdown().contains("No rules lost coverage"));
}
