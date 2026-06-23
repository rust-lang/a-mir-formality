//! End-to-end-ish tests for the scraper + report generator. Uses inline
//! fixtures (a fake `judgment_fn!` source snippet) so the tests don't depend
//! on the live `formality-rust` source layout.

use std::collections::{BTreeMap, BTreeSet};

use expect_test::expect;
use formality_coverage::jsonl::{
    self, Coverage, CoveredRule, NoApplicableRuleLoc, PremiseLoc, TestLoc,
};
use formality_coverage::report;
use formality_coverage::scrape::{scrape_text, Judgment, Premise, PremiseKind, Rule};

fn premise(raw: &str, kind: PremiseKind, fallible: bool, line: u32) -> Premise {
    Premise {
        raw_text: raw.into(),
        kind,
        fallible,
        line,
    }
}

/// A rule with one fallible premise on `premise_line`, for report snapshots.
fn rule_with_premise(name: &str, line: u32, premise_line: u32) -> Rule {
    Rule {
        name: name.into(),
        raw_text: String::new(),
        line,
        premises: vec![premise("if true", PremiseKind::If, true, premise_line)],
    }
}

const FIXTURE: &str = r#"
use formality_core::judgment_fn;

judgment_fn! {
    /// doc comment
    pub fn prove_thing(x: u32) => () {
        debug(x)

        (
            (if x > 0)
            --------------------------------------- ("positive")
            (prove_thing(x) => ())
        )

        (
            (if x == 0)
            --- ("zero")
            (prove_thing(x) => ())
        )
    }
}

judgment_fn! {
    fn only_one(x: u32) => () {
        debug(x)

        (
            (if x == 1)
            ----- ("one")
            (only_one(x) => ())
        )
    }
}
"#;

#[test]
fn scrapes_judgments_and_rules() {
    let got = scrape_text(FIXTURE, "fixture.rs");
    assert_eq!(got.len(), 2);

    assert_eq!(got[0].name, "prove_thing");
    assert_eq!(got[0].file, "fixture.rs");
    let rule_names: Vec<&str> = got[0].rules.iter().map(|r| r.name.as_str()).collect();
    assert_eq!(rule_names, vec!["positive", "zero"]);

    assert_eq!(got[1].name, "only_one");
    let rule_names: Vec<&str> = got[1].rules.iter().map(|r| r.name.as_str()).collect();
    assert_eq!(rule_names, vec!["one"]);
}

#[test]
fn rule_line_numbers_are_absolute() {
    let got = scrape_text(FIXTURE, "fixture.rs");
    let positive = got[0].rules.iter().find(|r| r.name == "positive").unwrap();
    let expected_line = FIXTURE
        .lines()
        .position(|l| l.contains(r#"("positive")"#))
        .unwrap() as u32
        + 1;
    assert_eq!(positive.line, expected_line);
}

#[test]
fn premises_carry_absolute_lines() {
    let got = scrape_text(FIXTURE, "fixture.rs");
    let positive = got[0].rules.iter().find(|r| r.name == "positive").unwrap();
    assert_eq!(positive.premises.len(), 1);
    let p = &positive.premises[0];
    assert_eq!(p.kind, PremiseKind::If);
    assert!(p.fallible);
    let expected_line = FIXTURE
        .lines()
        .position(|l| l.contains("if x > 0"))
        .unwrap() as u32
        + 1;
    assert_eq!(p.line, expected_line);
}

#[test]
fn scrapes_premises_with_fallibility_tags() {
    const SRC: &str = r#"
judgment_fn! {
    fn mixed(x: u32) => () {
        debug(x)

        (
            (let y = x)
            (if y > 0)
            (if let Some(z) = thing)
            (sub_judgment(y) => ())
            --- ("kitchen sink")
            (mixed(x) => ())
        )

        (
            (let y = x?)
            --- ("fallible let")
            (mixed(x) => ())
        )

        (
            (let y = x)
            --- ("all infallible")
            (mixed(x) => ())
        )
    }
}
"#;
    let got = scrape_text(SRC, "src.rs");
    assert_eq!(got.len(), 1);
    let rules = &got[0].rules;
    assert_eq!(rules.len(), 3);

    let kitchen = rules.iter().find(|r| r.name == "kitchen sink").unwrap();
    let kinds: Vec<PremiseKind> = kitchen.premises.iter().map(|p| p.kind).collect();
    assert_eq!(
        kinds,
        vec![
            PremiseKind::Let,
            PremiseKind::If,
            PremiseKind::IfLet,
            PremiseKind::Judgment,
        ]
    );
    let fallibilities: Vec<bool> = kitchen.premises.iter().map(|p| p.fallible).collect();
    assert_eq!(fallibilities, vec![false, true, true, true]);
    // Premise lines are strictly increasing within the rule body.
    let lines: Vec<u32> = kitchen.premises.iter().map(|p| p.line).collect();
    assert!(lines.windows(2).all(|w| w[0] < w[1]), "lines: {lines:?}");

    let fallible_let = rules.iter().find(|r| r.name == "fallible let").unwrap();
    assert_eq!(fallible_let.premises.len(), 1);
    assert_eq!(fallible_let.premises[0].kind, PremiseKind::Let);
    assert!(
        fallible_let.premises[0].fallible,
        "`let y = x?` must be flagged fallible because of the `?`"
    );

    let all_inf = rules.iter().find(|r| r.name == "all infallible").unwrap();
    assert!(all_inf.premises.iter().all(|p| !p.fallible));
}

/// Two-rule `prove_thing` judgment: `positive` (premise on line 9, tested
/// negatively) and `zero` (premise on line 15, never tested negatively).
fn prove_thing_judgment() -> Judgment {
    Judgment {
        name: "prove_thing".into(),
        doc_comment: String::new(),
        signature: String::new(),
        file: "fixture.rs".into(),
        line: 4,
        rules: vec![
            rule_with_premise("positive", 10, 9),
            rule_with_premise("zero", 16, 15),
        ],
    }
}

/// Coverage where `prove_thing`'s `positive` premise (line 9) failed in a
/// negative test with cause `if_false`, but `zero`'s premise (line 15) never did.
fn prove_thing_coverage() -> Coverage {
    let positive_premise = PremiseLoc {
        file: "crates/sub/fixture.rs".into(),
        line: 9,
    };
    let mut negative_premises = BTreeMap::new();
    negative_premises.insert(
        positive_premise.clone(),
        BTreeSet::from(["if_false".to_string()]),
    );
    let mut negative_premise_tests = BTreeMap::new();
    negative_premise_tests.insert(
        positive_premise,
        BTreeSet::from([TestLoc {
            file: "tests/prove_thing.rs".into(),
            line: 99,
        }]),
    );
    Coverage {
        positive: BTreeMap::from([(
            CoveredRule {
                judgment: "prove_thing".into(),
                rule: "positive".into(),
            },
            BTreeSet::from([TestLoc {
                file: "tests/prove_thing.rs".into(),
                line: 42,
            }]),
        )]),
        negative_premises,
        negative_premise_tests,
        no_applicable_rule: BTreeSet::new(),
    }
}

/// Base URL used in report snapshots to exercise the test-link rendering.
const GITHUB_BASE: &str = "https://github.com/example/repo/blob/main";

#[test]
fn markdown_index_snapshot() {
    let judgments = vec![prove_thing_judgment()];
    let cov = prove_thing_coverage();
    let md = report::render_index(&judgments, &cov);
    expect![[r#"
        # Coverage report

        | Judgment/Rule | Positive coverage | Negative coverage |
        | --- | --- | --- |
        | **[prove_thing](./prove_thing.md)** | - | - |
        | ↳ [positive](./prove_thing.md#positive) | [1 test](./prove_thing__positive__pos.md) | 1/1 |
        | ↳ [zero](./prove_thing.md#zero) | ✗ | 0/1 |
    "#]]
    .assert_eq(&md);
}

#[test]
fn markdown_subpage_snapshot() {
    let j = prove_thing_judgment();
    let cov = prove_thing_coverage();
    let md = report::render_subpage(&j, &cov);
    expect![[r#"
        # Judgment `prove_thing` at fixture.rs:4

        ## Rules

        | Rule | Line | Positive coverage |
        | --- | --- | --- |
        | <a id="positive"></a>`positive` | 10 | [1 test](./prove_thing__positive__pos.md) |
        | <a id="zero"></a>`zero` | 16 | ✗ |

        ## Premises (negative coverage)

        | Rule | Premise | Line | Negatively tested |
        | --- | --- | --- | --- |
        | `positive` | `if true` | 9 | [1 test](./prove_thing__positive__p9__neg.md) (if_false) |
        | `zero` | `if true` | 15 | ✗ |
    "#]]
    .assert_eq(&md);
}

#[test]
fn detail_pages_list_tests() {
    let j = prove_thing_judgment();
    let cov = prove_thing_coverage();
    let pages = report::render_detail_pages_for(&j, &cov, Some(GITHUB_BASE));

    // One positive page for the covered `positive` rule, one negative page for
    // its negatively-tested premise. `zero` is uncovered, so it gets no page.
    let slugs: Vec<&str> = pages.iter().map(|p| p.slug.as_str()).collect();
    assert_eq!(
        slugs,
        vec![
            "prove_thing__positive__pos",
            "prove_thing__positive__p9__neg",
        ]
    );

    let pos = &pages[0];
    assert!(
        pos.content
            .contains("# Positive coverage: `prove_thing` / `positive`"),
        "{}",
        pos.content
    );
    assert!(pos.content.contains("1 test exercised this rule"));
    assert!(
        pos.content.contains(
            "- [tests/prove_thing.rs:42](https://github.com/example/repo/blob/main/tests/prove_thing.rs#L42)"
        ),
        "{}",
        pos.content
    );

    let neg = &pages[1];
    assert!(neg.content.contains("premise `if true`"), "{}", neg.content);
    assert!(
        neg.content
            .contains("Premise at line 9. Observed failure causes: if_false."),
        "{}",
        neg.content
    );
    assert!(
        neg.content.contains(
            "- [tests/prove_thing.rs:99](https://github.com/example/repo/blob/main/tests/prove_thing.rs#L99)"
        ),
        "{}",
        neg.content
    );
}

#[test]
fn detail_pages_without_github_base_use_plain_locations() {
    let j = prove_thing_judgment();
    let cov = prove_thing_coverage();
    let pages = report::render_detail_pages_for(&j, &cov, None);
    let pos = &pages[0];
    assert!(pos.content.contains("- tests/prove_thing.rs:42"));
    assert!(!pos.content.contains("https://"), "{}", pos.content);
}

#[test]
fn infallible_premise_renders_as_na() {
    let j = Judgment {
        name: "easy".into(),
        doc_comment: String::new(),
        signature: String::new(),
        file: "fixture.rs".into(),
        line: 1,
        rules: vec![Rule {
            name: "trivial".into(),
            raw_text: String::new(),
            line: 7,
            premises: vec![premise("let x = y", PremiseKind::Let, false, 6)],
        }],
    };
    let cov = Coverage::default();

    let index = report::render_index(&[j.clone()], &cov);
    expect![[r#"
        # Coverage report

        | Judgment/Rule | Positive coverage | Negative coverage |
        | --- | --- | --- |
        | **[easy](./easy.md)** | - | - |
        | ↳ [trivial](./easy.md#trivial) | ✗ | N/A |
    "#]]
    .assert_eq(&index);

    let subpage = report::render_subpage(&j, &cov);
    expect![[r#"
        # Judgment `easy` at fixture.rs:1

        ## Rules

        | Rule | Line | Positive coverage |
        | --- | --- | --- |
        | <a id="trivial"></a>`trivial` | 7 | ✗ |

        ## Premises (negative coverage)

        | Rule | Premise | Line | Negatively tested |
        | --- | --- | --- | --- |
        | `trivial` | `let x = y` | 6 | N/A |
    "#]]
    .assert_eq(&subpage);
}

#[test]
fn no_applicable_rule_renders_in_index_and_subpage() {
    let j = prove_thing_judgment();
    let mut cov = Coverage::default();
    cov.no_applicable_rule.insert(NoApplicableRuleLoc {
        judgment: "prove_thing".into(),
        file: "crates/sub/fixture.rs".into(),
        line: 4,
    });

    let index = report::render_index(&[j.clone()], &cov);
    expect![[r#"
        # Coverage report

        | Judgment/Rule | Positive coverage | Negative coverage |
        | --- | --- | --- |
        | **[prove_thing](./prove_thing.md)** | - | no applicable rule observed |
        | ↳ [positive](./prove_thing.md#positive) | ✗ | 0/1 |
        | ↳ [zero](./prove_thing.md#zero) | ✗ | 0/1 |
    "#]]
    .assert_eq(&index);

    let subpage = report::render_subpage(&j, &cov);
    assert!(subpage.contains("_No applicable rule observed:"));
}

#[test]
fn empty_rules_renders_no_rules_message() {
    let j = Judgment {
        name: "lonely".into(),
        doc_comment: String::new(),
        signature: String::new(),
        file: "fixture.rs".into(),
        line: 1,
        rules: vec![],
    };
    let cov = Coverage::default();
    let md = report::render_subpage(&j, &cov);
    expect![[r#"
        # Judgment `lonely` at fixture.rs:1

        _No rules discovered._
    "#]]
    .assert_eq(&md);
}

#[test]
fn jsonl_reads_positive_and_negative_records() {
    let tmp = std::env::temp_dir().join(format!("formality-coverage-read-{}", std::process::id()));
    std::fs::create_dir_all(&tmp).unwrap();
    let path = tmp.join("test-coverage.jsonl");

    let body = concat!(
        r#"{"kind":"positive","test_file":"a.rs","test_line":1,"test_column":1,"rules":[{"judgment":"j1","rule":"r1","file":"f.rs","line":10}]}"#,
        "\n",
        r#"{"kind":"negative","test_file":"b.rs","test_line":2,"test_column":1,"reasons":[{"reason":"premise","judgment":"j1","rule":"r1","file":"f.rs","line":11,"cause":"if_false"},{"reason":"premise","judgment":"j1","rule":"r1","file":"f.rs","line":11,"cause":"if_let"}]}"#,
        "\n",
        r#"{"kind":"negative","test_file":"c.rs","test_line":3,"test_column":1,"reasons":[{"reason":"no_applicable_rule","judgment":"j2","file":"g.rs","line":5}]}"#,
        "\n",
    );
    std::fs::write(&path, body).unwrap();

    let cov = jsonl::read(&path).unwrap();
    assert_eq!(cov.positive.len(), 1);

    // The test location of the positive record is retained so the report can
    // link the ✓ back to the test on GitHub.
    let tests = cov
        .positive_tests("j1", "r1")
        .expect("rule r1 should be covered");
    assert!(tests.contains(&TestLoc {
        file: "a.rs".into(),
        line: 1,
    }));

    let causes = cov.premise_causes_for("f.rs", 11);
    let causes: Vec<&str> = causes.iter().map(String::as_str).collect();
    assert_eq!(causes, vec!["if_false", "if_let"]);

    // The negative record's test location is retained so the report can link
    // the failing premise back to the test that exercised it.
    let neg_tests = cov.negative_premise_tests("f.rs", 11);
    assert!(neg_tests.contains(&TestLoc {
        file: "b.rs".into(),
        line: 2,
    }));
    // Different file should not match.
    assert!(cov.premise_causes_for("other.rs", 11).is_empty());
    // Different line should not match.
    assert!(cov.premise_causes_for("f.rs", 12).is_empty());

    assert!(cov.no_applicable_rule_observed("g.rs", "j2"));
    assert!(!cov.no_applicable_rule_observed("g.rs", "jX"));

    let _ = std::fs::remove_dir_all(&tmp);
}

#[test]
fn jsonl_reader_tolerates_malformed_lines() {
    let tmp = std::env::temp_dir().join(format!(
        "formality-coverage-malformed-{}",
        std::process::id()
    ));
    std::fs::create_dir_all(&tmp).unwrap();
    let path = tmp.join("test-coverage.jsonl");

    // First line is well-formed; second line has two JSON objects glued together
    // (the failure mode we saw in practice when parallel appends interleaved);
    // third line is well-formed again.
    let body = concat!(
        r#"{"kind":"positive","test_file":"a.rs","test_line":1,"test_column":1,"rules":[{"judgment":"j1","rule":"r1","file":"f","line":1}]}"#,
        "\n",
        r#"{"kind":"positive","test_file":"b.rs","test_line":2,"test_column":1,"rules":[{"judgment":"j2","rule":"r2","file":"f","line":1}]}{"#,
        "\n",
        r#"{"kind":"positive","test_file":"c.rs","test_line":3,"test_column":1,"rules":[{"judgment":"j3","rule":"r3","file":"f","line":1}]}"#,
        "\n",
    );
    std::fs::write(&path, body).unwrap();

    let got = jsonl::read_positive(&path).expect("malformed line should not fail the reader");
    let names: Vec<(&str, &str)> = got
        .iter()
        .map(|r| (r.judgment.as_str(), r.rule.as_str()))
        .collect();
    assert_eq!(names, vec![("j1", "r1"), ("j3", "r3")]);

    let _ = std::fs::remove_dir_all(&tmp);
}
