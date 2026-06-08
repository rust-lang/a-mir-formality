//! End-to-end-ish tests for the scraper + report generator. Uses inline
//! fixtures (a fake `judgment_fn!` source snippet) so the tests don't depend
//! on the live `formality-rust` source layout.

use std::collections::{BTreeMap, BTreeSet};

use expect_test::expect;
use formality_coverage::jsonl::{self, BlamedRuleLoc, Coverage, CoveredRule, ImplicitNoMatchLoc};
use formality_coverage::report;
use formality_coverage::scrape::{scrape_text, Judgment, Premise, PremiseKind, Rule};

/// Build a synthetic `Rule` with a single fallible premise so report
/// snapshots stay focused on coverage data flow rather than the
/// premise-fallibility heuristic.
fn fallible_rule(name: &str, line: u32) -> Rule {
    Rule {
        name: name.into(),
        raw_text: String::new(),
        line,
        premises: vec![Premise {
            raw_text: "if true".into(),
            kind: PremiseKind::If,
            fallible: true,
        }],
    }
}

fn cov_with_positive(rules: &[(&str, &str)]) -> Coverage {
    let mut positive = BTreeSet::new();
    for (j, r) in rules {
        positive.insert(CoveredRule {
            judgment: (*j).into(),
            rule: (*r).into(),
        });
    }
    Coverage {
        positive,
        negative: BTreeMap::new(),
        implicit_no_match: BTreeSet::new(),
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
fn markdown_index_snapshot() {
    let judgments = vec![
        Judgment {
            name: "prove_thing".into(),
            doc_comment: String::new(),
            signature: String::new(),
            file: "fixture.rs".into(),
            line: 4,
            rules: vec![fallible_rule("positive", 10), fallible_rule("zero", 16)],
        },
        Judgment {
            name: "only_one".into(),
            doc_comment: String::new(),
            signature: String::new(),
            file: "fixture.rs".into(),
            line: 22,
            rules: vec![fallible_rule("one", 28)],
        },
    ];

    let cov = cov_with_positive(&[("prove_thing", "positive")]);

    let md = report::render_index(&judgments, &cov);
    expect![[r#"
        # Coverage report

        | Judgment/Rule | Positive coverage | Negative coverage |
        | --- | --- | --- |
        | **[prove_thing](./prove_thing.md)** | - | - |
        | ↳ [positive](./prove_thing.md#positive) | ✓ | ✗ |
        | ↳ [zero](./prove_thing.md#zero) | ✗ | ✗ |
        | **[only_one](./only_one.md)** | - | - |
        | ↳ [one](./only_one.md#one) | ✗ | ✗ |
    "#]]
    .assert_eq(&md);
}

#[test]
fn markdown_subpage_snapshot() {
    let j = Judgment {
        name: "prove_thing".into(),
        doc_comment: String::new(),
        signature: String::new(),
        file: "fixture.rs".into(),
        line: 4,
        rules: vec![fallible_rule("positive", 10), fallible_rule("zero", 16)],
    };
    let cov = cov_with_positive(&[("prove_thing", "positive")]);

    let md = report::render_subpage(&j, &cov);
    expect![[r#"
        # Judgment `prove_thing` at fixture.rs:4

        | Rule | Line | Positive coverage | Negative coverage |
        | --- | --- | --- | --- |
        | <a id="positive"></a>`positive` | 10 | ✓ | ✗ |
        | <a id="zero"></a>`zero` | 16 | ✗ | ✗ |
    "#]]
    .assert_eq(&md);
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
        r#"{"test_file":"a.rs","test_line":1,"test_column":1,"rules":[{"judgment":"j1","rule":"r1","file":"f","line":1}]}"#,
        "\n",
        r#"{"test_file":"b.rs","test_line":2,"test_column":1,"rules":[{"judgment":"j2","rule":"r2","file":"f","line":1}]}{"#,
        "\n",
        r#"{"test_file":"c.rs","test_line":3,"test_column":1,"rules":[{"judgment":"j3","rule":"r3","file":"f","line":1}]}"#,
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

#[test]
fn jsonl_reads_negative_records_with_causes() {
    let tmp = std::env::temp_dir().join(format!("formality-coverage-neg-{}", std::process::id()));
    std::fs::create_dir_all(&tmp).unwrap();
    let path = tmp.join("test-coverage.jsonl");

    // Mix of a positive record (no `kind` — back-compat) and two negative
    // records that blame the same rule but with different causes.
    let body = concat!(
        r#"{"test_file":"a.rs","test_line":1,"test_column":1,"rules":[{"judgment":"j1","rule":"r1","file":"f.rs","line":10}]}"#,
        "\n",
        r#"{"test_file":"b.rs","test_line":2,"test_column":1,"kind":"negative","rules":[{"rule":"r1","file":"f.rs","line":10,"cause":"if_false"}]}"#,
        "\n",
        r#"{"test_file":"c.rs","test_line":3,"test_column":1,"kind":"negative","rules":[{"rule":"r1","file":"f.rs","line":10,"cause":"if_let"}]}"#,
        "\n",
    );
    std::fs::write(&path, body).unwrap();

    let cov = jsonl::read(&path).unwrap();
    assert_eq!(cov.positive.len(), 1);
    let causes = cov
        .negative
        .get(&BlamedRuleLoc {
            rule: "r1".into(),
            file: "f.rs".into(),
            line: 10,
        })
        .expect("negative record should be present");
    let causes: Vec<&str> = causes.iter().map(String::as_str).collect();
    assert_eq!(causes, vec!["if_false", "if_let"]);

    // The lookup matches by rule name + file suffix.
    let causes = cov.negative_causes_for("f.rs", "r1");
    let causes: Vec<&str> = causes.iter().map(String::as_str).collect();
    assert_eq!(causes, vec!["if_false", "if_let"]);
    // Different file should not match.
    assert!(cov.negative_causes_for("other.rs", "r1").is_empty());

    let _ = std::fs::remove_dir_all(&tmp);
}

#[test]
fn negative_coverage_renders_in_subpage_with_causes() {
    let j = Judgment {
        name: "prove_thing".into(),
        doc_comment: String::new(),
        signature: String::new(),
        file: "fixture.rs".into(),
        line: 4,
        rules: vec![fallible_rule("positive", 10), fallible_rule("zero", 16)],
    };
    let mut negative = BTreeMap::new();
    let mut causes = BTreeSet::new();
    causes.insert("if_false".to_string());
    causes.insert("failed_judgment".to_string());
    // `file` here is whatever the macro recorded — a path including the
    // file basename. The matcher resolves it against the scraped
    // judgment file via suffix overlap.
    negative.insert(
        BlamedRuleLoc {
            rule: "positive".into(),
            file: "crates/sub/fixture.rs".into(),
            line: 11,
        },
        causes,
    );
    let cov = Coverage {
        positive: BTreeSet::new(),
        negative,
        implicit_no_match: BTreeSet::new(),
    };

    let md = report::render_subpage(&j, &cov);
    expect![[r#"
        # Judgment `prove_thing` at fixture.rs:4

        | Rule | Line | Positive coverage | Negative coverage |
        | --- | --- | --- | --- |
        | <a id="positive"></a>`positive` | 10 | ✗ | ✓ (failed_judgment, if_false) |
        | <a id="zero"></a>`zero` | 16 | ✗ | ✗ |
    "#]]
    .assert_eq(&md);
}

#[test]
fn jsonl_reads_implicit_no_match_records() {
    let tmp = std::env::temp_dir().join(format!(
        "formality-coverage-implicit-{}",
        std::process::id()
    ));
    std::fs::create_dir_all(&tmp).unwrap();
    let path = tmp.join("test-coverage.jsonl");

    let body = concat!(
        r#"{"test_file":"a.rs","test_line":1,"test_column":1,"kind":"negative","rules":[],"implicit_no_match":[{"judgment":"j_alpha","file":"crates/x/src/foo.rs","line":42}]}"#,
        "\n",
        r#"{"test_file":"b.rs","test_line":2,"test_column":1,"kind":"negative","rules":[{"rule":"r1","file":"f.rs","line":10,"cause":"if_false"}],"implicit_no_match":[]}"#,
        "\n",
    );
    std::fs::write(&path, body).unwrap();

    let cov = jsonl::read(&path).unwrap();
    assert_eq!(cov.implicit_no_match.len(), 1);
    assert!(cov.implicit_no_match.contains(&ImplicitNoMatchLoc {
        judgment: "j_alpha".into(),
        file: "crates/x/src/foo.rs".into(),
        line: 42,
    }));
    // Path overlap matches by suffix.
    assert!(cov.implicit_no_match_observed("src/foo.rs", "j_alpha"));
    assert!(!cov.implicit_no_match_observed("src/foo.rs", "j_other"));

    let _ = std::fs::remove_dir_all(&tmp);
}

#[test]
fn implicit_no_match_renders_in_index_and_subpage() {
    let j = Judgment {
        name: "prove_thing".into(),
        doc_comment: String::new(),
        signature: String::new(),
        file: "fixture.rs".into(),
        line: 4,
        rules: vec![fallible_rule("positive", 10)],
    };
    let mut implicit = BTreeSet::new();
    implicit.insert(ImplicitNoMatchLoc {
        judgment: "prove_thing".into(),
        file: "crates/sub/fixture.rs".into(),
        line: 4,
    });
    let cov = Coverage {
        positive: BTreeSet::new(),
        negative: BTreeMap::new(),
        implicit_no_match: implicit,
    };

    let index = report::render_index(&[j.clone()], &cov);
    expect![[r#"
        # Coverage report

        | Judgment/Rule | Positive coverage | Negative coverage |
        | --- | --- | --- |
        | **[prove_thing](./prove_thing.md)** | - | implicit no-match observed |
        | ↳ [positive](./prove_thing.md#positive) | ✗ | ✗ |
    "#]]
    .assert_eq(&index);

    let subpage = report::render_subpage(&j, &cov);
    expect![[r#"
        # Judgment `prove_thing` at fixture.rs:4

        _Implicit no-match observed: at least one test exercised this judgment with no matching rule._

        | Rule | Line | Positive coverage | Negative coverage |
        | --- | --- | --- | --- |
        | <a id="positive"></a>`positive` | 10 | ✗ | ✗ |
    "#]]
    .assert_eq(&subpage);
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

#[test]
fn unblameable_rule_renders_as_na() {
    let infallible_rule = Rule {
        name: "trivial".into(),
        raw_text: String::new(),
        line: 7,
        premises: vec![Premise {
            raw_text: "let x = y".into(),
            kind: PremiseKind::Let,
            fallible: false,
        }],
    };
    let j = Judgment {
        name: "easy".into(),
        doc_comment: String::new(),
        signature: String::new(),
        file: "fixture.rs".into(),
        line: 1,
        rules: vec![infallible_rule],
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

        | Rule | Line | Positive coverage | Negative coverage |
        | --- | --- | --- | --- |
        | <a id="trivial"></a>`trivial` | 7 | ✗ | N/A |
    "#]]
    .assert_eq(&subpage);
}
