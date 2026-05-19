//! End-to-end-ish tests for the scraper + report generator. Uses inline
//! fixtures (a fake `judgment_fn!` source snippet) so the tests don't depend
//! on the live `formality-rust` source layout.

use std::collections::BTreeSet;

use expect_test::expect;
use formality_coverage::jsonl::{self, CoveredRule};
use formality_coverage::report;
use formality_coverage::scrape::{scrape_text, Judgment, Rule};

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
            file: "fixture.rs".into(),
            line: 4,
            rules: vec![
                Rule {
                    name: "positive".into(),
                    line: 10,
                },
                Rule {
                    name: "zero".into(),
                    line: 16,
                },
            ],
        },
        Judgment {
            name: "only_one".into(),
            file: "fixture.rs".into(),
            line: 22,
            rules: vec![Rule {
                name: "one".into(),
                line: 28,
            }],
        },
    ];

    let mut covered = BTreeSet::new();
    covered.insert(CoveredRule {
        judgment: "prove_thing".into(),
        rule: "positive".into(),
    });

    let md = report::render_index(&judgments, &covered);
    expect![[r#"
        # Coverage report

        | Judgment/Rule | Positive coverage |
        | --- | --- |
        | **[prove_thing](./prove_thing.md)** | - |
        | ↳ [positive](./prove_thing.md#positive) | :check: |
        | ↳ [zero](./prove_thing.md#zero) | :x: |
        | **[only_one](./only_one.md)** | - |
        | ↳ [one](./only_one.md#one) | :x: |
    "#]]
    .assert_eq(&md);
}

#[test]
fn markdown_subpage_snapshot() {
    let j = Judgment {
        name: "prove_thing".into(),
        file: "fixture.rs".into(),
        line: 4,
        rules: vec![
            Rule {
                name: "positive".into(),
                line: 10,
            },
            Rule {
                name: "zero".into(),
                line: 16,
            },
        ],
    };
    let mut covered = BTreeSet::new();
    covered.insert(CoveredRule {
        judgment: "prove_thing".into(),
        rule: "positive".into(),
    });

    let md = report::render_subpage(&j, &covered);
    expect![[r#"
        # Judgment `prove_thing` at fixture.rs:4

        | Rule | Line | Positive coverage |
        | --- | --- | --- |
        | <a id="positive"></a>`positive` | 10 | :check: |
        | <a id="zero"></a>`zero` | 16 | :x: |
    "#]]
    .assert_eq(&md);
}

#[test]
fn empty_rules_renders_no_rules_message() {
    let j = Judgment {
        name: "lonely".into(),
        file: "fixture.rs".into(),
        line: 1,
        rules: vec![],
    };
    let covered = BTreeSet::new();
    let md = report::render_subpage(&j, &covered);
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
