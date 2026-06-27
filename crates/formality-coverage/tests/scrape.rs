//! End-to-end-ish tests for the scraper + report generator. Uses inline
//! fixtures (a fake `judgment_fn!` source snippet) so the tests don't depend
//! on the live `formality-rust` source layout.

use std::collections::{BTreeMap, BTreeSet};

use expect_test::expect;
use formality_core::judgment::coverage::{FailedRuleNode, FailedTreeNode, ProofTreeNode};
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
        raw_text: format!("(if true)\n----- (\"{name}\")\n(prove_thing(x) => ())"),
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
        signature: "prove_thing(x: u32) => ()".into(),
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
        ..Default::default()
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
    let md = report::render_subpage(&j, &cov, "md");
    expect![[r##"
        # Judgment `prove_thing` at fixture.rs:4

        **Signature:**

        ```rust,ignore
        prove_thing(x: u32) => ()
        ```

        The number on each rule's conclusion is **positive** coverage; the number on each premise is **negative** coverage. Click a number to browse the tests.

        <style>
        .cov-rule{border:1px solid var(--quote-border,#d0d0d0);border-radius:6px;margin:1rem 0;overflow:hidden}
        .cov-rule-head{padding:.4rem .8rem;background:var(--quote-bg,#f6f7f9);font-weight:600}
        table.cov-code{width:100%;border-collapse:collapse;font-family:var(--mono-font,monospace);font-size:.85em;margin:0}
        table.cov-code td{padding:.15rem .6rem;border:0}
        table.cov-code th{padding:.15rem .6rem;border:0;border-bottom:1px solid var(--quote-border,#d0d0d0);color:#888;font-weight:600;font-size:.9em}
        .cov-ln{text-align:right;color:#999;user-select:none;width:3em;white-space:nowrap}
        .cov-num{text-align:right;width:3.5em;white-space:nowrap;font-weight:600}
        .cov-num.pos a{color:#1a7f37}
        .cov-num.neg a{color:#b35900}
        .cov-none{color:#bbb}
        .cov-na{color:#bbb;font-weight:400}
        .cov-src-line{white-space:pre-wrap}
        tr.cov-sep td{color:#999}
        tr.cov-concl{background:rgba(127,127,127,.08)}
        </style>

        <div class="cov-rule" id="positive">
        <div class="cov-rule-head"><code>positive</code></div>
        <table class="cov-code">
        <thead><tr><th class="cov-ln">Line</th><th class="cov-num">Coverage</th><th class="cov-src-line">Source</th></tr></thead>
        <tr><td class="cov-ln">9</td><td class="cov-num neg"><a href="./prove_thing__positive__p9__neg.md" title="failure causes: if_false">1</a></td><td class="cov-src-line">(if true)</td></tr>
        <tr class="cov-sep"><td class="cov-ln"></td><td class="cov-num"></td><td class="cov-src-line">──────── ("positive")</td></tr>
        <tr class="cov-concl"><td class="cov-ln">11</td><td class="cov-num pos"><a href="./prove_thing__positive__pos.md">1</a></td><td class="cov-src-line">(prove_thing(x) =&gt; ())</td></tr>
        </table>
        </div>

        <div class="cov-rule" id="zero">
        <div class="cov-rule-head"><code>zero</code></div>
        <table class="cov-code">
        <thead><tr><th class="cov-ln">Line</th><th class="cov-num">Coverage</th><th class="cov-src-line">Source</th></tr></thead>
        <tr><td class="cov-ln">15</td><td class="cov-num neg"><span class="cov-none">✗</span></td><td class="cov-src-line">(if true)</td></tr>
        <tr class="cov-sep"><td class="cov-ln"></td><td class="cov-num"></td><td class="cov-src-line">──────── ("zero")</td></tr>
        <tr class="cov-concl"><td class="cov-ln">17</td><td class="cov-num pos"><span class="cov-none">✗</span></td><td class="cov-src-line">(prove_thing(x) =&gt; ())</td></tr>
        </table>
        </div>

    "##]].assert_eq(&md);
}

#[test]
fn detail_pages_list_tests() {
    let j = prove_thing_judgment();
    let cov = prove_thing_coverage();
    let pages = report::render_detail_pages_for(&j, &cov, Some(GITHUB_BASE), None);

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
    // Each test is listed flat with its source location linked to GitHub.
    assert!(
        pos.content.contains(
            "**Source location:** [tests/prove_thing.rs:42](https://github.com/example/repo/blob/main/tests/prove_thing.rs#L42)"
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
            "**Source location:** [tests/prove_thing.rs:99](https://github.com/example/repo/blob/main/tests/prove_thing.rs#L99)"
        ),
        "{}",
        neg.content
    );
}

#[test]
fn detail_pages_without_github_base_use_plain_locations() {
    let j = prove_thing_judgment();
    let cov = prove_thing_coverage();
    let pages = report::render_detail_pages_for(&j, &cov, None, None);
    let pos = &pages[0];
    assert!(
        pos.content
            .contains("**Source location:** tests/prove_thing.rs:42"),
        "{}",
        pos.content
    );
    assert!(!pos.content.contains("https://"), "{}", pos.content);
}

#[test]
fn detail_pages_render_proof_trees() {
    let j = prove_thing_judgment();
    let mut cov = prove_thing_coverage();

    // The positive test (tests/prove_thing.rs:42) proved `prove_thing` via the
    // `positive` rule, which in turn proved a sub-judgment.
    cov.positive_trees.insert(
        TestLoc {
            file: "tests/prove_thing.rs".into(),
            line: 42,
        },
        vec![ProofTreeNode {
            judgment: "prove_thing".into(),
            rule: Some("positive".into()),
            file: "crates/sub/fixture.rs".into(),
            line: 11,
            children: vec![ProofTreeNode {
                judgment: "prove_sub".into(),
                rule: Some("sub".into()),
                file: "crates/sub/other.rs".into(),
                line: 3,
                children: vec![],
            }],
        }],
    );

    // The negative test (tests/prove_thing.rs:99) failed two stacks: one blaming
    // the `positive` premise on line 9 (which this page is about) and one blaming
    // the `zero` premise on line 15 (which it is not).
    cov.negative_trees.insert(
        TestLoc {
            file: "tests/prove_thing.rs".into(),
            line: 99,
        },
        vec![FailedTreeNode {
            judgment: "prove_thing".into(),
            file: "crates/sub/fixture.rs".into(),
            line: 4,
            rules: vec![
                FailedRuleNode {
                    rule: Some("positive".into()),
                    file: "crates/sub/fixture.rs".into(),
                    line: 9,
                    cause: "if_false".into(),
                    child: None,
                },
                FailedRuleNode {
                    rule: Some("zero".into()),
                    file: "crates/sub/fixture.rs".into(),
                    line: 15,
                    cause: "if_false".into(),
                    child: None,
                },
            ],
        }],
    );

    let pages = report::render_detail_pages_for(&j, &cov, Some(GITHUB_BASE), None);
    let pos = &pages[0];
    let neg = &pages[1];

    // The positive page shows the success tree behind a collapsed disclosure.
    assert!(
        pos.content.contains("<summary>Proof tree</summary>"),
        "{}",
        pos.content
    );
    assert!(
        pos.content
            .contains("└─ prove_thing (positive) at fixture.rs:11"),
        "{}",
        pos.content
    );
    assert!(
        pos.content.contains("   └─ prove_sub (sub) at other.rs:3"),
        "{}",
        pos.content
    );

    // The negative page shows the failed tree pruned to the stack that blames
    // premise line 9; the unrelated `zero` stack (line 15) is dropped.
    assert!(
        neg.content.contains("<summary>Failed proof tree</summary>"),
        "{}",
        neg.content
    );
    assert!(
        neg.content
            .contains(r#"└─ rule "positive" at fixture.rs:9 (failed: if_false)"#),
        "{}",
        neg.content
    );
    assert!(
        !neg.content.contains("zero"),
        "unrelated stack should be pruned: {}",
        neg.content
    );
}

#[test]
fn detail_pages_embed_test_source_when_root_given() {
    // A judgment with one rule, covered by one test that lives in a real file
    // on disk so the detail page can embed its source.
    let tmp = std::env::temp_dir().join(format!("formality-coverage-src-{}", std::process::id()));
    std::fs::create_dir_all(&tmp).unwrap();
    let test_file = "my_module.rs";
    let body = "// preamble\n#[test]\nfn my_test() {\n    let x = 1;\n    assert_eq!(x, 1);\n}\n";
    std::fs::write(tmp.join(test_file), body).unwrap();

    let j = Judgment {
        name: "j".into(),
        doc_comment: String::new(),
        signature: String::new(),
        file: "fixture.rs".into(),
        line: 1,
        rules: vec![rule_with_premise("r", 3, 2)],
    };
    let mut cov = Coverage::default();
    cov.positive.insert(
        CoveredRule {
            judgment: "j".into(),
            rule: "r".into(),
        },
        // The `assert_eq!` line (5) is inside `my_test`.
        BTreeSet::from([TestLoc {
            file: test_file.into(),
            line: 5,
        }]),
    );

    let pages = report::render_detail_pages_for(&j, &cov, None, Some(&tmp));
    let pos = &pages[0];
    // The whole enclosing test function (including the `#[test]` attribute) is
    // embedded in a fenced code block.
    assert!(pos.content.contains("```rust,ignore"), "{}", pos.content);
    assert!(pos.content.contains("#[test]"), "{}", pos.content);
    assert!(pos.content.contains("fn my_test() {"), "{}", pos.content);
    assert!(pos.content.contains("assert_eq!(x, 1);"), "{}", pos.content);

    let _ = std::fs::remove_dir_all(&tmp);
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
            raw_text: "(let x = y)\n----- (\"trivial\")\n(easy(y) => x)".into(),
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

    let subpage = report::render_subpage(&j, &cov, "md");
    expect![[r##"
        # Judgment `easy` at fixture.rs:1

        The number on each rule's conclusion is **positive** coverage; the number on each premise is **negative** coverage. Click a number to browse the tests.

        <style>
        .cov-rule{border:1px solid var(--quote-border,#d0d0d0);border-radius:6px;margin:1rem 0;overflow:hidden}
        .cov-rule-head{padding:.4rem .8rem;background:var(--quote-bg,#f6f7f9);font-weight:600}
        table.cov-code{width:100%;border-collapse:collapse;font-family:var(--mono-font,monospace);font-size:.85em;margin:0}
        table.cov-code td{padding:.15rem .6rem;border:0}
        table.cov-code th{padding:.15rem .6rem;border:0;border-bottom:1px solid var(--quote-border,#d0d0d0);color:#888;font-weight:600;font-size:.9em}
        .cov-ln{text-align:right;color:#999;user-select:none;width:3em;white-space:nowrap}
        .cov-num{text-align:right;width:3.5em;white-space:nowrap;font-weight:600}
        .cov-num.pos a{color:#1a7f37}
        .cov-num.neg a{color:#b35900}
        .cov-none{color:#bbb}
        .cov-na{color:#bbb;font-weight:400}
        .cov-src-line{white-space:pre-wrap}
        tr.cov-sep td{color:#999}
        tr.cov-concl{background:rgba(127,127,127,.08)}
        </style>

        <div class="cov-rule" id="trivial">
        <div class="cov-rule-head"><code>trivial</code></div>
        <table class="cov-code">
        <thead><tr><th class="cov-ln">Line</th><th class="cov-num">Coverage</th><th class="cov-src-line">Source</th></tr></thead>
        <tr><td class="cov-ln">6</td><td class="cov-num neg"><span class="cov-na">N/A</span></td><td class="cov-src-line">(let x = y)</td></tr>
        <tr class="cov-sep"><td class="cov-ln"></td><td class="cov-num"></td><td class="cov-src-line">──────── ("trivial")</td></tr>
        <tr class="cov-concl"><td class="cov-ln">8</td><td class="cov-num pos"><span class="cov-none">✗</span></td><td class="cov-src-line">(easy(y) =&gt; x)</td></tr>
        </table>
        </div>

    "##]].assert_eq(&subpage);
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

    let subpage = report::render_subpage(&j, &cov, "md");
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
    let md = report::render_subpage(&j, &cov, "md");
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
fn jsonl_reads_embedded_proof_trees() {
    let tmp = std::env::temp_dir().join(format!("formality-coverage-trees-{}", std::process::id()));
    std::fs::create_dir_all(&tmp).unwrap();
    let path = tmp.join("test-coverage.jsonl");

    let body = concat!(
        r#"{"kind":"positive","test_file":"a.rs","test_line":1,"test_column":1,"rules":[{"judgment":"j1","rule":"r1","file":"f.rs","line":10}],"trees":[{"judgment":"j1","rule":"r1","file":"f.rs","line":10,"children":[{"judgment":"j2","rule":"r2","file":"g.rs","line":20}]}]}"#,
        "\n",
        r#"{"kind":"negative","test_file":"b.rs","test_line":2,"test_column":1,"reasons":[{"reason":"premise","judgment":"j1","rule":"r1","file":"f.rs","line":11,"cause":"if_false"}],"trees":[{"judgment":"j1","file":"f.rs","line":1,"rules":[{"rule":"r1","file":"f.rs","line":11,"cause":"if_false"}]}]}"#,
        "\n",
    );
    std::fs::write(&path, body).unwrap();

    let cov = jsonl::read(&path).unwrap();

    let pos = cov.positive_trees_for(&TestLoc {
        file: "a.rs".into(),
        line: 1,
    });
    assert_eq!(pos.len(), 1);
    assert_eq!(pos[0].judgment, "j1");
    assert_eq!(pos[0].rule.as_deref(), Some("r1"));
    assert_eq!(pos[0].children[0].judgment, "j2");

    let neg = cov.negative_trees_for(&TestLoc {
        file: "b.rs".into(),
        line: 2,
    });
    assert_eq!(neg.len(), 1);
    assert_eq!(neg[0].rules[0].rule.as_deref(), Some("r1"));
    assert_eq!(neg[0].rules[0].cause, "if_false");

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
