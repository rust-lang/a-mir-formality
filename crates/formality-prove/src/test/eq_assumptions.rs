use expect_test::expect;
use formality_macros::test;
use formality_types::rust::term;

use crate::decls::Decls;

use crate::test_util::test_prove;

#[test]
fn test_a() {
    test_prove(
        Decls::empty(),
        term("{} => {for<ty T, ty U> if {T = u32, U = Vec<T>} U = Vec<u32>}"),
    )
    .assert_ok(expect![[r#"
        {
          Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} },
        }
    "#]]);
}

#[test]
fn test_b() {
    test_prove(
        Decls::empty(),
        term("exists<ty A> {} => {for<ty T, ty U> if {T = u32, U = Vec<T>} A = U}"),
    )
    .assert_ok(expect![[r#"
        {
          Constraints { env: Env { variables: [?ty_2, ?ty_1], bias: Soundness }, known_true: true, substitution: {?ty_1 => Vec<u32>, ?ty_2 => u32} },
        }
    "#]]);
}

#[test]
fn test_normalize_assoc_ty() {
    test_prove(
        Decls::empty(),
        term("{} => {for<ty T> if { <T as Iterator>::Item = u32 } <T as Iterator>::Item = u32}"),
    )
    .assert_ok(expect![[r#"
        {
          Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} },
        }
    "#]]);
}

#[test]
fn test_normalize_assoc_ty_existential0() {
    test_prove(
        Decls::empty(),
        term("exists<ty A> {} => {for<ty T> if { <T as Iterator>::Item = u32 } <A as Iterator>::Item = u32}"),
    ).assert_err(
    expect![[r#"
        judgment `prove { goal: {for <ty> if {<^ty0_0 as Iterator>::Item = u32} <?ty_0 as Iterator>::Item = u32}, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness }, decls: decls(222, [], [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
          failed at (src/file.rs:LL:CC) because
            judgment `prove_wc_list { goal: {for <ty> if {<^ty0_0 as Iterator>::Item = u32} <?ty_0 as Iterator>::Item = u32}, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness } }` failed at the following rule(s):
              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                judgment `prove_wc { goal: for <ty> if {<^ty0_0 as Iterator>::Item = u32} <?ty_0 as Iterator>::Item = u32, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness } }` failed at the following rule(s):
                  the rule "forall" failed at step #2 (src/file.rs:LL:CC) because
                    judgment `prove_wc { goal: if {<!ty_1 as Iterator>::Item = u32} <?ty_0 as Iterator>::Item = u32, assumptions: {}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness } }` failed at the following rule(s):
                      the rule "implies" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: <?ty_0 as Iterator>::Item = u32, assumptions: {<!ty_1 as Iterator>::Item = u32}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness } }` failed at the following rule(s):
                          the rule "eq" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `prove_eq { a: <?ty_0 as Iterator>::Item, b: u32, assumptions: {<!ty_1 as Iterator>::Item = u32}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness } }` failed at the following rule(s):
                              the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                                judgment `prove_normalize { p: <?ty_0 as Iterator>::Item, assumptions: {<!ty_1 as Iterator>::Item = u32}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness } }` failed at the following rule(s):
                                  the rule "normalize-via-assumption" failed at step #1 (src/file.rs:LL:CC) because
                                    judgment `prove_normalize_via { goal: <?ty_0 as Iterator>::Item, via: <!ty_1 as Iterator>::Item = u32, assumptions: {<!ty_1 as Iterator>::Item = u32}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness } }` failed at the following rule(s):
                                      the rule "axiom-l" failed at step #2 (src/file.rs:LL:CC) because
                                        judgment `prove_syntactically_eq { a: <!ty_1 as Iterator>::Item, b: <?ty_0 as Iterator>::Item, assumptions: {<!ty_1 as Iterator>::Item = u32}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness } }` failed at the following rule(s):
                                          the rule "alias" failed at step #3 (src/file.rs:LL:CC) because
                                            judgment `prove_syntactically_eq { a: !ty_1, b: ?ty_0, assumptions: {<!ty_1 as Iterator>::Item = u32}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness } }` failed at the following rule(s):
                                              the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                                judgment `prove_syntactically_eq { a: ?ty_0, b: !ty_1, assumptions: {<!ty_1 as Iterator>::Item = u32}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness } }` failed at the following rule(s):
                                                  the rule "existential-nonvar" failed at step #0 (src/file.rs:LL:CC) because
                                                    judgment `prove_existential_var_eq { v: ?ty_0, b: !ty_1, assumptions: {<!ty_1 as Iterator>::Item = u32}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness } }` failed at the following rule(s):
                                                      the rule "existential-nonvar" failed at step #0 (src/file.rs:LL:CC) because
                                                        pattern `None` did not match value `Some(!ty_1)`
                                                      the rule "existential-universal" failed at step #0 (src/file.rs:LL:CC) because
                                                        condition evaluted to false: `env.universe(p) < env.universe(v)`
                                          the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                            judgment `prove_syntactically_eq { a: <?ty_0 as Iterator>::Item, b: <!ty_1 as Iterator>::Item, assumptions: {<!ty_1 as Iterator>::Item = u32}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness } }` failed at the following rule(s):
                                              the rule "alias" failed at step #3 (src/file.rs:LL:CC) because
                                                judgment `prove_syntactically_eq { a: ?ty_0, b: !ty_1, assumptions: {<!ty_1 as Iterator>::Item = u32}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness } }` failed at the following rule(s):
                                                  the rule "existential-nonvar" failed at step #0 (src/file.rs:LL:CC) because
                                                    judgment `prove_existential_var_eq { v: ?ty_0, b: !ty_1, assumptions: {<!ty_1 as Iterator>::Item = u32}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness } }` failed at the following rule(s):
                                                      the rule "existential-nonvar" failed at step #0 (src/file.rs:LL:CC) because
                                                        pattern `None` did not match value `Some(!ty_1)`
                                                      the rule "existential-universal" failed at step #0 (src/file.rs:LL:CC) because
                                                        condition evaluted to false: `env.universe(p) < env.universe(v)`
                                  the rule "normalize-via-impl" failed at step #0 (src/file.rs:LL:CC) because
                                    expression evaluated to an empty collection: `decls.alias_eq_decls(&a.name)`
                              the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                judgment `prove_eq { a: u32, b: <?ty_0 as Iterator>::Item, assumptions: {<!ty_1 as Iterator>::Item = u32}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness } }` failed at the following rule(s):
                                  the rule "normalize-l" failed at step #1 (src/file.rs:LL:CC) because
                                    judgment `prove_after { constraints: Constraints { env: Env { variables: [?ty_0, !ty_1], bias: Soundness }, known_true: true, substitution: {} }, goal: {<!ty_1 as Iterator>::Item = <?ty_0 as Iterator>::Item}, assumptions: {<!ty_1 as Iterator>::Item = u32} }` failed at the following rule(s):
                                      the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                                        judgment `prove { goal: {<!ty_0 as Iterator>::Item = <?ty_1 as Iterator>::Item}, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness }, decls: decls(222, [], [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                                          failed at (src/file.rs:LL:CC) because
                                            judgment `prove_wc_list { goal: {<!ty_0 as Iterator>::Item = <?ty_1 as Iterator>::Item}, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                                judgment `prove_wc { goal: <!ty_0 as Iterator>::Item = <?ty_1 as Iterator>::Item, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                  the rule "eq" failed at step #0 (src/file.rs:LL:CC) because
                                                    judgment `prove_eq { a: <!ty_0 as Iterator>::Item, b: <?ty_1 as Iterator>::Item, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                      the rule "alias" failed at step #3 (src/file.rs:LL:CC) because
                                                        judgment `prove { goal: {!ty_0 = ?ty_1}, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness }, decls: decls(222, [], [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                                                          failed at (src/file.rs:LL:CC) because
                                                            judgment `prove_wc_list { goal: {!ty_0 = ?ty_1}, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                                                judgment `prove_wc { goal: !ty_0 = ?ty_1, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                  the rule "eq" failed at step #0 (src/file.rs:LL:CC) because
                                                                    judgment `prove_eq { a: !ty_0, b: ?ty_1, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                      the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                                                        judgment `prove_eq { a: ?ty_1, b: !ty_0, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                          the rule "existential" failed at step #0 (src/file.rs:LL:CC) because
                                                                            judgment `prove_existential_var_eq { v: ?ty_1, b: !ty_0, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                              the rule "existential-nonvar" failed at step #0 (src/file.rs:LL:CC) because
                                                                                pattern `None` did not match value `Some(!ty_0)`
                                                                              the rule "existential-universal" failed at step #0 (src/file.rs:LL:CC) because
                                                                                condition evaluted to false: `env.universe(p) < env.universe(v)`
                                                      the rule "normalize-l" failed at step #1 (src/file.rs:LL:CC) because
                                                        judgment `prove_after { constraints: Constraints { env: Env { variables: [?ty_1, !ty_0], bias: Soundness }, known_true: true, substitution: {} }, goal: {u32 = <?ty_1 as Iterator>::Item}, assumptions: {<!ty_0 as Iterator>::Item = u32} }` failed at the following rule(s):
                                                          the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                                                            judgment `prove { goal: {u32 = <?ty_1 as Iterator>::Item}, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness }, decls: decls(222, [], [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                                                              failed at (src/file.rs:LL:CC) because
                                                                judgment `prove_wc_list { goal: {u32 = <?ty_1 as Iterator>::Item}, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                                                    judgment `prove_wc { goal: u32 = <?ty_1 as Iterator>::Item, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                      the rule "eq" failed at step #0 (src/file.rs:LL:CC) because
                                                                        judgment `prove_eq { a: u32, b: <?ty_1 as Iterator>::Item, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                          the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                                                            judgment `prove_eq { a: <?ty_1 as Iterator>::Item, b: u32, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                              the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                                                                                judgment `prove_normalize { p: <?ty_1 as Iterator>::Item, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                                  the rule "normalize-via-assumption" failed at step #1 (src/file.rs:LL:CC) because
                                                                                    judgment `prove_normalize_via { goal: <?ty_1 as Iterator>::Item, via: <!ty_0 as Iterator>::Item = u32, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                                      the rule "axiom-l" failed at step #2 (src/file.rs:LL:CC) because
                                                                                        judgment `prove_syntactically_eq { a: <!ty_0 as Iterator>::Item, b: <?ty_1 as Iterator>::Item, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                                          the rule "alias" failed at step #3 (src/file.rs:LL:CC) because
                                                                                            judgment `prove_syntactically_eq { a: !ty_0, b: ?ty_1, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                                              the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                                                                                judgment `prove_syntactically_eq { a: ?ty_1, b: !ty_0, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                                                  the rule "existential-nonvar" failed at step #0 (src/file.rs:LL:CC) because
                                                                                                    judgment `prove_existential_var_eq { v: ?ty_1, b: !ty_0, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                                                      the rule "existential-nonvar" failed at step #0 (src/file.rs:LL:CC) because
                                                                                                        pattern `None` did not match value `Some(!ty_0)`
                                                                                                      the rule "existential-universal" failed at step #0 (src/file.rs:LL:CC) because
                                                                                                        condition evaluted to false: `env.universe(p) < env.universe(v)`
                                                                                          the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                                                                            judgment `prove_syntactically_eq { a: <?ty_1 as Iterator>::Item, b: <!ty_0 as Iterator>::Item, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                                              the rule "alias" failed at step #3 (src/file.rs:LL:CC) because
                                                                                                judgment `prove_syntactically_eq { a: ?ty_1, b: !ty_0, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                                                  the rule "existential-nonvar" failed at step #0 (src/file.rs:LL:CC) because
                                                                                                    judgment `prove_existential_var_eq { v: ?ty_1, b: !ty_0, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                                                      the rule "existential-nonvar" failed at step #0 (src/file.rs:LL:CC) because
                                                                                                        pattern `None` did not match value `Some(!ty_0)`
                                                                                                      the rule "existential-universal" failed at step #0 (src/file.rs:LL:CC) because
                                                                                                        condition evaluted to false: `env.universe(p) < env.universe(v)`
                                                                                  the rule "normalize-via-impl" failed at step #0 (src/file.rs:LL:CC) because
                                                                                    expression evaluated to an empty collection: `decls.alias_eq_decls(&a.name)`
                                                      the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                                        judgment `prove_eq { a: <?ty_1 as Iterator>::Item, b: <!ty_0 as Iterator>::Item, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                          the rule "alias" failed at step #3 (src/file.rs:LL:CC) because
                                                            judgment `prove { goal: {?ty_1 = !ty_0}, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness }, decls: decls(222, [], [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                                                              failed at (src/file.rs:LL:CC) because
                                                                judgment `prove_wc_list { goal: {?ty_1 = !ty_0}, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                                                    judgment `prove_wc { goal: ?ty_1 = !ty_0, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                      the rule "eq" failed at step #0 (src/file.rs:LL:CC) because
                                                                        judgment `prove_eq { a: ?ty_1, b: !ty_0, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                          the rule "existential" failed at step #0 (src/file.rs:LL:CC) because
                                                                            judgment `prove_existential_var_eq { v: ?ty_1, b: !ty_0, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                              the rule "existential-nonvar" failed at step #0 (src/file.rs:LL:CC) because
                                                                                pattern `None` did not match value `Some(!ty_0)`
                                                                              the rule "existential-universal" failed at step #0 (src/file.rs:LL:CC) because
                                                                                condition evaluted to false: `env.universe(p) < env.universe(v)`
                                                          the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                                                            judgment `prove_normalize { p: <?ty_1 as Iterator>::Item, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                              the rule "normalize-via-assumption" failed at step #1 (src/file.rs:LL:CC) because
                                                                judgment `prove_normalize_via { goal: <?ty_1 as Iterator>::Item, via: <!ty_0 as Iterator>::Item = u32, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                  the rule "axiom-l" failed at step #2 (src/file.rs:LL:CC) because
                                                                    judgment `prove_syntactically_eq { a: <!ty_0 as Iterator>::Item, b: <?ty_1 as Iterator>::Item, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                      the rule "alias" failed at step #3 (src/file.rs:LL:CC) because
                                                                        judgment `prove_syntactically_eq { a: !ty_0, b: ?ty_1, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                          the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                                                            judgment `prove_syntactically_eq { a: ?ty_1, b: !ty_0, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                              the rule "existential-nonvar" failed at step #0 (src/file.rs:LL:CC) because
                                                                                judgment `prove_existential_var_eq { v: ?ty_1, b: !ty_0, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                                  the rule "existential-nonvar" failed at step #0 (src/file.rs:LL:CC) because
                                                                                    pattern `None` did not match value `Some(!ty_0)`
                                                                                  the rule "existential-universal" failed at step #0 (src/file.rs:LL:CC) because
                                                                                    condition evaluted to false: `env.universe(p) < env.universe(v)`
                                                                      the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                                                        judgment `prove_syntactically_eq { a: <?ty_1 as Iterator>::Item, b: <!ty_0 as Iterator>::Item, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                          the rule "alias" failed at step #3 (src/file.rs:LL:CC) because
                                                                            judgment `prove_syntactically_eq { a: ?ty_1, b: !ty_0, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                              the rule "existential-nonvar" failed at step #0 (src/file.rs:LL:CC) because
                                                                                judgment `prove_existential_var_eq { v: ?ty_1, b: !ty_0, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                                  the rule "existential-nonvar" failed at step #0 (src/file.rs:LL:CC) because
                                                                                    pattern `None` did not match value `Some(!ty_0)`
                                                                                  the rule "existential-universal" failed at step #0 (src/file.rs:LL:CC) because
                                                                                    condition evaluted to false: `env.universe(p) < env.universe(v)`
                                                              the rule "normalize-via-impl" failed at step #0 (src/file.rs:LL:CC) because
                                                                expression evaluated to an empty collection: `decls.alias_eq_decls(&a.name)`"#]]);
}

#[test]
fn test_normalize_assoc_ty_existential1() {
    test_prove(
        Decls::empty(),
        term(
            "\
            forall<ty T> \
            exists<ty A> \
            { <T as Iterator>::Item = u32 } => { <A as Iterator>::Item = u32 }",
        ),
    )
    .assert_ok(expect![[r#"
        {
          Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness }, known_true: true, substitution: {?ty_2 => !ty_1} },
        }
    "#]]);
}
