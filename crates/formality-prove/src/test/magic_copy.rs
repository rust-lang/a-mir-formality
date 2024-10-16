use expect_test::expect;
use formality_macros::test;
use formality_types::rust::term;

use crate::decls::Decls;

use crate::test_util::test_prove;

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Decls {
    Decls {
        trait_decls: vec![
            term("trait Copy<ty Self> where {}"),
            term("trait Magic<ty Self> where {Copy(Self)}"),
        ],
        impl_decls: vec![
            term("impl<ty T> Magic(T) where {Magic(T)}"),
            term("impl Copy(u32) where {}"),
        ],
        ..Decls::empty()
    }
}

#[test]
fn all_t_not_magic() {
    test_prove(decls(), term("{} => {for<ty T> Magic(T)}")).assert_err(
    expect![[r#"
        judgment `prove { goal: {for <ty> Magic(^ty0_0)}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [trait Copy <ty> , trait Magic <ty> where {Copy(^ty0_0)}], [impl <ty> Magic(^ty0_0) where {Magic(^ty0_0)}, impl Copy(u32)], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
          failed at (src/file.rs:LL:CC) because
            judgment `prove_wc_list { goal: {for <ty> Magic(^ty0_0)}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                judgment `prove_wc { goal: for <ty> Magic(^ty0_0), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                  the rule "forall" failed at step #2 (src/file.rs:LL:CC) because
                    judgment `prove_wc { goal: Magic(!ty_1), assumptions: {}, env: Env { variables: [!ty_1], bias: Soundness } }` failed at the following rule(s):
                      the rule "positive impl" failed at step #7 (src/file.rs:LL:CC) because
                        judgment `prove_after { constraints: Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness }, known_true: true, substitution: {?ty_2 => !ty_1} }, goal: {Copy(?ty_2)}, assumptions: {} }` failed at the following rule(s):
                          the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                            judgment `prove { goal: {Copy(!ty_0)}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness }, decls: decls(222, [trait Copy <ty> , trait Magic <ty> where {Copy(^ty0_0)}], [impl <ty> Magic(^ty0_0) where {Magic(^ty0_0)}, impl Copy(u32)], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                              failed at (src/file.rs:LL:CC) because
                                judgment `prove_wc_list { goal: {Copy(!ty_0)}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                    judgment `prove_wc { goal: Copy(!ty_0), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                      the rule "positive impl" failed at step #5 (src/file.rs:LL:CC) because
                                        judgment `prove { goal: {!ty_0 = u32}, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness }, decls: decls(222, [trait Copy <ty> , trait Magic <ty> where {Copy(^ty0_0)}], [impl <ty> Magic(^ty0_0) where {Magic(^ty0_0)}, impl Copy(u32)], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                                          failed at (src/file.rs:LL:CC) because
                                            judgment `prove_wc_list { goal: {!ty_0 = u32}, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                                judgment `prove_wc { goal: !ty_0 = u32, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                                  the rule "assumption - relation" failed at step #1 (src/file.rs:LL:CC) because
                                                    judgment had no applicable rules: `prove_via { goal: !ty_0 = u32, via: Copy(!ty_0), assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }`
                                                  the rule "eq" failed at step #0 (src/file.rs:LL:CC) because
                                                    judgment `prove_eq { a: !ty_0, b: u32, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                                      the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                                                        judgment `prove_normalize { p: !ty_0, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                                          the rule "normalize-via-assumption" failed at step #1 (src/file.rs:LL:CC) because
                                                            judgment had no applicable rules: `prove_normalize_via { goal: !ty_0, via: Copy(!ty_0), assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }`
                                                      the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                                        judgment `prove_eq { a: u32, b: !ty_0, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                                          the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                                                            judgment `prove_normalize { p: u32, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                                              the rule "normalize-via-assumption" failed at step #1 (src/file.rs:LL:CC) because
                                                                judgment had no applicable rules: `prove_normalize_via { goal: u32, via: Copy(!ty_0), assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }`
                                                          the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                                            cyclic proof attempt: `prove_eq { a: !ty_0, b: u32, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }`
                                      the rule "trait implied bound" failed at step #4 (src/file.rs:LL:CC) because
                                        judgment `prove_after { constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness }, known_true: true, substitution: {?ty_1 => !ty_0} }, goal: {Magic(?ty_1)}, assumptions: {} }` failed at the following rule(s):
                                          the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                                            judgment `prove { goal: {Magic(!ty_0)}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness }, decls: decls(222, [trait Copy <ty> , trait Magic <ty> where {Copy(^ty0_0)}], [impl <ty> Magic(^ty0_0) where {Magic(^ty0_0)}, impl Copy(u32)], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                                              failed at (src/file.rs:LL:CC) because
                                                judgment `prove_wc_list { goal: {Magic(!ty_0)}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                                    judgment `prove_wc { goal: Magic(!ty_0), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                                      the rule "positive impl" failed at step #7 (src/file.rs:LL:CC) because
                                                        judgment `prove_after { constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness }, known_true: true, substitution: {?ty_1 => !ty_0} }, goal: {Copy(?ty_1)}, assumptions: {} }` failed at the following rule(s):
                                                          the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                                                            judgment `prove { goal: {Copy(!ty_0)}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness }, decls: decls(222, [trait Copy <ty> , trait Magic <ty> where {Copy(^ty0_0)}], [impl <ty> Magic(^ty0_0) where {Magic(^ty0_0)}, impl Copy(u32)], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                                                              failed at (src/file.rs:LL:CC) because
                                                                cyclic proof attempt: `prove_wc_list { goal: {Copy(!ty_0)}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }`
                                                      the rule "trait implied bound" failed at step #3 (src/file.rs:LL:CC) because
                                                        judgment had no applicable rules: `prove_via { goal: Magic(!ty_0), via: Copy(?ty_1), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness } }`
                      the rule "trait implied bound" failed at step #3 (src/file.rs:LL:CC) because
                        judgment had no applicable rules: `prove_via { goal: Magic(!ty_1), via: Copy(?ty_2), assumptions: {}, env: Env { variables: [!ty_1, ?ty_2], bias: Soundness } }`"#]]);
}

#[test]
fn all_t_not_copy() {
    test_prove(decls(), term("{} => {for<ty T> Copy(T)}")).assert_err(
    expect![[r#"
        judgment `prove { goal: {for <ty> Copy(^ty0_0)}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [trait Copy <ty> , trait Magic <ty> where {Copy(^ty0_0)}], [impl <ty> Magic(^ty0_0) where {Magic(^ty0_0)}, impl Copy(u32)], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
          failed at (src/file.rs:LL:CC) because
            judgment `prove_wc_list { goal: {for <ty> Copy(^ty0_0)}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                judgment `prove_wc { goal: for <ty> Copy(^ty0_0), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                  the rule "forall" failed at step #2 (src/file.rs:LL:CC) because
                    judgment `prove_wc { goal: Copy(!ty_1), assumptions: {}, env: Env { variables: [!ty_1], bias: Soundness } }` failed at the following rule(s):
                      the rule "positive impl" failed at step #5 (src/file.rs:LL:CC) because
                        judgment `prove { goal: {!ty_0 = u32}, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness }, decls: decls(222, [trait Copy <ty> , trait Magic <ty> where {Copy(^ty0_0)}], [impl <ty> Magic(^ty0_0) where {Magic(^ty0_0)}, impl Copy(u32)], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                          failed at (src/file.rs:LL:CC) because
                            judgment `prove_wc_list { goal: {!ty_0 = u32}, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                judgment `prove_wc { goal: !ty_0 = u32, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                  the rule "assumption - relation" failed at step #1 (src/file.rs:LL:CC) because
                                    judgment had no applicable rules: `prove_via { goal: !ty_0 = u32, via: Copy(!ty_0), assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }`
                                  the rule "eq" failed at step #0 (src/file.rs:LL:CC) because
                                    judgment `prove_eq { a: !ty_0, b: u32, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                      the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                                        judgment `prove_normalize { p: !ty_0, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                          the rule "normalize-via-assumption" failed at step #1 (src/file.rs:LL:CC) because
                                            judgment had no applicable rules: `prove_normalize_via { goal: !ty_0, via: Copy(!ty_0), assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }`
                                      the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                        judgment `prove_eq { a: u32, b: !ty_0, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                          the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                                            judgment `prove_normalize { p: u32, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                              the rule "normalize-via-assumption" failed at step #1 (src/file.rs:LL:CC) because
                                                judgment had no applicable rules: `prove_normalize_via { goal: u32, via: Copy(!ty_0), assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }`
                                          the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                            cyclic proof attempt: `prove_eq { a: !ty_0, b: u32, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }`
                      the rule "trait implied bound" failed at step #4 (src/file.rs:LL:CC) because
                        judgment `prove_after { constraints: Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness }, known_true: true, substitution: {?ty_2 => !ty_1} }, goal: {Magic(?ty_2)}, assumptions: {} }` failed at the following rule(s):
                          the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                            judgment `prove { goal: {Magic(!ty_0)}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness }, decls: decls(222, [trait Copy <ty> , trait Magic <ty> where {Copy(^ty0_0)}], [impl <ty> Magic(^ty0_0) where {Magic(^ty0_0)}, impl Copy(u32)], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                              failed at (src/file.rs:LL:CC) because
                                judgment `prove_wc_list { goal: {Magic(!ty_0)}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                    judgment `prove_wc { goal: Magic(!ty_0), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                      the rule "positive impl" failed at step #7 (src/file.rs:LL:CC) because
                                        judgment `prove_after { constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness }, known_true: true, substitution: {?ty_1 => !ty_0} }, goal: {Copy(?ty_1)}, assumptions: {} }` failed at the following rule(s):
                                          the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                                            judgment `prove { goal: {Copy(!ty_0)}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness }, decls: decls(222, [trait Copy <ty> , trait Magic <ty> where {Copy(^ty0_0)}], [impl <ty> Magic(^ty0_0) where {Magic(^ty0_0)}, impl Copy(u32)], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                                              failed at (src/file.rs:LL:CC) because
                                                judgment `prove_wc_list { goal: {Copy(!ty_0)}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                                    judgment `prove_wc { goal: Copy(!ty_0), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                                      the rule "positive impl" failed at step #5 (src/file.rs:LL:CC) because
                                                        judgment `prove { goal: {!ty_0 = u32}, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness }, decls: decls(222, [trait Copy <ty> , trait Magic <ty> where {Copy(^ty0_0)}], [impl <ty> Magic(^ty0_0) where {Magic(^ty0_0)}, impl Copy(u32)], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                                                          failed at (src/file.rs:LL:CC) because
                                                            judgment `prove_wc_list { goal: {!ty_0 = u32}, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                                              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                                                judgment `prove_wc { goal: !ty_0 = u32, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                  the rule "assumption - relation" failed at step #1 (src/file.rs:LL:CC) because
                                                                    judgment had no applicable rules: `prove_via { goal: !ty_0 = u32, via: Copy(!ty_0), assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }`
                                                                  the rule "eq" failed at step #0 (src/file.rs:LL:CC) because
                                                                    judgment `prove_eq { a: !ty_0, b: u32, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                      the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                                                                        judgment `prove_normalize { p: !ty_0, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                          the rule "normalize-via-assumption" failed at step #1 (src/file.rs:LL:CC) because
                                                                            judgment had no applicable rules: `prove_normalize_via { goal: !ty_0, via: Copy(!ty_0), assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }`
                                                                      the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                                                        judgment `prove_eq { a: u32, b: !ty_0, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                          the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                                                                            judgment `prove_normalize { p: u32, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                                                              the rule "normalize-via-assumption" failed at step #1 (src/file.rs:LL:CC) because
                                                                                judgment had no applicable rules: `prove_normalize_via { goal: u32, via: Copy(!ty_0), assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }`
                                                                          the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                                                            cyclic proof attempt: `prove_eq { a: !ty_0, b: u32, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness } }`
                                                      the rule "trait implied bound" failed at step #4 (src/file.rs:LL:CC) because
                                                        judgment `prove_after { constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness }, known_true: true, substitution: {?ty_1 => !ty_0} }, goal: {Magic(?ty_1)}, assumptions: {} }` failed at the following rule(s):
                                                          the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                                                            judgment `prove { goal: {Magic(!ty_0)}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness }, decls: decls(222, [trait Copy <ty> , trait Magic <ty> where {Copy(^ty0_0)}], [impl <ty> Magic(^ty0_0) where {Magic(^ty0_0)}, impl Copy(u32)], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                                                              failed at (src/file.rs:LL:CC) because
                                                                cyclic proof attempt: `prove_wc_list { goal: {Magic(!ty_0)}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }`
                                      the rule "trait implied bound" failed at step #3 (src/file.rs:LL:CC) because
                                        judgment had no applicable rules: `prove_via { goal: Magic(!ty_0), via: Copy(?ty_1), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness } }`"#]]);
}
