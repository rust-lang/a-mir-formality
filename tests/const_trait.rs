#![allow(non_snake_case)] // we embed type names into the names for our test functions

use a_mir_formality::{test_program_ok, test_where_clause};
use formality_core::test_util::ResultTestExt;
use formality_macros::test;

#[test]
fn test_const_syntax() {
    let gen_program = |addl: &str| {
        const BASE_PROGRAM: &str = "[
        crate core {
           const trait Default {
           }

           impl const Default for () {
           }
        }

        ]";

        BASE_PROGRAM.replace("ADDITIONAL", addl)
    };

    test_program_ok(&gen_program("")).assert_ok(expect_test::expect!["()"]);
}

#[test]
fn test_runtime_fn_with_runtime_effect() {
    let BASE_PROGRAM: &str = "[
        crate test {
            fn foo() -> () random_keyword do runtime {(runtime)}
        }
    ]";
    test_where_clause(
        BASE_PROGRAM,
        "{} => {}",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }}"]);
}

#[test]
fn test_const_fn_with_const_effect() {
    let BASE_PROGRAM: &str = "[
        crate test {
            fn foo() -> () random_keyword do const {(const)}
        }
    ]";
    test_where_clause(
        BASE_PROGRAM,
        "{} => {}",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }}"]);
}

#[test]
fn test_runtime_fn_with_const_effect() {
    let BASE_PROGRAM: &str = "[
        crate test {
            fn foo() -> () random_keyword do runtime {(const)}
        }
    ]";
    test_where_clause(
        BASE_PROGRAM,
        "{} => {}",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }}"]);
}

#[test]
fn test_const_fn_with_runtime_effect() {
    let BASE_PROGRAM: &str = "[
        crate test {
            fn foo() -> () random_keyword do const {(runtime)}
        }
    ]";
    test_where_clause(
        BASE_PROGRAM,
        "{} => {}",
    )
    .assert_err(expect_test::expect![[r#"
        judgment `prove { goal: {@ subset(runtime , const)}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
          failed at (src/file.rs:LL:CC) because
            judgment `prove_wc_list { goal: {@ subset(runtime , const)}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                judgment `prove_wc { goal: @ subset(runtime , const), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                  the rule "effect subset" failed at step #0 (src/file.rs:LL:CC) because
                    judgment `prove_effect_subset { subset: runtime, superset: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                      the rule "atomic" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_atomic_effect_subset { atomic_subeffect: runtime, superset: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                          the rule "transitive" failed at step #0 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_effect_upper_bound { f1: runtime, assumptions: {}, env: Env { variables: [], bias: Soundness } }`
                          the rule "union-subset-lhs" failed at step #1 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_atomic_effect_eq { f1: runtime, f2: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }`"#]]);
  }


// FIXME(tiif): The trait ref syntax is a little bit confusing for now, 
// `const Foo()` is the syntax for a trait ref named Foo with const effect
// `Foo()` is the syntax for a trait ref named Foo with runtime effect. 


/// Somewhat equivalent to 
/// ```
/// fn foo<T>()
/// where 
///   T: Default<do ⊆ const>,
/// do
///   <T as Default> 
/// {
///     <T as Default>::default()
/// }
/// ````
#[test]
fn test_associated_effect_reflexive() {
    // FIXME (tiif): we should find a way to define the trait, and only using the TraitId instead of creating 
    // Default trait ref twice....
    let BASE_PROGRAM: &str = "[
        crate test {
          fn foo() -> () random_keyword do AssociatedEffect(Default()) {(AssociatedEffect(Default()))}
        }
    ]";
    test_where_clause(
        BASE_PROGRAM,
        "{} => {}",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }}"]);
  }


/// Somewhat equivalent to 
/// ```
/// fn foo<T>()
/// where 
///   T: Default<do ⊆ const>,
/// do
///   const 
/// {
///     <T as Default>::default()
/// }
/// ````
#[test]
fn test_const_associated_effect_in_const_fn() {
    let BASE_PROGRAM: &str = "[
        crate test {
          fn foo() -> () random_keyword do const {(AssociatedEffect(const Default()))}
        }
    ]";
    test_where_clause(
        BASE_PROGRAM,
        "{} => {}",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }}"]);
  }


/// Somewhat equivalent to 
/// ```
/// fn foo<T>()
/// where 
///   T: Default<do ⊆ const>,
/// do
///   runtime
/// {
///     <T as Default>::default()
/// }
/// ````
#[test]
fn test_const_associated_effect_in_runtime_fn() {
    let BASE_PROGRAM: &str = "[
        crate test {
          fn foo() -> () random_keyword do runtime {(AssociatedEffect(const Default()))}
        }
    ]";
    test_where_clause(
        BASE_PROGRAM,
        "{} => {}",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }}"]);
  }

/// Somewhat equivalent to 
/// ```
/// fn foo<T>()
/// where 
///   T: Default<do ⊆ runtime>,
/// do
///   const 
/// {
///     <T as Default>::default()
/// }
/// ````
#[test]
fn test_runtime_associated_effect_in_const_fn() {
    let BASE_PROGRAM: &str = "[
        crate test {
          fn foo() -> () random_keyword do const {(AssociatedEffect(Default()))}
        }
    ]";
    test_where_clause(
        BASE_PROGRAM,
        "{} => {}",
    )
    .assert_err(expect_test::expect![[r#"
        judgment `prove { goal: {@ subset(AssociatedEffect(Default()) , const)}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
          failed at (src/file.rs:LL:CC) because
            judgment `prove_wc_list { goal: {@ subset(AssociatedEffect(Default()) , const)}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                judgment `prove_wc { goal: @ subset(AssociatedEffect(Default()) , const), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                  the rule "effect subset" failed at step #0 (src/file.rs:LL:CC) because
                    judgment `prove_effect_subset { subset: AssociatedEffect(Default()), superset: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                      the rule "associated effect" failed at step #1 (src/file.rs:LL:CC) because
                        judgment `prove_effect_subset { subset: runtime, superset: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                          the rule "atomic" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `prove_atomic_effect_subset { atomic_subeffect: runtime, superset: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                              the rule "transitive" failed at step #0 (src/file.rs:LL:CC) because
                                judgment had no applicable rules: `prove_effect_upper_bound { f1: runtime, assumptions: {}, env: Env { variables: [], bias: Soundness } }`
                              the rule "union-subset-lhs" failed at step #1 (src/file.rs:LL:CC) because
                                judgment had no applicable rules: `prove_atomic_effect_eq { f1: runtime, f2: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }`
                      the rule "atomic" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_atomic_effect_subset { atomic_subeffect: AssociatedEffect(Default()), superset: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                          the rule "transitive" failed at step #0 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_effect_upper_bound { f1: AssociatedEffect(Default()), assumptions: {}, env: Env { variables: [], bias: Soundness } }`
                          the rule "union-subset-lhs" failed at step #1 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_atomic_effect_eq { f1: AssociatedEffect(Default()), f2: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }`"#]]);
 }

