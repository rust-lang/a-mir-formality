use expect_test::expect;
use formality_macros::test;
use formality_types::rust::term;

use crate::decls::Decls;

use crate::test_util::test_prove;

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Decls {
    Decls {
        trait_decls: vec![term("trait Foo<ty Self> where {}")],
        impl_decls: vec![term("impl<ty T> Foo(Vec<T>) where {}")],
        ..Decls::empty()
    }
}

/// Test that `X = Vec<X>` cannot be solved
#[test]
fn direct_cycle() {
    let constraints = test_prove(decls(), term("exists<ty A> {} => {A = Vec<A>}"));
    expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&constraints);
}

/// Test that `X = Vec<Y>` can be solved
#[test]
fn eq_variable_to_rigid() {
    let constraints = test_prove(decls(), term("exists<ty X, ty Y> {} => {X = Vec<Y>}"));
    expect![[r#"
        {
            Constraints {
                env: Env {
                    variables: [
                        ?ty_3,
                        ?ty_1,
                        ?ty_2,
                    ],
                    coherence_mode: false,
                },
                known_true: true,
                substitution: {
                    ?ty_1 => Vec<?ty_3>,
                    ?ty_2 => ?ty_3,
                },
            },
        }
    "#]]
    .assert_debug_eq(&constraints);
}

/// Test that `Vec<Y> = X` can be solved
#[test]
fn eq_rigid_to_variable() {
    let constraints = test_prove(decls(), term("exists<ty X, ty Y> {} => {Vec<Y> = X}"));
    expect![[r#"
        {
            Constraints {
                env: Env {
                    variables: [
                        ?ty_3,
                        ?ty_1,
                        ?ty_2,
                    ],
                    coherence_mode: false,
                },
                known_true: true,
                substitution: {
                    ?ty_1 => Vec<?ty_3>,
                    ?ty_2 => ?ty_3,
                },
            },
        }
    "#]]
    .assert_debug_eq(&constraints);
}

/// Test that `X = Vec<X>` cannot be solved (when constructed over several steps)
#[test]
fn indirect_cycle_1() {
    let constraints = test_prove(
        decls(),
        term("exists<ty A, ty B> {} => {A = Vec<B>, B = A}"),
    );
    expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&constraints);
}

/// Test that `X = Vec<X>` cannot be solved (when constructed over several steps)
#[test]
fn indirect_cycle_2() {
    let constraints = test_prove(
        decls(),
        term("exists<ty A, ty B> {} => {B = A, A = Vec<B>}"),
    );
    expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&constraints);
}
