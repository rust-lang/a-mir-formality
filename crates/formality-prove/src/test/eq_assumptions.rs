use expect_test::expect;
use formality_macros::test;
use formality_types::parse::term;

use crate::decls::Decls;

use super::test_prove;

#[test]
fn test_a() {
    let constraints = test_prove(
        Decls::empty(),
        term("{} => {for<ty T, ty U> if {T = u32, U = Vec<T>} U = Vec<u32>}"),
    );
    expect![[r#"
        {
            Constraints {
                env: Env {
                    variables: [],
                    coherence_mode: false,
                },
                known_true: true,
                substitution: {},
            },
        }
    "#]]
    .assert_debug_eq(&constraints);
}

#[test]
fn test_b() {
    let constraints = test_prove(
        Decls::empty(),
        term("exists<ty A> {} => {for<ty T, ty U> if {T = u32, U = Vec<T>} A = U}"),
    );
    expect![[r#"
        {
            Constraints {
                env: Env {
                    variables: [
                        ?ty_2,
                        ?ty_1,
                    ],
                    coherence_mode: false,
                },
                known_true: true,
                substitution: {
                    ?ty_1 => (rigid (adt Vec) (rigid (scalar u32))),
                    ?ty_2 => (rigid (scalar u32)),
                },
            },
        }
    "#]]
    .assert_debug_eq(&constraints);
}

#[test]
fn test_normalize_assoc_ty() {
    let constraints = test_prove(
        Decls::empty(),
        term("{} => {for<ty T> if { <T as Iterator>::Item = u32 } <T as Iterator>::Item = u32}"),
    );
    expect![[r#"
        {
            Constraints {
                env: Env {
                    variables: [],
                    coherence_mode: false,
                },
                known_true: true,
                substitution: {},
            },
        }
    "#]]
    .assert_debug_eq(&constraints);
}

#[test]
fn test_normalize_assoc_ty_inference0() {
    let constraints = test_prove(
        Decls::empty(),
        term("exists<ty A> {} => {for<ty T> if { <T as Iterator>::Item = u32 } <A as Iterator>::Item = u32}"),
    );
    expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&constraints);
}

#[test]
fn test_normalize_assoc_ty_inference1() {
    let constraints = test_prove(
        Decls::empty(),
        term(
            "\
            forall<ty T> \
            exists<ty A> \
            { <T as Iterator>::Item = u32 } => { <A as Iterator>::Item = u32 }",
        ),
    );
    expect![[r#"
        {
            Constraints {
                env: Env {
                    variables: [
                        !ty_1,
                        ?ty_2,
                    ],
                    coherence_mode: false,
                },
                known_true: true,
                substitution: {
                    ?ty_2 => !ty_1,
                },
            },
        }
    "#]]
    .assert_debug_eq(&constraints);
}
