use expect_test::expect;
use formality_macros::test;
use formality_types::{
    grammar::{Binder, Parameter},
    parse::term,
};

use crate::prove::Env;

use super::minimize;

#[test]
fn minimize_a() {
    let env = Env::default();
    let term: Binder<Vec<Parameter>> = term("<ty A, ty B, ty C> [A, C]");
    let (env, subst) = env.existential_substitution(&term);
    let term = term.instantiate_with(&subst).unwrap();

    expect![[r#"
        (
            Env {
                variables: [
                    ?ty_1,
                    ?ty_2,
                    ?ty_3,
                ],
            },
            [
                ?ty_1,
                ?ty_3,
            ],
        )
    "#]]
    .assert_debug_eq(&(&env, &term));

    let (env_min, term_min, m) = minimize(env, term);

    expect![[r#"
        (
            Env {
                variables: [
                    ?ty_0,
                    ?ty_1,
                ],
            },
            [
                ?ty_0,
                ?ty_1,
            ],
        )
    "#]]
    .assert_debug_eq(&(&env_min, &term_min));

    let r = m.reconstitute(env_min, term_min);

    expect![[r#"
        (
            Env {
                variables: [
                    ?ty_1,
                    ?ty_2,
                    ?ty_3,
                ],
            },
            [
                ?ty_1,
                ?ty_3,
            ],
        )
    "#]].assert_debug_eq(&r);
}
