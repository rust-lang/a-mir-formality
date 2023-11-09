use expect_test::expect;
use formality_core::To;
use formality_macros::test;
use formality_types::{
    grammar::{Binder, Parameter, ScalarId, Ty},
    rust::term,
};

use crate::prove::{Constraints, Env};

use super::minimize;

#[test]
fn minimize_a() {
    let env = Env::default();
    let term: Binder<Vec<Parameter>> = term("<ty A, ty B, ty C> [A, C]");
    let (env, subst) = env.existential_substitution(&term);
    let term = term.instantiate_with(&subst).unwrap();

    expect!["(Env { variables: [?ty_1, ?ty_2, ?ty_3], coherence_mode: false }, [?ty_1, ?ty_3])"]
        .assert_eq(&format!("{:?}", (&env, &term)));

    let (mut env_min, term_min, m) = minimize(env, term);

    expect!["(Env { variables: [?ty_0, ?ty_1], coherence_mode: false }, [?ty_0, ?ty_1])"]
    .assert_eq(&format!("{:?}", (&env_min, &term_min)));

    let ty0 = term_min[0].as_variable().unwrap();
    let ty1 = term_min[1].as_variable().unwrap();
    let ty2 = env_min.insert_fresh_before(
        formality_types::grammar::ParameterKind::Ty,
        env_min.universe(ty1),
    );

    let c_min = Constraints {
        env: env_min,
        known_true: true,
        substitution: vec![(ty1, ty2.to::<Ty>()), (ty0, ScalarId::U32.to::<Ty>())]
            .into_iter()
            .collect(),
    };
    let c = m.reconstitute(c_min);

    expect![[r#"
        Constraints {
            env: Env {
                variables: [
                    ?ty_1,
                    ?ty_2,
                    ?ty_4,
                    ?ty_3,
                ],
                coherence_mode: false,
            },
            known_true: true,
            substitution: {
                ?ty_1 => u32,
                ?ty_3 => ?ty_4,
            },
        }
    "#]]
    .assert_debug_eq(&c);
}
