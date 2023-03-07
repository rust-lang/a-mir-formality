use formality_types::{
    grammar::{AliasTy, AtomicRelation, Parameter, TyData, Wc, WcData, Wcs},
    judgment_fn,
};

use crate::{
    program::{AliasEqDeclBoundData, Program},
    prove::{
        constraints::no_constraints, env::Env, prove, prove_after::prove_after, prove_eq::all_eq,
    },
};

use super::constraints::Constraints;

judgment_fn! {
    pub fn prove_normalize(
        program: Program,
        env: Env,
        assumptions: Wcs,
        p: Parameter,
    ) => (Env, Parameter, Constraints) {
        (
            (&assumptions => a)
            (prove_normalize_via(&program, &env, &assumptions, a, &goal) => c)
            ----------------------------- ("normalize-via-assumption")
            (prove_normalize(program, env, assumptions, goal) => c)
        )

        (
            (program.alias_eq_decls(&a.name) => decl)
            (let (env, subst) = env.existential_substitution(&decl.binder))
            (let decl = decl.binder.instantiate_with(&subst).unwrap())
            (let AliasEqDeclBoundData { alias: AliasTy { name, parameters }, ty, where_clause } = decl)
            (assert a.name == name)
            (prove(&program, env, &assumptions, all_eq(&a.parameters, &parameters)) => (env, c))
            (prove_after(&program, env, c, &assumptions, &where_clause) => (env, c))
            (let ty = c.substitution().apply(&ty))
            (let (env, c) = c.pop_subst(env, &subst))
            (assert () == env.assert_encloses(&ty))
            ----------------------------- ("normalize-via-impl")
            (prove_normalize(program, env, assumptions, TyData::AliasTy(a)) => (env, ty, c))
        )
    }
}

judgment_fn! {
    fn prove_normalize_via(
        program: Program,
        env: Env,
        assumptions: Wcs,
        via: Wc,
        goal: Parameter,
    ) => (Env, Parameter, Constraints) {
        (
            (if goal == a && goal != b)
            ----------------------------- ("axiom-l")
            (prove_normalize_via(_program, env, _assumptions, AtomicRelation::Equals(a, b), goal) => (env, b, no_constraints()))
        )

        (
            (if goal != a && goal == b)
            ----------------------------- ("axiom-r")
            (prove_normalize_via(_program, env, _assumptions, AtomicRelation::Equals(a, b), goal) => (env, a, no_constraints()))
        )

        (
            (let (env, subst) = env.existential_substitution(&binder))
            (let via1 = binder.instantiate_with(&subst).unwrap())
            (prove_normalize_via(program, env, assumptions, via1, goal) => (env, p, c))
            (let (env, c) = c.pop_subst(env, &subst))
            (assert () == env.assert_encloses(&p))
            ----------------------------- ("forall")
            (prove_normalize_via(program, env, assumptions, WcData::ForAll(binder), goal) => (env, p, c))
        )

        (
            (prove_normalize_via(&program, &env, &assumptions, &wc_consequence, goal) => (env, p, c))
            (prove_after(&program, &env, c, &assumptions, &wc_condition) => (env, c))
            (let p = c.substitution().apply(&p))
            ----------------------------- ("implies")
            (prove_normalize_via(program, env, assumptions, WcData::Implies(wc_condition, wc_consequence), goal) => (env, p, c))
        )
    }
}
