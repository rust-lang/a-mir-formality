use formality_types::{
    cast::{Downcast, DowncastFrom},
    derive_links::DowncastTo,
    grammar::{
        AliasTy, AtomicRelation, InferenceVar, Parameter, RigidTy, TyData, Variable, Wc, WcData,
        Wcs,
    },
    judgment_fn,
};

use crate::{
    program::{AliasEqDeclBoundData, Program},
    prove::{
        env::Env,
        prove,
        prove_after::prove_after,
        prove_eq::{all_eq, prove_existential_var_eq},
        zip::zip,
    },
};

use super::constraints::Constraints;

judgment_fn! {
    pub fn prove_normalize(
        program: Program,
        env: Env,
        assumptions: Wcs,
        p: Parameter,
    ) => (Constraints, Parameter) {
        debug(p, assumptions, env, program)

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
            (prove(&program, env, &assumptions, all_eq(&a.parameters, &parameters)) => c)
            (prove_after(&program, c, &assumptions, &where_clause) => c)
            (let ty = c.substitution().apply(&ty))
            (let c = c.pop_subst(&subst))
            (assert c.env().encloses(&ty))
            ----------------------------- ("normalize-via-impl")
            (prove_normalize(program, env, assumptions, TyData::AliasTy(a)) => (c, ty))
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
    ) => (Constraints, Parameter) {
        debug(goal, via, assumptions, env, program)

        // The following 2 rules handle normalization of inference variables. We look specifically for
        // the case of a assumption `?X = Y`, which lets us normalize `?X` to `Y`, and ignore
        // everything else. In principle, we could allow the more general normalization rules
        // below handle this case too, but that generates a LOT of false paths, and I *believe*
        // it is unnecessary (FIXME: prove this).

        (
            (if let Some(Variable::InferenceVar(v_a)) = a.downcast())
            (if v_goal == v_a)
            ----------------------------- ("var-axiom-l")
            (prove_normalize_via(_program, env, _assumptions, AtomicRelation::Equals(a, b), Variable::InferenceVar(v_goal)) => (Constraints::none(env), b))
        )

        (
            (if let Some(Variable::InferenceVar(v_a)) = a.downcast())
            (if v_goal == v_a)
            ----------------------------- ("var-axiom-r")
            (prove_normalize_via(_program, env, _assumptions, AtomicRelation::Equals(b, a), Variable::InferenceVar(v_goal)) => (Constraints::none(env), b))
        )

        // The following 2 rules handle normalization of a type `X` given an assumption `X = Y`.
        // We can't just check for `goal == a` though because we sometimes need to bind inference
        // variables. Consider normalizing `R<?X>` given an assumption `R<u32> = Y`: this can be
        // normalized to `Y` given the constraint `?X = u32`.
        //
        // We don't use these rules to normalize an inference variable `?X` because such a goal
        // could be equated to everything, and thus generates a ton of spurious paths.

        (
            (if let None = goal.downcast::<InferenceVar>())
            (if goal != b)
            (prove_syntactically_eq(program, env, assumptions, a, goal) => c)
            (let b = c.substitution().apply(&b))
            ----------------------------- ("axiom-l")
            (prove_normalize_via(program, env, assumptions, AtomicRelation::Equals(a, b), goal) => (c, b))
        )

        (
            (if let None = goal.downcast::<InferenceVar>())
            (if goal != b)
            (prove_syntactically_eq(program, env, assumptions, a, goal) => c)
            (let b = c.substitution().apply(&b))
            ----------------------------- ("axiom-r")
            (prove_normalize_via(program, env, assumptions, AtomicRelation::Equals(b, a), goal) => (c, b))
        )

        // These rules handle the the ∀ and ⇒ cases.

        (
            (let (env, subst) = env.existential_substitution(&binder))
            (let via1 = binder.instantiate_with(&subst).unwrap())
            (prove_normalize_via(program, env, assumptions, via1, goal) => (c, p))
            (let c = c.pop_subst(&subst))
            (assert c.env().encloses(&p))
            ----------------------------- ("forall")
            (prove_normalize_via(program, env, assumptions, WcData::ForAll(binder), goal) => (c, p))
        )

        (
            (prove_normalize_via(&program, &env, &assumptions, &wc_consequence, goal) => (c, p))
            (prove_after(&program, c, &assumptions, &wc_condition) => c)
            (let p = c.substitution().apply(&p))
            ----------------------------- ("implies")
            (prove_normalize_via(program, env, assumptions, WcData::Implies(wc_condition, wc_consequence), goal) => (c, p))
        )
    }
}

judgment_fn! {
    fn prove_syntactically_eq(
        program: Program,
        env: Env,
        assumptions: Wcs,
        a: Parameter,
        b: Parameter,
    ) => Constraints {
        debug(a, b, assumptions, env, program)

        trivial(a == b => Constraints::none(env))

        (
            (prove_syntactically_eq(program, env, assumptions, b, a) => c)
            ----------------------------- ("symmetric")
            (prove_syntactically_eq(program, env, assumptions, a, b) => c)
        )

        (
            (let RigidTy { name: a_name, parameters: a_parameters } = a)
            (let RigidTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)
            (zip(&program, &env, &assumptions, a_parameters, b_parameters, &prove_syntactically_eq) => c)
            ----------------------------- ("rigid")
            (prove_syntactically_eq(program, env, assumptions, TyData::RigidTy(a), TyData::RigidTy(b)) => c)
        )

        (
            (let AliasTy { name: a_name, parameters: a_parameters } = a)
            (let AliasTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)
            (zip(&program, &env, &assumptions, a_parameters, b_parameters, &prove_syntactically_eq) => c)
            ----------------------------- ("alias")
            (prove_syntactically_eq(program, env, assumptions, TyData::AliasTy(a), TyData::AliasTy(b)) => c)
        )

        (
            (prove_existential_var_eq(program, env, assumptions, v, t) => c)
            ----------------------------- ("existential-nonvar")
            (prove_syntactically_eq(program, env, assumptions, Variable::InferenceVar(v), t) => c)
        )
    }
}
