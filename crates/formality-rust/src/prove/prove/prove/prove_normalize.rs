use formality_core::{judgment_fn, Downcast};
use crate::types::grammar::{
    AliasTy, ExistentialVar, Parameter, Relation, RigidTy, TyData, Variable, Wc, WcData, Wcs,
};

use crate::prove::prove::{
    decls::{AliasEqDeclBoundData, Decls},
    prove::{
        combinators::zip, env::Env, prove, prove_after::prove_after,
        prove_eq::prove_existential_var_eq,
    },
};

use super::constraints::Constraints;

judgment_fn! {
    /// Normalize `p` one step, returning a set of constraints and a new parameter `q` that is
    /// semantically equivalent to `p`. e.g., if p is `<Vec<T> as IntoIterator>::Item`, this would
    /// return `T`.
    pub fn prove_normalize(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        p: Parameter,
    ) => (Constraints, Parameter) {
        debug(p, assumptions, env)

        (
            (a in &assumptions)!
            (prove_normalize_via(&decls, &env, &assumptions, a, &goal) => c)
            ----------------------------- ("normalize-via-assumption")
            (prove_normalize(decls, env, assumptions, goal) => c)
        )

        (
            (decl in decls.alias_eq_decls(&a.name))
            (let (env, subst) = env.existential_substitution(&decl.binder))
            (let decl = decl.binder.instantiate_with(&subst).unwrap())
            (let AliasEqDeclBoundData { alias: AliasTy { name, parameters }, ty, where_clause } = decl)
            (assert a.name == name)
            (prove(&decls, env, &assumptions, Wcs::all_eq(&a.parameters, &parameters)) => c)
            (prove_after(&decls, c, &assumptions, &where_clause) => c)
            (let ty = c.substitution().apply(&ty))
            (let c = c.pop_subst(&subst))
            (assert c.env().encloses(&ty))
            ----------------------------- ("normalize-via-impl")
            (prove_normalize(decls, env, assumptions, TyData::AliasTy(a)) => (c, ty))
        )
    }
}

judgment_fn! {
    fn prove_normalize_via(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        via: Wc,
        goal: Parameter,
    ) => (Constraints, Parameter) {
        debug(goal, via, assumptions, env)

        // The following 2 rules handle normalization of existential variables. We look specifically for
        // the case of a assumption `?X = Y`, which lets us normalize `?X` to `Y`, and ignore
        // everything else. In principle, we could allow the more general normalization rules
        // below handle this case too, but that generates a LOT of false paths, and I *believe*
        // it is unnecessary

        (
            (if let Some(Variable::ExistentialVar(v_a)) = a.downcast())
            (if v_goal == v_a)!
            ----------------------------- ("var-axiom-l")
            (prove_normalize_via(_decls, env, _assumptions, Relation::Equals(a, b), Variable::ExistentialVar(v_goal)) => (Constraints::none(env), b))
        )

        (
            (if let Some(Variable::ExistentialVar(v_a)) = a.downcast())
            (if v_goal == v_a)!
            ----------------------------- ("var-axiom-r")
            (prove_normalize_via(_decls, env, _assumptions, Relation::Equals(b, a), Variable::ExistentialVar(v_goal)) => (Constraints::none(env), b))
        )

        // The following 2 rules handle normalization of a type `X` given an assumption `X = Y`.
        // We can't just check for `goal == a` though because we sometimes need to bind existential
        // variables. Consider normalizing `R<?X>` given an assumption `R<u32> = Y`: this can be
        // normalized to `Y` given the constraint `?X = u32`.
        //
        // We don't use these rules to normalize an existential variable `?X` because such a goal
        // could be equated to everything, and thus generates a ton of spurious paths.

        (
            (if let None = goal.downcast::<ExistentialVar>())
            (if goal != b)!
            (prove_syntactically_eq(decls, env, assumptions, a, goal) => c)
            (let b = c.substitution().apply(&b))
            ----------------------------- ("axiom-l")
            (prove_normalize_via(decls, env, assumptions, Relation::Equals(a, b), goal) => (c, b))
        )

        (
            (if let None = goal.downcast::<ExistentialVar>())
            (if goal != b)!
            (prove_syntactically_eq(decls, env, assumptions, a, goal) => c)
            (let b = c.substitution().apply(&b))
            ----------------------------- ("axiom-r")
            (prove_normalize_via(decls, env, assumptions, Relation::Equals(b, a), goal) => (c, b))
        )

        // These rules handle the the ∀ and ⇒ cases.

        (
            (let (env, subst) = env.existential_substitution(&binder))
            (let via1 = binder.instantiate_with(&subst).unwrap())
            (prove_normalize_via(decls, env, assumptions, via1, goal) => (c, p))
            (let c = c.pop_subst(&subst))
            (assert c.env().encloses(&p))
            ----------------------------- ("forall")
            (prove_normalize_via(decls, env, assumptions, WcData::ForAll(binder), goal) => (c, p))
        )

        (
            (prove_normalize_via(&decls, &env, &assumptions, &wc_consequence, goal) => (c, p))
            (prove_after(&decls, c, &assumptions, &wc_condition) => c)
            (let p = c.substitution().apply(&p))
            ----------------------------- ("implies")
            (prove_normalize_via(decls, env, assumptions, WcData::Implies(wc_condition, wc_consequence), goal) => (c, p))
        )
    }
}

judgment_fn! {
    fn prove_syntactically_eq(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        a: Parameter,
        b: Parameter,
    ) => Constraints {
        debug(a, b, assumptions, env)

        trivial(a == b => Constraints::none(env))

        (
            (prove_syntactically_eq(decls, env, assumptions, b, a) => c)
            ----------------------------- ("symmetric")
            (prove_syntactically_eq(decls, env, assumptions, a, b) => c)
        )

        (
            (let RigidTy { name: a_name, parameters: a_parameters } = a)
            (let RigidTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)!
            (zip(&decls, &env, &assumptions, a_parameters, b_parameters, &prove_syntactically_eq) => c)
            ----------------------------- ("rigid")
            (prove_syntactically_eq(decls, env, assumptions, TyData::RigidTy(a), TyData::RigidTy(b)) => c)
        )

        (
            (let AliasTy { name: a_name, parameters: a_parameters } = a)
            (let AliasTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)!
            (zip(&decls, &env, &assumptions, a_parameters, b_parameters, &prove_syntactically_eq) => c)
            ----------------------------- ("alias")
            (prove_syntactically_eq(decls, env, assumptions, TyData::AliasTy(a), TyData::AliasTy(b)) => c)
        )

        (
            (prove_existential_var_eq(decls, env, assumptions, v, t) => c)
            ----------------------------- ("existential-nonvar")
            (prove_syntactically_eq(decls, env, assumptions, Variable::ExistentialVar(v), t) => c)
        )
    }
}
