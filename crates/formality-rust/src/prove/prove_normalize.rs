use crate::{
    grammar::{AliasTy, ExistentialVar, Parameter, Predicate, RigidTy, Ty, Variable, Wc, Wcs},
    prove::Constrained,
};
use formality_core::{judgment_fn, Downcast};

use crate::prove::{
    binder_scope::enter_existentially,
    combinators::zip,
    decls::{AliasEqDeclBoundData, Program},
    env::Env,
    prove,
    prove_after::prove_after,
    prove_eq::prove_existential_var_eq,
};

use super::constraints::Constraints;

judgment_fn! {
    /// Normalize `p` one step, returning a set of constraints and a new parameter `q` that is
    /// semantically equivalent to `p`. e.g., if p is `<Vec<T> as IntoIterator>::Item`, this would
    /// return `T`.
    pub fn prove_normalize(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        p: Parameter,
    ) => Constrained<Parameter> {
        debug(p, assumptions, env)

        (
            (a in assumptions)!
            (prove_normalize_via(decls, env, assumptions, a, goal) => c)
            ----------------------------- ("normalize-via-assumption")
            (prove_normalize(decls, env, assumptions, goal) => c)
        )

        (
            (decl in decls.alias_eq_decls(&a.name))
            (scope(enter_existentially(decls, env, assumptions, &decl.binder) => (env, decl)) with(ty, c)
                (let AliasEqDeclBoundData { alias: AliasTy { name, parameters }, ty, where_clause } = decl)
                (assert a.name == *name)
                (prove(decls, env, assumptions, Wcs::all_eq(&a.parameters, &parameters)) => c)
                (prove_after(decls, c, assumptions, &where_clause) => c)
                (let ty = c.substitution().apply(ty)))
            ----------------------------- ("normalize-via-impl")
            (prove_normalize(decls, env, assumptions, Ty::AliasTy(a)) => Constrained(ty, c))
        )
    }
}

judgment_fn! {
    fn prove_normalize_via(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        via: Wc,
        goal: Parameter,
    ) => Constrained<Parameter> {
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
            (prove_normalize_via(_decls, env, _assumptions, Predicate::Equals(a, b), Variable::ExistentialVar(v_goal)) => Constrained::none(env, b))
        )

        (
            (if let Some(Variable::ExistentialVar(v_a)) = a.downcast())
            (if v_goal == v_a)!
            ----------------------------- ("var-axiom-r")
            (prove_normalize_via(_decls, env, _assumptions, Predicate::Equals(b, a), Variable::ExistentialVar(v_goal)) => Constrained::none(env, b))
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
            (let b = c.substitution().apply(b))
            ----------------------------- ("axiom-l")
            (prove_normalize_via(decls, env, assumptions, Predicate::Equals(a, b), goal) => Constrained(b, c))
        )

        (
            (if let None = goal.downcast::<ExistentialVar>())
            (if goal != b)!
            (prove_syntactically_eq(decls, env, assumptions, a, goal) => c)
            (let b = c.substitution().apply(b))
            ----------------------------- ("axiom-r")
            (prove_normalize_via(decls, env, assumptions, Predicate::Equals(b, a), goal) => Constrained(b, c))
        )

        // These rules handle the the ∀ and ⇒ cases.

        (
            (scope(enter_existentially(decls, env, assumptions, binder) => (env, via1)) with(p, c)
                (prove_normalize_via(decls, env, assumptions, via1, goal) => Constrained(p, c)))
            ----------------------------- ("forall")
            (prove_normalize_via(decls, env, assumptions, Wc::ForAll(binder), goal) => Constrained(p, c))
        )

        (
            (prove_normalize_via(decls, env, assumptions, wc_consequence, goal) => Constrained(p, c))
            (prove_after(decls, c, assumptions, wc_condition) => c)
            (let p = c.substitution().apply(p))
            ----------------------------- ("implies")
            (prove_normalize_via(decls, env, assumptions, Wc::Implies(wc_condition, wc_consequence), goal) => Constrained(p, c))
        )
    }
}

judgment_fn! {
    fn prove_syntactically_eq(
        _decls: Program,
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
            (zip(decls, env, assumptions, a_parameters.clone(), b_parameters.clone(), &prove_syntactically_eq) => c)
            ----------------------------- ("rigid")
            (prove_syntactically_eq(decls, env, assumptions, Ty::RigidTy(a), Ty::RigidTy(b)) => c)
        )

        (
            (let AliasTy { name: a_name, parameters: a_parameters } = a)
            (let AliasTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)!
            (zip(decls, env, assumptions, a_parameters.clone(), b_parameters.clone(), &prove_syntactically_eq) => c)
            ----------------------------- ("alias")
            (prove_syntactically_eq(decls, env, assumptions, Ty::AliasTy(a), Ty::AliasTy(b)) => c)
        )

        (
            (prove_existential_var_eq(decls, env, assumptions, v, t) => c)
            ----------------------------- ("existential-nonvar")
            (prove_syntactically_eq(decls, env, assumptions, Variable::ExistentialVar(v), t) => c)
        )
    }
}
