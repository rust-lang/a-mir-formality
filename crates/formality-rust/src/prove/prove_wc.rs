use crate::grammar::{Predicate, Wc, Wcs};
use formality_core::judgment_fn;

use crate::prove::{
    combinators::for_all,
    decls::Program,
    env::{Bias, Env},
    is_local::{is_local_trait_ref, may_be_remote},
    prove,
    prove_after::prove_after,
    prove_const_has_type::prove_const_has_type,
    prove_eq::prove_eq,
    prove_outlives::prove_outlives,
    prove_sub::prove_sub,
    prove_via::prove_via,
    prove_wf::prove_wf,
};

use super::constraints::Constraints;

judgment_fn! {
    /// The "heart" of the trait system -- prove that a where-clause holds given a set of declarations, variable environment, and set of assumptions.
    /// If successful, returns the constraints under which the where-clause holds.
    pub fn prove_wc(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        goal: Wc,
    ) => Constraints {
        debug(goal, assumptions, env)

        trivial(assumptions.iter().any(|assumption| assumption == goal) => Constraints::none(env))

        (
            (let (env, subst) = env.universal_substitution(binder))
            (let p1 = binder.instantiate_with(&subst).unwrap())
            (prove_wc(decls, env, assumptions, p1) => c)
            --- ("forall")
            (prove_wc(decls, env, assumptions, Wc::ForAll(binder)) => c.pop_forall(&subst))
        )

        (
            (let (env, subst) = env.existential_substitution(binder))
            (let goals = binder.instantiate_with(&subst).unwrap())
            (prove(decls, env, assumptions, goals) => c)
            --- ("exists")
            (prove_wc(decls, env, assumptions, Wc::Exists(binder)) => c.pop_subst(&subst))
        )

        (
            (prove_wc(decls, env, (assumptions, p1), p2) => c)
            --- ("implies")
            (prove_wc(decls, env, assumptions, Wc::Implies(p1, p2)) => c)
        )

        (
            (a in assumptions)!
            (prove_via(decls, env, assumptions, a, goal) => c)
            ----------------------------- ("assumption")
            (prove_wc(decls, env, assumptions, Wc::Predicate(goal)) => c)
        )


        // This rule is: prove `T: Foo<U>` holds on the basis of an `impl<A,B> Foo<B> for A where WC` impl somewhere.
        (
            // Get the impl declaration.
            (i in decls.impl_decls(&trait_ref.trait_id))!

            // Instantiate impl generics with inference variables (in our example, `A => ?A, B => ?B`).
            (let (env, subst) = env.existential_substitution(&i.binder))
            (let i = i.binder.instantiate_with(&subst).unwrap())

            // Instantiate trait where-clauses from `Foo<?B>`. If we had `trait Foo<X: Debug>`, for example,
            // this would yield `?B: Debug`.
            (let t = decls.trait_decl(&i.trait_ref.trait_id).binder.instantiate_with(&i.trait_ref.parameters).unwrap())

            // The provisional trait closes coinductive impl cycles, but cannot supply
            // declaration bounds before the impl's premises have been established.
            //
            // NB: This is actually not what Rust currently does, but it is what "we" (types team) want it to do.
            (let co_assumptions = (assumptions, Wc::Coinductive(trait_ref.clone())))
            (prove(decls, env, co_assumptions, Wcs::all_eq(&trait_ref.parameters, &i.trait_ref.parameters)) => c)
            (prove_after(decls, c, co_assumptions, &i.where_clause) => c)

            // Prove that the well-formedness requirements of the *trait* hold -- for this proof, we cannot
            // assume that the trait is implemented, because that would allow specious implied bounds
            // (i.e., we could assume that `B: Debug` based on the trait definition + the existence of an impl,
            // but actually the impl is responsible for proving that `B: Debug`).
            (prove_after(decls, c, assumptions, &t.where_clause) => c)
            ----------------------------- ("positive impl")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => c.pop_subst(&subst))
        )

        (
            (if env.bias() == Bias::Completeness)!
            (may_be_remote(decls, env, assumptions, trait_ref) => c)
            ----------------------------- ("coherence / remote impl")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => c)
        )

        (
            (i in decls.neg_impl_decls(&trait_ref.trait_id))
            (let (env, subst) = env.existential_substitution(&i.binder))
            (let i = i.binder.instantiate_with(&subst).unwrap())
            (prove(decls, env, assumptions, Wcs::all_eq(&trait_ref.parameters, &i.trait_ref.parameters)) => c)
            (prove_after(decls, c, assumptions, &i.where_clause) => c)
            ----------------------------- ("negative impl")
            (prove_wc(decls, env, assumptions, Predicate::NotImplemented(trait_ref)) => c.pop_subst(&subst))
        )

        (
            (prove_eq(decls, env, assumptions, alias_ty, ty) => c)
            ----------------------------- ("alias eq")
            (prove_wc(decls, env, assumptions, Predicate::AliasEq(alias_ty, ty)) => c)
        )

        (
            (ti in decls.trait_invariants())
            (let (env, subst) = env.existential_substitution(&ti.binder))
            (let ti = ti.binder.instantiate_with(&subst).unwrap())
            (let inductive_assumptions = assumptions.without_coinductive())
            (prove_via(decls, env, inductive_assumptions, &ti.where_clause, trait_ref) => c)
            (prove_after(decls, c, inductive_assumptions, &ti.trait_ref) => c)
            ----------------------------- ("trait implied bound")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => c.pop_subst(&subst))
        )

        (
            (prove_eq(decls, env, assumptions, a, b) => c)
            ----------------------------- ("eq")
            (prove_wc(decls, env, assumptions, Predicate::Equals(a, b)) => c)
        )

        (
            (prove_sub(decls, env, assumptions, a, b) => c)
            ----------------------------- ("subtype")
            (prove_wc(decls, env, assumptions, Predicate::Sub(a, b)) => c)
        )

        (
            (for_all(decls, env, assumptions, &trait_ref.parameters, &prove_wf) => c)
            (let t = decls.trait_decl(&trait_ref.trait_id))
            (let t = t.binder.instantiate_with(&trait_ref.parameters).unwrap())
            (prove_after(decls, c, assumptions, &t.where_clause) => c)
            ----------------------------- ("trait well formed")
            (prove_wc(decls, env, assumptions, Predicate::WellFormedTraitRef(trait_ref)) => c)
        )

        (
            (is_local_trait_ref(decls, env, assumptions, trait_ref) => c)
            ----------------------------- ("trait ref is local")
            (prove_wc(decls, env, assumptions, Predicate::IsLocal(trait_ref)) => c)
        )

        (
            (prove_outlives(decls, env, assumptions, a, b) => c)
            ----------------------------- ("outlives")
            (prove_wc(decls, env, assumptions, Predicate::Outlives(a, b)) => c)
        )


        (
            (prove_wf(decls, env, assumptions, p) => c)
            ----------------------------- ("parameter well formed")
            (prove_wc(decls, env, assumptions, Predicate::WellFormed(p)) => c)
        )

        (
            (prove_const_has_type(decls, env, assumptions, constant) => (ty_constant, c))
            (prove_after(decls, c, assumptions, Predicate::equals(ty_constant, ty)) => c)
            ----------------------------- ("const has ty")
            (prove_wc(decls, env, assumptions, Predicate::ConstHasType(constant, ty)) => c)
        )
    }
}
