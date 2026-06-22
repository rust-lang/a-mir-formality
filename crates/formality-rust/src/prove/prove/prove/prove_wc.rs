use crate::grammar::{Predicate, Relation, Wc, WcData, Wcs};
use formality_core::judgment_fn;

use crate::prove::prove::{
    decls::Program,
    prove::{
        combinators::for_all,
        env::{Bias, Env},
        is_local::{is_local_trait_ref, may_be_remote},
        prove,
        prove_const_has_type::prove_const_has_type,
        prove_eq::prove_eq,
        prove_outlives::prove_outlives,
        prove_sub::prove_sub,
        prove_via::prove_via,
        prove_wf::prove_wf,
    },
};

judgment_fn! {
    /// The "heart" of the trait system -- prove that a where-clause holds given a set of declarations, variable environment, and set of assumptions.
    /// If successful, returns the constraints under which the where-clause holds.
    pub fn prove_wc(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        goal: Wc,
    ) => Env {
        debug(goal, assumptions, env)

        (
            (let (env, subst) = env.universal_substitution(binder))
            (let p1 = binder.instantiate_with(&subst).unwrap())
            (prove_wc(decls, env, assumptions, p1) => env)
            --- ("forall")
            (prove_wc(decls, env, assumptions, WcData::ForAll(binder)) => env.clone().pop_subst(&subst))
        )

        (
            (prove_wc(decls, env, (assumptions, p1), p2) => env)
            --- ("implies")
            (prove_wc(decls, env, assumptions, WcData::Implies(p1, p2)) => env)
        )

        (
            (a in assumptions)!
            (prove_via(decls, env, assumptions, a, goal) => env)
            ----------------------------- ("assumption - predicate")
            (prove_wc(decls, env, assumptions, WcData::Predicate(goal)) => env)
        )
        (
            (a in assumptions)!
            (prove_via(decls, env, assumptions, a, goal) => env)
            ----------------------------- ("assumption - relation")
            (prove_wc(decls, env, assumptions, WcData::Relation(goal)) => env)
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

            // Create a set of assumptions `co_assumptions` that include the predicate the impl itself
            // is asserting (i.e., `A: Foo<B>`). When proving the impl's where-clauses, we are allowed
            // to assume this is true (in a coinductive fashion).
            //
            // NB: This is actually not what Rust currently does, but it is what "we" (types team) want it to do.
            (let co_assumptions = (assumptions, trait_ref))
            (prove(decls, env, co_assumptions, Wcs::all_eq(&trait_ref.parameters, &i.trait_ref.parameters)) => env)

            // Prove that the well-formedness requirements of the *trait* hold -- for this proof, we cannot
            // assume that the trait is implemented, because that would allow specious implied bounds
            // (i.e., we could assume that `B: Debug` based on the trait definition + the existence of an impl,
            // but actually the impl is responsible for proving that `B: Debug`).
            (prove(decls, env, assumptions, &t.where_clause) => env)
            ----------------------------- ("positive impl")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => env.clone().pop_subst(&subst))
        )

        (
            (if env.bias() == Bias::Completeness)!
            (may_be_remote(decls, env, assumptions, trait_ref) => env)
            ----------------------------- ("coherence / remote impl")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => env)
        )

        (
            (i in decls.neg_impl_decls(&trait_ref.trait_id))
            (let (env, subst) = env.existential_substitution(&i.binder))
            (let i = i.binder.instantiate_with(&subst).unwrap())
            (prove(decls, env, assumptions, Wcs::all_eq(&trait_ref.parameters, &i.trait_ref.parameters)) => env)
            ----------------------------- ("negative impl")
            (prove_wc(decls, env, assumptions, Predicate::NotImplemented(trait_ref)) => env.clone().pop_subst(&subst))
        )

        (
            (prove_eq(decls, env, assumptions, alias_ty, ty) => env)
            ----------------------------- ("alias eq")
            (prove_wc(decls, env, assumptions, Predicate::AliasEq(alias_ty, ty)) => env)
        )

        (
            (ti in decls.trait_invariants())
            (let (env, subst) = env.existential_substitution(&ti.binder))
            (let ti = ti.binder.instantiate_with(&subst).unwrap())
            (prove_via(decls, env, assumptions, &ti.where_clause, trait_ref) => env)
            (prove(decls, env, assumptions, &ti.trait_ref) => env)
            ----------------------------- ("trait implied bound")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => env.clone().pop_subst(&subst))
        )

        (
            (prove_eq(decls, env, assumptions, a, b) => env)
            ----------------------------- ("eq")
            (prove_wc(decls, env, assumptions, Relation::Equals(a, b)) => env)
        )

        (
            (prove_sub(decls, env, assumptions, a, b) => env)
            ----------------------------- ("subtype")
            (prove_wc(decls, env, assumptions, WcData::Relation(Relation::Sub(a, b))) => env)
        )

        (
            (for_all(decls, env, assumptions, &trait_ref.parameters, &prove_wf) => env)
            (let t = decls.trait_decl(&trait_ref.trait_id))
            (let t = t.binder.instantiate_with(&trait_ref.parameters).unwrap())
            (prove(decls, env, assumptions, &t.where_clause) => env)
            ----------------------------- ("trait well formed")
            (prove_wc(decls, env, assumptions, Predicate::WellFormedTraitRef(trait_ref)) => env)
        )

        (
            (is_local_trait_ref(decls, env, assumptions, trait_ref) => env)
            ----------------------------- ("trait ref is local")
            (prove_wc(decls, env, assumptions, Predicate::IsLocal(trait_ref)) => env)
        )

        (
            (prove_outlives(decls, env, assumptions, a, b) => env)
            ----------------------------- ("outlives")
            (prove_wc(decls, env, assumptions, Relation::Outlives(a, b)) => env)
        )


        (
            (prove_wf(decls, env, assumptions, p) => env)
            ----------------------------- ("parameter well formed")
            (prove_wc(decls, env, assumptions, Relation::WellFormed(p)) => env)
        )

        (
            (prove_const_has_type(decls, env, assumptions, constant) => (ty_constant, env))
            (prove(decls, env, assumptions, Relation::equals(ty_constant, ty)) => env)
            ----------------------------- ("const has ty")
            (prove_wc(decls, env, assumptions, Predicate::ConstHasType(constant, ty)) => env)
        )
    }
}
