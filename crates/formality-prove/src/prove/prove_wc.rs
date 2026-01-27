use formality_core::judgment_fn;
use formality_types::grammar::{Predicate, Relation, Wc, WcData, Wcs};

use crate::{
    decls::Decls,
    prove::{
        combinators::for_all,
        env::{Bias, Env},
        is_local::{is_local_trait_ref, may_be_remote},
        prove,
        prove_after::prove_after,
        prove_eq::prove_eq,
        prove_outlives::prove_outlives,
        prove_sub::prove_sub,
        prove_via::prove_via,
        prove_wf::prove_wf,
    },
};

use super::constraints::Constraints;

judgment_fn! {
    /// The "heart" of the trait system -- prove that a where-clause holds given a set of declarations, variable environment, and set of assumptions.
    /// If successful, returns the constraints under which the where-clause holds.
    pub fn prove_wc(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        goal: Wc,
    ) => Constraints {
        debug(goal, assumptions, env)

        (
            (let (env, subst) = env.universal_substitution(&binder))
            (let p1 = binder.instantiate_with(&subst).unwrap())
            (prove_wc(decls, env, &assumptions, p1) => c)
            --- ("forall")
            (prove_wc(decls, env, assumptions, WcData::ForAll(binder)) => c.pop_subst(&subst))
        )

        (
            (prove_wc(decls, env, (assumptions, p1), p2) => c)
            --- ("implies")
            (prove_wc(decls, env, assumptions, WcData::Implies(p1, p2)) => c)
        )

        (
            (a in &assumptions)!
            (prove_via(&decls, &env, &assumptions, a, &goal) => c)
            ----------------------------- ("assumption - predicate")
            (prove_wc(decls, env, assumptions, WcData::Predicate(goal)) => c)
        )
        (
            (a in &assumptions)!
            (prove_via(&decls, &env, &assumptions, a, &goal) => c)
            ----------------------------- ("assumption - relation")
            (prove_wc(decls, env, assumptions, WcData::Relation(goal)) => c)
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
            (let co_assumptions = (&assumptions, &trait_ref))
            (prove(&decls, env, co_assumptions, Wcs::all_eq(&trait_ref.parameters, &i.trait_ref.parameters)) => c)
            (prove_after(&decls, c, co_assumptions, &i.where_clause) => c)

            // Prove that the well-formedness requirements of the *trait* hold -- for this proof, we cannot
            // assume that the trait is implemented, because that would allow specious implied bounds
            // (i.e., we could assume that `B: Debug` based on the trait definition + the existence of an impl,
            // but actually the impl is responsible for proving that `B: Debug`).
            (prove_after(&decls, c, &assumptions, &t.where_clause) => c)
            ----------------------------- ("positive impl")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => c.pop_subst(&subst))
        )

        (
            (if env.bias() == Bias::Completeness)!
            (may_be_remote(decls, &env, assumptions, trait_ref) => c)
            ----------------------------- ("coherence / remote impl")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => c)
        )

        (
            (i in decls.neg_impl_decls(&trait_ref.trait_id))
            (let (env, subst) = env.existential_substitution(&i.binder))
            (let i = i.binder.instantiate_with(&subst).unwrap())
            (prove(&decls, env, &assumptions, Wcs::all_eq(&trait_ref.parameters, &i.trait_ref.parameters)) => c)
            (prove_after(&decls, c, &assumptions, &i.where_clause) => c)
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
            (prove_via(&decls, env, &assumptions, &ti.where_clause, &trait_ref) => c)
            (prove_after(&decls, c, &assumptions, &ti.trait_ref) => c)
            ----------------------------- ("trait implied bound")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => c.pop_subst(&subst))
        )

        (
            (prove_eq(decls, env, assumptions, a, b) => c)
            ----------------------------- ("eq")
            (prove_wc(decls, env, assumptions, Relation::Equals(a, b)) => c)
        )

        (
            (prove_sub(decls, env, assumptions, &a, &b) => c)
            ----------------------------- ("subtype")
            (prove_wc(decls, env, assumptions, WcData::Relation(Relation::Sub(a, b))) => c)
        )

        (
            (for_all(&decls, &env, &assumptions, &trait_ref.parameters, &prove_wf) => c)
            (let t = decls.trait_decl(&trait_ref.trait_id))
            (let t = t.binder.instantiate_with(&trait_ref.parameters).unwrap())
            (prove_after(&decls, c, &assumptions, t.where_clause) => c)
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
            (prove_wc(decls, env, assumptions, Relation::Outlives(a, b)) => c)
        )


        (
            (prove_wf(decls, env, assumptions, p) => c)
            ----------------------------- ("parameter well formed")
            (prove_wc(decls, env, assumptions, Relation::WellFormed(p)) => c)
        )

        (
            (if let Some((_, const_ty)) = ct.as_value())
            (prove(decls, env, assumptions, Wcs::all_eq(vec![const_ty], vec![ty])) => c)
            ----------------------------- ("const has ty")
            (prove_wc(decls, env, assumptions, Predicate::ConstHasType(ct, ty)) => c)
        )
    }
}
