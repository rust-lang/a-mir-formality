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
        prove_via::prove_via,
        prove_wf::prove_wf,
    },
};

use super::constraints::Constraints;

judgment_fn! {
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
            (&assumptions => a)!
            (prove_via(&decls, &env, &assumptions, a, &goal) => c)
            ----------------------------- ("assumption - predicate")
            (prove_wc(decls, env, assumptions, WcData::Predicate(goal)) => c)
        )
        (
            (&assumptions => a)!
            (prove_via(&decls, &env, &assumptions, a, &goal) => c)
            ----------------------------- ("assumption - relation")
            (prove_wc(decls, env, assumptions, WcData::Relation(goal)) => c)
        )

        (
            (decls.impl_decls(&trait_ref.trait_id) => i)!
            (let (env, subst) = env.existential_substitution(&i.binder))
            (let i = i.binder.instantiate_with(&subst).unwrap())
            (let t = decls.trait_decl(&i.trait_ref.trait_id).binder.instantiate_with(&i.trait_ref.parameters).unwrap())
            (let co_assumptions = (&assumptions, &trait_ref))
            (prove(&decls, env, co_assumptions, Wcs::all_eq(&trait_ref.parameters, &i.trait_ref.parameters)) => c)
            (prove_after(&decls, c, co_assumptions, &i.where_clause) => c)
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
            (decls.neg_impl_decls(&trait_ref.trait_id) => i)
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
            (decls.trait_invariants() => ti)
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
            (for_all(&decls, &env, &assumptions, &trait_ref.parameters, &prove_wf) => c)
            (let t = &decls.trait_decl(&trait_ref.trait_id))
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
