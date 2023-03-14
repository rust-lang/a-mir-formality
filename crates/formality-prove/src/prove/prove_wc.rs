use formality_types::{
    grammar::{Predicate, Relation, Wc, WcData, Wcs},
    judgment_fn,
};

use crate::{
    decls::Decls,
    prove::{
        env::Env,
        prove,
        prove_after::prove_after,
        prove_eq::{all_eq, prove_eq},
        prove_via::prove_via,
    },
};

use super::constraints::Constraints;

judgment_fn! {
    pub fn prove_wc(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        goal: Wc,
    ) => Constraints {
        debug(goal, assumptions, env, decls)

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
            (&assumptions => a)
            (prove_via(&decls, &env, &assumptions, a, &goal) => c)
            ----------------------------- ("assumption")
            (prove_wc(decls, env, assumptions, WcData::PR(goal)) => c)
        )

        (
            (decls.impl_decls(&trait_ref.trait_id) => i)
            (let (env, subst) = env.existential_substitution(&i.binder))
            (let i = i.binder.instantiate_with(&subst).unwrap())
            (let t = decls.trait_decl(&i.trait_ref.trait_id).binder.instantiate_with(&i.trait_ref.parameters).unwrap())
            (let co_assumptions = (&assumptions, &trait_ref))
            (prove(&decls, env, &co_assumptions, all_eq(&trait_ref.parameters, &i.trait_ref.parameters)) => c)
            (prove_after(&decls, c, &co_assumptions, &i.where_clause) => c)
            (prove_after(&decls, c, &assumptions, &t.where_clause) => c)
            ----------------------------- ("impl")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => c.pop_subst(&subst))
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
    }
}
