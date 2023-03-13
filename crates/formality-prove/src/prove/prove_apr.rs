use formality_types::{
    grammar::{AtomicPredicate, AtomicRelation, Parameter, Wcs, APR},
    judgment_fn,
};

use crate::{
    decls::Decls,
    prove::{
        env::Env,
        prove,
        prove_after::prove_after,
        prove_apr_via::prove_apr_via,
        prove_eq::{all_eq, prove_ty_eq},
    },
};

use super::constraints::Constraints;

judgment_fn! {
    pub fn prove_apr(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        goal: APR,
    ) => Constraints {
        debug(goal, assumptions, env, decls)

        (
            (&assumptions => a)
            (prove_apr_via(&decls, &env, &assumptions, a, &goal) => c)
            ----------------------------- ("assumption")
            (prove_apr(decls, env, assumptions, goal) => c)
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
            (prove_apr(decls, env, assumptions, AtomicPredicate::IsImplemented(trait_ref)) => c.pop_subst(&subst))
        )

        (
            (decls.trait_invariants() => ti)
            (let (env, subst) = env.existential_substitution(&ti.binder))
            (let ti = ti.binder.instantiate_with(&subst).unwrap())
            (prove_apr_via(&decls, env, &assumptions, &ti.where_clause, &trait_ref) => c)
            (prove_after(&decls, c, &assumptions, &ti.trait_ref) => c)
            ----------------------------- ("trait implied bound")
            (prove_apr(decls, env, assumptions, AtomicPredicate::IsImplemented(trait_ref)) => c.pop_subst(&subst))
        )

        (
            (prove_ty_eq(decls, env, assumptions, a, b) => c)
            ----------------------------- ("eq")
            (prove_apr(decls, env, assumptions, AtomicRelation::Equals(Parameter::Ty(a), Parameter::Ty(b))) => c)
        )
    }
}
