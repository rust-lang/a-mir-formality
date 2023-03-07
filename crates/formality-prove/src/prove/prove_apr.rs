use formality_types::{
    grammar::{AtomicPredicate, AtomicRelation, Parameter, Wcs, APR},
    judgment_fn,
};

use crate::{
    program::Program,
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
        program: Program,
        env: Env,
        assumptions: Wcs,
        goal: APR,
    ) => Constraints {
        (
            (&assumptions => a)
            (prove_apr_via(&program, &env, &assumptions, a, &goal) => c)
            ----------------------------- ("assumption")
            (prove_apr(program, env, assumptions, goal) => c)
        )

        (
            (program.impl_decls(&trait_ref.trait_id) => i)
            (let (env, subst) = env.existential_substitution(&i.binder))
            (let i = i.binder.instantiate_with(&subst).unwrap())
            (let t = program.trait_decl(&i.trait_ref.trait_id).binder.instantiate_with(&i.trait_ref.parameters).unwrap())
            (let assumptions_c = (&assumptions, &trait_ref))
            (prove(&program, env, &assumptions_c, all_eq(&trait_ref.parameters, &i.trait_ref.parameters)) => c)
            (prove_after(&program, c, &assumptions_c, (&i.where_clause, &t.where_clause)) => c)
            ----------------------------- ("impl")
            (prove_apr(program, env, assumptions, AtomicPredicate::IsImplemented(trait_ref)) => c.pop_subst(&subst))
        )

        (
            (program.trait_invariants() => ti)
            (let (env, subst) = env.existential_substitution(&ti.binder))
            (let ti = ti.binder.instantiate_with(&subst).unwrap())
            (prove_apr_via(&program, env, &assumptions, &ti.where_clause, &trait_ref) => c)
            (prove_after(&program, c, &assumptions, &ti.trait_ref) => c)
            ----------------------------- ("trait implied bound")
            (prove_apr(program, env, assumptions, AtomicPredicate::IsImplemented(trait_ref)) => c.pop_subst(&subst))
        )

        (
            (prove_ty_eq(program, env, assumptions, a, b) => c)
            ----------------------------- ("eq")
            (prove_apr(program, env, assumptions, AtomicRelation::Equals(Parameter::Ty(a), Parameter::Ty(b))) => c)
        )
    }
}
