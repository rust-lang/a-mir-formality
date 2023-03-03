use formality_types::{
    grammar::{AtomicPredicate, AtomicRelation, Binder, Parameter, Wcs, APR},
    judgment_fn,
};

use crate::{
    program::Program,
    prove::{
        constraints::merge_constraints,
        prove,
        prove_after::prove_after,
        prove_apr_via::prove_apr_via,
        prove_eq::{all_eq, prove_ty_eq},
        subst::existential_substitution,
    },
};

use super::constraints::Constraints;

judgment_fn! {
    pub fn prove_apr(
        program: Program,
        assumptions: Wcs,
        goal: APR,
    ) => Binder<Constraints> {
        (
            (&assumptions => a)
            (prove_apr_via(&program, &assumptions, a, &goal) => c)
            ----------------------------- ("assumption")
            (prove_apr(program, assumptions, goal) => c)
        )

        (
            (program.impl_decls(&trait_ref.trait_id) => i)
            (let subst = existential_substitution(&i.binder, (&assumptions, &trait_ref)))
            (let i = i.binder.instantiate_with(&subst).unwrap())
            (let t = program.trait_decl(&i.trait_ref.trait_id).binder.instantiate_with(&i.trait_ref.parameters).unwrap())
            (let assumptions_c = (&assumptions, trait_ref.is_implemented()))
            (prove(&program, &assumptions_c, all_eq(&trait_ref.parameters, &i.trait_ref.parameters)) => c1)
            (prove_after(&program, c1, &assumptions_c, (&i.where_clause, &t.where_clause)) => c2)
            ----------------------------- ("impl")
            (prove_apr(program, assumptions, AtomicPredicate::IsImplemented(trait_ref)) => merge_constraints(&subst, (), c2))
        )

        (
            (program.trait_invariants() => ti)
            (let subst = existential_substitution(&ti.binder, (&assumptions, &trait_ref)))
            (let ti = ti.binder.instantiate_with(&subst).unwrap())
            (prove_apr_via(&program, &assumptions, &ti.where_clause, trait_ref.is_implemented()) => c1)
            (prove_after(&program, c1, &assumptions, ti.trait_ref.is_implemented()) => c2)
            ----------------------------- ("trait implied bound")
            (prove_apr(program, assumptions, AtomicPredicate::IsImplemented(trait_ref)) => merge_constraints(&subst, (), c2))
        )

        (
            (prove_ty_eq(program, assumptions, a, b) => c)
            ----------------------------- ("eq")
            (prove_apr(program, assumptions, AtomicRelation::Equals(Parameter::Ty(a), Parameter::Ty(b))) => c)
        )
    }
}
