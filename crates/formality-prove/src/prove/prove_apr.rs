use formality_types::{
    cast::Upcast,
    grammar::{AtomicPredicate, AtomicRelation, Parameter, Wcs, APR},
    judgment_fn,
};

use crate::{
    program::Program,
    prove::{
        prove_after::prove_after,
        prove_apr_via::prove_apr_via,
        prove_eq::{all_eq, prove_ty_eq},
        prove_wc_list::prove_wc_list,
        subst::existential_substitution,
    },
};

use super::ConstraintSet;

judgment_fn! {
    pub fn prove_apr(
        program: Program,
        assumptions: Wcs,
        goal: APR,
    ) => ConstraintSet {
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
            (let assumptions_c = assumptions.union(trait_ref.is_implemented()))
            (prove_wc_list(&program, &assumptions_c, all![
                all_eq(&trait_ref.parameters, &i.trait_ref.parameters),
                i.where_clause,
                t.where_clause,
            ]) => c)
            ----------------------------- ("impl")
            (prove_apr(program, assumptions, APR::AtomicPredicate(AtomicPredicate::IsImplemented(trait_ref))) => c)
        )

        (
            (program.trait_invariants() => ti)
            (let subst = existential_substitution(&ti.binder, (&assumptions, &trait_ref)))
            (let ti = ti.binder.instantiate_with(&subst).unwrap())
            (prove_apr_via(&program, &assumptions, &ti.where_clause, trait_ref.is_implemented()) => c1)
            (prove_after(&program, &assumptions, c1, ti.trait_ref.is_implemented()) => c2)
            ----------------------------- ("trait implied bound")
            (prove_apr(program, assumptions, APR::AtomicPredicate(AtomicPredicate::IsImplemented(trait_ref))) => c2)
        )

        (
            (prove_ty_eq(program, assumptions, a, b) => c)
            ----------------------------- ("eq")
            (prove_apr(program, assumptions, APR::AtomicRelation(AtomicRelation::Equals(Parameter::Ty(a), Parameter::Ty(b)))) => c)
        )
    }
}
