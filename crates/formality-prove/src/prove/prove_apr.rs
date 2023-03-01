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
            (JudgmentStruct(program, assumptions, goal) => c)
        )

        (
            (program.impl_decls(&trait_ref.trait_id) => i)
            (let i = i.binder.instantiate_existentially((&assumptions, &trait_ref)))
            (let t = program.trait_decl(&i.trait_ref.trait_id).binder.instantiate_with(&i.trait_ref.parameters).unwrap())
            (let assumptions_c = assumptions.union(trait_ref.is_implemented()))
            (prove_wc_list(&program, &assumptions_c, all![
                all_eq(&trait_ref.parameters, &i.trait_ref.parameters),
                i.where_clause,
                t.where_clause,
            ]) => c)
            ----------------------------- ("impl")
            (JudgmentStruct(program, assumptions, APR::AtomicPredicate(AtomicPredicate::IsImplemented(trait_ref))) => c)
        )

        (
            (program.trait_invariants() => ti)
            (let ti = ti.binder.instantiate_existentially((&assumptions, &trait_ref)))
            (prove_apr_via(&program, &assumptions, &ti.where_clause, trait_ref.is_implemented()) => c1)
            (prove_after(&program, &assumptions, c1, ti.trait_ref.is_implemented()) => c2)
            ----------------------------- ("trait implied bound")
            (JudgmentStruct(program, assumptions, APR::AtomicPredicate(AtomicPredicate::IsImplemented(trait_ref))) => c2)
        )

        (
            (prove_ty_eq(program, assumptions, a, b) => c)
            ----------------------------- ("eq")
            (JudgmentStruct(program, assumptions, APR::AtomicRelation(AtomicRelation::Equals(Parameter::Ty(a), Parameter::Ty(b)))) => c)
        )
    }
}
