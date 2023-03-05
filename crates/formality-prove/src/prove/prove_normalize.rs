use formality_types::{
    cast::{Upcast},
    grammar::{AliasTy, AtomicRelation, Binder, Parameter, TyData, Wc, WcData, Wcs},
    judgment_fn,
};

use crate::{
    program::{AliasEqDeclBoundData, Program},
    prove::{
        constraints::{instantiate_and_apply_constraints, merge_constraints, no_constraints},
        prove,
        prove_eq::all_eq,
        subst::existential_substitution,
    },
};

use super::constraints::Constraints;

judgment_fn! {
    pub fn prove_normalize(
        program: Program,
        assumptions: Wcs,
        p: Parameter,
    ) => Binder<Constraints<Parameter>> {
        (
            (&assumptions => a)
            (prove_normalize_via(&program, &assumptions, a, &goal) => c)
            ----------------------------- ("normalize-via-assumption")
            (prove_normalize(program, assumptions, goal) => c)
        )

        (
            (program.alias_eq_decls(&a.name) => decl)
            (let subst = existential_substitution(&decl.binder, (&assumptions, &a)))
            (let AliasEqDeclBoundData { alias: AliasTy { name, parameters }, ty, where_clause } = decl.binder.instantiate_with(&subst).unwrap())
            (assert a.name == name)
            (prove(&program, &assumptions, (
                all_eq(&a.parameters, &parameters),
                where_clause,
            )) => c)
            (let c = c.map(|c| c.map(|()| (&ty).upcast()))) // XXX subst
            ----------------------------- ("normalize-via-impl")
            (prove_normalize(program, assumptions, TyData::AliasTy(a)) => merge_constraints(&subst, (), c))
        )
    }
}

judgment_fn! {
    fn prove_normalize_via(
        program: Program,
        assumptions: Wcs,
        via: Wc,
        goal: Parameter,
    ) => Binder<Constraints<Parameter>> {
        (
            (if goal == a && goal != b)
            ----------------------------- ("axiom-l")
            (prove_normalize_via(_program, _assumptions, AtomicRelation::Equals(a, b), goal) => no_constraints(b))
        )

        (
            (if goal != a && goal == b)
            ----------------------------- ("axiom-r")
            (prove_normalize_via(_program, _assumptions, AtomicRelation::Equals(a, b), goal) => no_constraints(a))
        )

        (
            (let subst = existential_substitution(&binder, (&assumptions, &goal)))
            (let via1 = binder.instantiate_with(&subst).unwrap())
            (prove_normalize_via(program, assumptions, via1, goal) => c)
            ----------------------------- ("forall")
            (prove_normalize_via(program, assumptions, WcData::ForAll(binder), goal) => merge_constraints(&subst, (), c))
        )

        (
            (prove_normalize_via(&program, &assumptions, &wc_consequence, goal) => c1)
            (let (existentials, c1, (assumptions, wc_condition)) = instantiate_and_apply_constraints(c1, (assumptions.clone(), wc_condition.clone())))
            (let (p, c1) = c1.split_result())
            (prove(&program, &assumptions, &wc_condition) => c2)
            (let c2 = c2.map(|c2| c2.map(|()| p.clone())))
            ----------------------------- ("implies")
            (prove_normalize_via(program, assumptions, WcData::Implies(wc_condition, wc_consequence), goal) => merge_constraints(&existentials, &c1, c2))
        )
    }
}
