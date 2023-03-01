use formality_types::{
    cast::{Downcast, Upcast, Upcasted},
    cast_impl,
    collections::Set,
    grammar::{
        AliasTy, AtomicRelation, InferenceVar, Parameter, RigidTy, TyData, Variable, Wcs,
    },
    judgment,
    judgment::Judgment,
    set,
};

use crate::program::Program;

use super::{prove_wc_list::prove_wc_list, ConstraintSet};

/// Goal(s) to prove `a` and `b` are equal (they must have equal length)
pub fn all_eq(a: impl Upcast<Vec<Parameter>>, b: impl Upcast<Vec<Parameter>>) -> Wcs {
    let a: Vec<Parameter> = a.upcast();
    let b: Vec<Parameter> = b.upcast();
    assert_eq!(a.len(), b.len());
    a.into_iter()
        .zip(b)
        .map(|(a, b)| AtomicRelation::eq(a, b))
        .upcasted()
        .collect()
}

/// Goal(s) to prove `a` and `b` are equal
pub fn eq(a: impl Upcast<Parameter>, b: impl Upcast<Parameter>) -> AtomicRelation {
    AtomicRelation::eq(a, b)
}

pub fn prove_parameters_eq(
    program: impl Upcast<Program>,
    assumptions: impl Upcast<Wcs>,
    a: impl Upcast<Vec<Parameter>>,
    b: impl Upcast<Vec<Parameter>>,
) -> Set<ConstraintSet> {
    let program = program.upcast();
    let assumptions = assumptions.upcast();
    let goals = all_eq(a, b);
    prove_wc_list(program, assumptions, goals)
}

#[derive(Clone, Hash, Ord, Eq, PartialEq, PartialOrd, Debug)]
struct ProveTyEq(Program, Wcs, TyData, TyData);

cast_impl!(ProveTyEq);

pub fn prove_ty_eq(
    program: impl Upcast<Program>,
    assumptions: impl Upcast<Wcs>,
    a: impl Upcast<TyData>,
    b: impl Upcast<TyData>,
) -> Set<ConstraintSet> {
    ProveTyEq(
        program.upcast(),
        assumptions.upcast(),
        a.upcast(),
        b.upcast(),
    )
    .apply()
}

judgment! {
    (ProveTyEq => ConstraintSet)

    (
        (if l == r)
        ----------------------------- ("reflexive")
        (ProveTyEq(_env, _assumptions, l, r) => set![])
    )

    (
        (let RigidTy { name: a_name, parameters: a_parameters } = a)
        (let RigidTy { name: b_name, parameters: b_parameters } = b)
        (if a_name == b_name)
        (prove_parameters_eq(program, assumptions, a_parameters, b_parameters) => c)
        ----------------------------- ("rigid")
        (ProveTyEq(program, assumptions, TyData::RigidTy(a), TyData::RigidTy(b)) => c)
    )

    (
        (let AliasTy { name: a_name, parameters: a_parameters } = a)
        (let AliasTy { name: b_name, parameters: b_parameters } = b)
        (if a_name == b_name)
        (prove_parameters_eq(program, assumptions, a_parameters, b_parameters) => c)
        ----------------------------- ("alias-unnormalized")
        (ProveTyEq(program, assumptions, TyData::AliasTy(a), TyData::AliasTy(b)) => c)
    )

    (
        (if let None = t.downcast::<InferenceVar>())
        ----------------------------- ("existential-l")
        (ProveTyEq(_env, _assumptions, TyData::Variable(Variable::InferenceVar(v)), t) => set![eq(v, t)])
    )

    (
        (if let None = t.downcast::<InferenceVar>())
        ----------------------------- ("existential-r")
        (ProveTyEq(_env, _assumptions, t, TyData::Variable(Variable::InferenceVar(v))) => set![eq(v, t)])
    )

    (
        (if l < r)
        ----------------------------- ("existential-both")
        (ProveTyEq(_env, _assumptions, TyData::Variable(Variable::InferenceVar(l)), TyData::Variable(Variable::InferenceVar(r))) => set![eq(l, r)])
    )

    (
        (if r < l)
        ----------------------------- ("existential-both-rev")
        (ProveTyEq(_env, _assumptions, TyData::Variable(Variable::InferenceVar(l)), TyData::Variable(Variable::InferenceVar(r))) => set![eq(r, l)])
    )

    (
        (if !matches!(b, TyData::Variable(Variable::InferenceVar(_))))
        (program.alias_eq_decls(&a.name) => decl)
        (let decl = decl.binder.instantiate_existentially((&assumptions, &a, &b)))
        (assert a.name == decl.alias.name)
        (let assumptions1 = if decl.ty.is_rigid() {
            // Normalizing to a rigid type: productive
            assumptions.union(eq(&a, &b))
        } else {
            // Normalizing to a variable or alias: not productive
            assumptions.clone()
        })
        (prove_wc_list(&program, &assumptions1, all![
            all_eq(&a.parameters, &decl.alias.parameters),
            eq(&b, &decl.ty),
            decl.where_clause,
        ]) => c)
        ----------------------------- ("alias-normalized")
        (ProveTyEq(program, assumptions, TyData::AliasTy(a), b) => c)
    )

    (
        (prove_ty_eq(program, assumptions, a, t) => c)
        ----------------------------- ("alias-r")
        (ProveTyEq(program, assumptions, t, TyData::AliasTy(a)) => c)
    )
}
