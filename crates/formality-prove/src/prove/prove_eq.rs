use formality_types::{
    cast::{Upcast, Upcasted},
    cast_impl,
    collections::Set,
    grammar::{AtomicRelation, Parameter, RigidTy, TyData, Variable, WcList},
    judgment,
    judgment::Judgment,
    set,
};

use crate::program::Program;

use super::{prove_wc_list::prove_wc_list, ConstraintSet};

pub fn prove_ty_eq(
    program: impl Upcast<Program>,
    assumptions: impl Upcast<WcList>,
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

pub fn prove_parameters_eq(
    program: impl Upcast<Program>,
    assumptions: impl Upcast<WcList>,
    a: impl Upcast<Vec<Parameter>>,
    b: impl Upcast<Vec<Parameter>>,
) -> Set<ConstraintSet> {
    let program = program.upcast();
    let assumptions = assumptions.upcast();
    let a: Vec<Parameter> = a.upcast();
    let b: Vec<Parameter> = b.upcast();
    let goals: WcList = a
        .into_iter()
        .zip(b)
        .map(|(a, b)| AtomicRelation::eq(a, b))
        .upcasted()
        .collect();
    prove_wc_list(program, assumptions, goals)
}

#[derive(Clone, Hash, Ord, Eq, PartialEq, PartialOrd, Debug)]
struct ProveTyEq(Program, WcList, TyData, TyData);

cast_impl!(ProveTyEq);

judgment! {
    (ProveTyEq => ConstraintSet)

    (
        (let RigidTy { name: a_name, parameters: a_parameters } = a)
        (let RigidTy { name: b_name, parameters: b_parameters } = b)
        (if a_name == b_name)
        (prove_parameters_eq(program, assumptions, a_parameters, b_parameters) => c)
        -----------------------------
        (ProveTyEq(program, assumptions, TyData::RigidTy(a), TyData::RigidTy(b)) => c)
    )

    (
        (let c = set![AtomicRelation::eq(v, t)])
        -----------------------------
        (ProveTyEq(_env, _assumptions, v @ TyData::Variable(Variable::InferenceVar(_)), t @ TyData::RigidTy(_)) => c)
    )

    (
        (let c = set![AtomicRelation::eq(v, t)])
        -----------------------------
        (ProveTyEq(_env, _assumptions, t @ TyData::RigidTy(_), v @ TyData::Variable(Variable::InferenceVar(_))) => c)
    )
}
