use formality_types::{
    cast::{Downcast, Upcast, Upcasted},
    collections::Map,
    fold::Fold,
    grammar::{
        AliasTy, AtomicRelation, Binder, InferenceVar, Parameter, PlaceholderVar, RigidTy, Ty,
        TyData, Universe, Variable, Wcs,
    },
    judgment_fn,
    term::Term,
    visit::Visit,
};

use crate::{
    program::Program,
    prove::{
        constraints::{constrain, merge_constraints, no_constraints, occurs_in},
        prove,
        prove_normalize::prove_normalize,
        subst::existential_substitution,
    },
};

use super::{
    constraints::{self, Constraints},
    subst::{self, fresh_existential},
};

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

judgment_fn! {
    pub fn prove_ty_eq(
        program: Program,
        assumptions: Wcs,
        a: Ty,
        b: Ty,
    ) => Binder<Constraints> {
        (
            (if l == r)
            ----------------------------- ("reflexive")
            (prove_ty_eq(_program, _assumptions, l, r) => no_constraints(()))
        )

        (
            (prove_ty_eq(program, assumptions, r, l) => c)
            ----------------------------- ("symmetric")
            (prove_ty_eq(program, assumptions, l, r) => c)
        )

        (
            (let RigidTy { name: a_name, parameters: a_parameters } = a)
            (let RigidTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)
            (prove(program, assumptions, all_eq(a_parameters, b_parameters)) => c)
            ----------------------------- ("rigid")
            (prove_ty_eq(program, assumptions, TyData::RigidTy(a), TyData::RigidTy(b)) => c)
        )

        (
            (let AliasTy { name: a_name, parameters: a_parameters } = a)
            (let AliasTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)
            (prove(program, assumptions, all_eq(a_parameters, b_parameters)) => c)
            ----------------------------- ("alias")
            (prove_ty_eq(program, assumptions, TyData::AliasTy(a), TyData::AliasTy(b)) => c)
        )

        (
            (if let None = t.downcast::<InferenceVar>())
            (if let Some(c) = equate_variable(v, t, assumptions))
            ----------------------------- ("existential-l")
            (prove_ty_eq(_env, assumptions, Variable::InferenceVar(v), t) => constrain(v, t))
        )

        (
            (if let None = t.downcast::<InferenceVar>())
            (if !occurs_in(v, &t))
            ----------------------------- ("existential-r")
            (prove_ty_eq(_env, _assumptions, t, Variable::InferenceVar(v)) => constrain(v, t))
        )

        (
            (let a = l.min(r))
            (let b = l.max(r))
            ----------------------------- ("existential-both")
            (prove_ty_eq(_env, _assumptions, Variable::InferenceVar(l), Variable::InferenceVar(r)) => constrain(a, b))
        )

        (
            (prove_normalize(&program, &assumptions, &x) => nc1)
            (let subst = existential_substitution(&nc1, (&assumptions, &x, &z)))
            (let (y, c1) = nc1.instantiate_with(&subst).unwrap().split_result())
            (prove(&program, &assumptions, eq(y, &z)) => c2)
            ----------------------------- ("normalize-l")
            (prove_ty_eq(program, assumptions, x, z) => merge_constraints(&subst, &c1, c2))
        )
    }
}

fn equate_variable(x: InferenceVar, p: &Parameter) -> Option<Binder<Constraints>> {
    let mut replacements: Map<InferenceVar, Variable> = Map::default();

    if occurs_in(x, p) {
        return None;
    }

    let fv = p.free_variables().deduplicate();

    let p1: Parameter = p.substitute(&mut |v| match v {
        Variable::InferenceVar(v) => {
            if v.universe <= x.universe {
                None
            } else if let Some(r) = replacements.get(&v) {
                Some(r.upcast())
            } else {
                // For each inference variable `v` in a universe not nameable from `u`,
                // replace with a fresh inference variable `v1` that IS in `u`,
                // and create a binding `v := v1` (this binding is legal because
                // v1 is fresh and `universe(v) >= universe(v1)`).
                let v1: Variable = fresh_existential(x.universe, v.kind, &fresh_in).upcast();
                replacements.insert(v, v1.clone());
                Some(v1.upcast())
            }
        }
        Variable::PlaceholderVar(_) | Variable::BoundVar(_) => None,
    });

    let mut existentials: Vec<Variable> = replacements.values().cloned().collect();
    let constraints: Constraints = replacements
        .into_iter()
        .upcasted()
        .chain(std::iter::once((x, p1)))
        .collect();
    Some(Binder::mentioned(existentials, constraints))
}
