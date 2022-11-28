#![cfg(test)]

use formality_types::{
    grammar::{AtomicRelation, Binder, ElaboratedHypotheses, Ty},
    parse::term,
};

use super::Env;
use crate::MockDatabase;

#[test]
fn occurs_check1() {
    // (∃T. ∀U. T = U) -- unprovable
    let t: Binder<Ty> = term("<ty T> T");
    let mut env = Env::default();
    let t_e = env.instantiate_existentially(&t);
    let t_u = env.instantiate_universally(&t);
    let db = MockDatabase::empty();
    let assumptions = ElaboratedHypotheses::none();
    assert!(env
        .apply_relation(&db, &assumptions, &AtomicRelation::eq(t_e, t_u))
        .is_err());
}

#[test]
fn occurs_check2() {
    // (∀T. ∃U. T = U) -- ok
    let t: Binder<Ty> = term("<ty T> T");
    let mut env = Env::default();
    let t_u = env.instantiate_universally(&t);
    let t_e = env.instantiate_existentially(&t);
    let db = MockDatabase::empty();
    let assumptions = ElaboratedHypotheses::none();
    let (env1, goals) = env
        .apply_relation(&db, &assumptions, &AtomicRelation::eq(&t_e, &t_u))
        .unwrap();
    assert!(goals.is_empty());
    assert_eq!(env1.refresh_inference_variables(&t_e), t_u);
}
