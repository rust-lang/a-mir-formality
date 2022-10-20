#![cfg(test)]

use crate::{
    grammar::{Binder, Ty, TyData, Variable},
    parse::term,
};

use super::Env;

#[test]
fn occurs_check1() {
    // (∃T. ∀U. T = U) -- unprovable
    let t: Binder<Ty> = term("<ty T> T");
    let mut env = Env::default();
    let t_e = env.instantiate_existentially(&t);
    let v_e = match t_e.data() {
        TyData::Variable(Variable::InferenceVar(v)) => *v,
        _ => panic!(),
    };
    let t_u = env.instantiate_universally(&t);
    assert!(env.map_to(v_e, t_u.into()).is_err());
}

#[test]
fn occurs_check2() {
    // (∀T. ∃U. T = U) -- ok
    let t: Binder<Ty> = term("<ty T> T");
    let mut env = Env::default();
    let t_u = env.instantiate_universally(&t);
    let t_e = env.instantiate_existentially(&t);
    let v_e = match t_e.data() {
        TyData::Variable(Variable::InferenceVar(v)) => *v,
        _ => panic!(),
    };
    assert!(env.map_to(v_e, t_u.clone().into()).is_ok());
    assert_eq!(env.refresh_inference_variables(t_e), t_u);
}
