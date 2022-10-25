use anyhow::bail;

use crate::{
    cast::Upcast,
    derive_links::{Parameter, ParameterKind, Variable},
    grammar::{fresh_bound_var, Fallible, Goal, Lt, RigidTy, Ty, TyData},
};

use super::Env;

pub(super) fn eq(env: &Env, a: &Parameter, b: &Parameter) -> Fallible<(Env, Vec<Goal>)> {
    let mut env = env.clone();
    let a = env.refresh_inference_variables(a);
    let b = env.refresh_inference_variables(b);
    let goals = eq_parameters(&mut env, &a, &b)?;
    Ok((env, goals))
}

fn eq_parameters(env: &mut Env, a: &Parameter, b: &Parameter) -> Fallible<Vec<Goal>> {
    match (&a, &b) {
        (Parameter::Ty(a), Parameter::Ty(b)) => eq_tys(env, a, b),
        (Parameter::Lt(a), Parameter::Lt(b)) => eq_lts(env, a, b),
        (Parameter::Ty(_), _) | (Parameter::Lt(_), _) => panic!("ill-kinded: {a:?} vs {b:?}"),
    }
}

fn eq_tys(env: &mut Env, a: &Ty, b: &Ty) -> Fallible<Vec<Goal>> {
    if a == b {
        return Ok(vec![]);
    }

    match (a.data(), b.data()) {
        (&TyData::Variable(Variable::InferenceVar(a)), _) => {
            assert!(!env.is_mapped(a));
            let goals = env.map_to(a, b)?;
            Ok(goals)
        }

        (_, &TyData::Variable(Variable::InferenceVar(b))) => {
            assert!(!env.is_mapped(b));
            let goals = env.map_to(b, a)?;
            Ok(goals)
        }

        (TyData::AliasTy(alias_a), TyData::AliasTy(alias_b)) => {
            let normalizes_goal = {
                let (kvi, bvar) = fresh_bound_var(ParameterKind::Ty);
                let normalizes_a = alias_a.normalizes_to(bvar.ty());
                let normalizes_b = alias_b.normalizes_to(bvar.ty());
                Goal::exists(&[kvi], Goal::all(vec![normalizes_a, normalizes_b]))
            };

            if alias_a.name == alias_b.name {
                Ok(vec![Goal::any(vec![
                    normalizes_goal,
                    eq_parameters_goal(&alias_a.parameters, &alias_b.parameters),
                ])])
            } else {
                Ok(vec![normalizes_goal])
            }
        }

        (TyData::AliasTy(alias_a), _) => {
            let normalizes_goal = alias_a.normalizes_to(b);
            Ok(vec![normalizes_goal.upcast()])
        }

        (_, TyData::AliasTy(alias_b)) => {
            let normalizes_goal = alias_b.normalizes_to(a);
            Ok(vec![normalizes_goal.upcast()])
        }

        (
            TyData::RigidTy(RigidTy {
                name: name_a,
                parameters: parameters_a,
            }),
            TyData::RigidTy(RigidTy {
                name: name_b,
                parameters: parameters_b,
            }),
        ) => {
            if name_a != name_b {
                anyhow::bail!("cannot equate `{a:?}` and `{b:?}`");
            }

            eq_vec_parameters(env, parameters_a, parameters_b)
        }

        (TyData::Variable(Variable::PlaceholderVar(_)), _)
        | (_, TyData::Variable(Variable::PlaceholderVar(_))) => {
            bail!("not-eq({a:?}, {b:?})")
        }

        (TyData::Variable(Variable::BoundVar(_)), _)
        | (_, TyData::Variable(Variable::BoundVar(_))) => {
            panic!("found unexpected bound variable")
        }

        (TyData::PredicateTy(_), _) | (_, TyData::PredicateTy(_)) => todo!(),
    }
}

fn eq_lts(_env: &mut Env, _a: &Lt, _b: &Lt) -> Fallible<Vec<Goal>> {
    unimplemented!("equate lifetimes")
}

fn eq_parameters_goal(a_s: &[Parameter], b_s: &[Parameter]) -> Goal {
    let eq_goals: Vec<_> = a_s.iter().zip(b_s).map(|(a, b)| Goal::eq(a, b)).collect();
    Goal::all(eq_goals)
}

fn eq_vec_parameters(env: &mut Env, a_s: &[Parameter], b_s: &[Parameter]) -> Fallible<Vec<Goal>> {
    assert_eq!(a_s.len(), b_s.len());
    let mut goals = vec![];
    for (a, b) in a_s.iter().zip(b_s) {
        goals.extend(eq_parameters(env, a, b)?);
    }
    Ok(goals)
}
