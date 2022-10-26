use anyhow::bail;

use crate::{
    cast::Upcast,
    derive_links::{Parameter, Variable},
    grammar::{Fallible, Goal, RigidName, RigidTy, Ty, TyData},
};

use super::Env;

pub(super) fn sub(env: &Env, a: &Parameter, b: &Parameter) -> Fallible<(Env, Vec<Goal>)> {
    let mut env = env.clone();
    let a = env.refresh_inference_variables(a);
    let b = env.refresh_inference_variables(b);
    let goals = sub_parameters(&mut env, &a, &b)?;
    Ok((env, goals))
}

fn sub_parameters(env: &mut Env, a: &Parameter, b: &Parameter) -> Fallible<Vec<Goal>> {
    match (&a, &b) {
        (Parameter::Ty(a), Parameter::Ty(b)) => sub_tys(env, a, b),
        (Parameter::Lt(_), Parameter::Lt(_)) => Ok(vec![Goal::outlives(a, b)]),
        (Parameter::Ty(_), _) | (Parameter::Lt(_), _) => panic!("ill-kinded: {a:?} vs {b:?}"),
    }
}

fn sub_tys(env: &mut Env, a: &Ty, b: &Ty) -> Fallible<Vec<Goal>> {
    if a == b {
        return Ok(vec![]);
    }

    match (a.data(), b.data()) {
        (&TyData::Variable(Variable::InferenceVar(a)), TyData::RigidTy(RigidTy { name, .. })) => {
            assert!(!env.is_mapped(a));
            let a1: Ty = fresh_rigid_ty(env, name);
            let mut goals = vec![a1.well_formed().upcast()];
            goals.extend(env.map_to(a, &a1)?);
            goals.extend(sub_tys(env, &a1, b)?);
            Ok(goals)
        }

        (TyData::RigidTy(RigidTy { name, .. }), &TyData::Variable(Variable::InferenceVar(b))) => {
            assert!(!env.is_mapped(b));
            let b1: Ty = fresh_rigid_ty(env, name);
            let mut goals = vec![b1.well_formed().upcast()];
            goals.extend(env.map_to(b, &b1)?);
            goals.extend(sub_tys(env, a, &b1)?);
            Ok(goals)
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

            // FIXME: variance

            Ok(zip_eq(parameters_a, parameters_b))
        }

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
            let normalizes_goal = Goal::exists_f(|(ty_a, ty_b): (Ty, Ty)| {
                Goal::all(vec![
                    alias_a.normalizes_to(&ty_a).upcast(),
                    alias_b.normalizes_to(&ty_b).upcast(),
                    Goal::sub(ty_a, ty_b),
                ])
            });

            if alias_a.name == alias_b.name {
                Ok(vec![Goal::any(vec![
                    normalizes_goal,
                    Goal::all(zip_eq(&alias_a.parameters, &alias_b.parameters)),
                ])])
            } else {
                Ok(vec![normalizes_goal])
            }
        }

        (TyData::AliasTy(alias_a), _) => {
            let normalizes_goal = Goal::exists_f(|ty: Ty| {
                Goal::all(vec![alias_a.normalizes_to(&ty).upcast(), Goal::sub(ty, b)])
            });
            Ok(vec![normalizes_goal])
        }

        (_, TyData::AliasTy(alias_b)) => {
            let normalizes_goal = Goal::exists_f(|ty: Ty| {
                Goal::all(vec![alias_b.normalizes_to(&ty).upcast(), Goal::sub(a, ty)])
            });
            Ok(vec![normalizes_goal])
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

fn zip_eq(a_s: &[Parameter], b_s: &[Parameter]) -> Vec<Goal> {
    a_s.iter().zip(b_s).map(|(a, b)| Goal::eq(a, b)).collect()
}

fn fresh_rigid_ty(env: &mut Env, name: &RigidName) -> Ty {
    drop((env, name));
    todo!()
}
