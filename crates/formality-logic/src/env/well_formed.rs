use anyhow::bail;
use formality_types::{
    cast::{Upcast, Upcasted},
    grammar::{
        AliasTy, Fallible, Goal, ImplicationTy, Parameter, PredicateTy, RigidName, RigidTy, Ty,
        TyData, Variable,
    },
    seq,
};

use crate::Env;

impl Env {
    /// Rules for when a typr is well-formed. These interact with inference in subtle ways
    /// and thus require built-in treatment, but they seem different from the other built-in
    /// rules like subtype/equality, so I don't love them being here.
    ///
    /// The main interaction is that proving a WF goal is based on "syntactic equality" and
    /// doesn't normalize -- so eg if we have to prove `WellFormed(<alias>)`, we convert
    /// that to a `WellFormedAlias` goal, we don't try to equate that alias with some
    /// rigid type that may be WF. (Presumption is that a alias cannot be normalized unless
    /// it is also WF, since that would require an impl to apply.)
    ///
    /// If `well_formed` is invoked on an unbound inference variable, it simply returns an
    /// ambiguous goal, which will force it to be re-evaluated later when more info is known.
    pub(super) fn well_formed(&self, p: &Parameter) -> Fallible<(Env, Vec<Goal>)> {
        let mut env = self.clone();
        let p = env.refresh_inference_variables(p);
        let goals = match p {
            Parameter::Ty(t) => env.well_formed_ty(&t)?,
            Parameter::Lt(_) => vec![], // always assume LT are WF
        };
        Ok((env, goals))
    }

    fn well_formed_ty(&mut self, t: &Ty) -> Fallible<Vec<Goal>> {
        match t.data() {
            TyData::RigidTy(RigidTy { name, parameters }) => match name {
                RigidName::AdtId(adt) => Ok(vec![adt.well_formed(parameters).upcast()]),
                RigidName::ScalarId(_) => {
                    assert_eq!(parameters.len(), 0);
                    Ok(vec![])
                }
                RigidName::Ref(_) => {
                    assert_eq!(parameters.len(), 2);
                    Ok(vec![Goal::outlives(&parameters[1], &parameters[0])])
                }
                RigidName::Tuple(_) => Ok(parameters
                    .iter()
                    .map(|p| p.well_formed())
                    .upcasted()
                    .collect()),
                RigidName::FnPtr(_) => todo!(),
                RigidName::FnDef(_) => todo!(),
            },

            TyData::AliasTy(AliasTy { name, parameters }) => {
                Ok(seq![name.well_formed(parameters).upcast()])
            }

            TyData::PredicateTy(p) => match p {
                PredicateTy::ForAll(binder) => {
                    let u = self.instantiate_universally(binder);
                    self.well_formed_ty(&u)
                }
                PredicateTy::ImplicationTy(ImplicationTy { predicates, ty }) => {
                    Ok(vec![Goal::implies(predicates, ty.well_formed())])
                }
                PredicateTy::Exists(_) => todo!(),
                PredicateTy::EnsuresTy(_) => todo!(),
            },

            TyData::Variable(v) => match v {
                // we expect that placeholders are proven via assumptions in the environment
                Variable::PlaceholderVar(_) => bail!("cannot prove placeholder wellformed"),

                // we cannot prove an inference variable WF; we have to wait until we
                // know something about it
                Variable::InferenceVar(_) => Ok(vec![Goal::ambiguous()]),

                Variable::BoundVar(_) => panic!("unexpected bound variable"),
            },
        }
    }
}
