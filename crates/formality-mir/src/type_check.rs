use anyhow::bail;
use formality_decl::grammar::{Program, VariantId};
use formality_logic::{Db, Env, GoalResult};
use formality_types::{
    cast::Upcast,
    derive_links::DowncastTo,
    grammar::{
        Fallible, Goal, Hypothesis, ImplicationTy, Parameter, ParameterKind, PredicateTy,
        RigidName, RigidTy, ScalarId, Ty, TyData,
    },
};

use crate::grammar::{
    BasicBlockDecl, LocalDecl, LocalId, LocalsAndBlocks, MirFnBody, Place, PlaceTy, Projection,
    Rvalue, Statement,
};

pub fn type_check(db: &Db, program: &Program, env: &Env, fn_body: &MirFnBody) -> Fallible<()> {
    let mut env = env.clone();
    let MirFnBody { binder } = fn_body;
    let LocalsAndBlocks {
        local_decls,
        basic_block_decls,
    } = env.instantiate_existentially(binder);
    TypeChecker {
        db,
        program,
        env,
        local_decls: &local_decls,
        basic_block_decls: &basic_block_decls,
    }
    .check();
}

struct TypeChecker<'tc> {
    db: &'tc Db,
    program: &'tc Program,
    env: Env,
    assumptions: &'tc [Hypothesis],
    local_decls: &'tc [LocalDecl],
    basic_block_decls: &'tc [BasicBlockDecl],
    deferred_goals: Vec<Goal>,
}

impl<'tc> TypeChecker<'tc> {
    fn check(&mut self) -> Fallible<()> {
        for local_decl in self.local_decls {
            self.check_local_decl(local_decl)?;
        }

        for basic_block in self.basic_block_decls {
            self.check_basic_block(basic_block)?;
        }

        self.prove_deferred_goals()?;

        if self.deferred_goals.len() == 0 {
            Ok(())
        } else {
            bail!("unable to prove {:?}", self.deferred_goals);
        }

        Ok(())
    }

    fn prove_goal(&mut self, goal: impl Upcast<Goal>) -> Fallible<()> {
        let goal: Goal = goal.upcast();
        match formality_logic::prove_goal(self.db, &self.env, self.assumptions, &goal) {
            GoalResult::Yes(env) => {
                self.env = env;
                Ok(())
            }
            GoalResult::Maybe(env) => {
                self.env = env;
                self.deferred_goals.push(goal);
                Ok(())
            }
            GoalResult::No => {
                bail!("cannot prove `{goal:?}`");
            }
        }
    }

    /// Tries to prove deferred goals where possible.
    /// Returns ok when a fixed point is reached.
    /// Returns an error if any deferred goal is now false.
    fn prove_deferred_goals(&mut self) -> Fallible<()> {
        let mut previous_len = 0;

        while self.deferred_goals.len() != previous_len {
            let deferred_goals = std::mem::take(&mut self.deferred_goals);
            previous_len = deferred_goals.len();
            for goal in deferred_goals {
                self.prove_goal(goal)?;
            }
        }

        Ok(())
    }

    fn check_local_decl(&mut self, local_decl: &LocalDecl) -> Fallible<()> {
        self.prove_goal(local_decl.ty.well_formed())?;
        Ok(())
    }

    fn check_basic_block(&mut self, basic_block: &BasicBlockDecl) -> Fallible<()> {
        let BasicBlockDecl {
            id: _,
            statements,
            terminator,
        } = basic_block;

        for s in statements {
            self.check_statement(s)?;
        }

        self.check_terminator(terminator)?;

        Ok(())
    }

    fn check_statement(&mut self, s: &Statement) -> Fallible<()> {
        match s {
            Statement::Assign(place, rvalue) => {
                let rvalue_ty = self.check_rvalue(rvalue)?;
                let place_ty = self.check_place(place)?;
                self.check_assignable(rvalue_ty, place_ty)?;
                Ok(())
            }
            Statement::Noop => Ok(()),
            Statement::FakeRead(place) => {
                self.check_place(place)?;

                Ok(())
            }
        }
    }

    fn check_assignable(
        &mut self,
        value_ty: impl Upcast<Parameter>,
        place_ty: impl Upcast<Parameter>,
    ) -> Fallible<()> {
        self.prove_goal(Goal::sub(value_ty, place_ty))
    }

    fn check_eq(
        &mut self,
        value_ty: impl Upcast<Parameter>,
        place_ty: impl Upcast<Parameter>,
    ) -> Fallible<()> {
        self.prove_goal(Goal::eq(value_ty, place_ty))
    }

    fn check_rvalue(&mut self, rvalue: &Rvalue) -> Fallible<Ty> {
        todo!()
    }

    fn check_place(&mut self, place: &Place) -> Fallible<Ty> {
        let Place {
            local_id,
            projections,
        } = place;

        let local_ty: PlaceTy = self.check_local(local_id)?.upcast();
        for projection in projections {}

        Ok(())
    }

    fn check_local(&mut self, local_id: &LocalId) -> Fallible<Ty> {
        match self.local_decls.iter().find(|ld| ld.name == *local_id) {
            Some(ld) => Ok(ld.ty.clone()),
            None => bail!("no local variable `{:?}`", local_id),
        }
    }

    fn check_projection(
        &mut self,
        base_ty: &PlaceTy,
        projection: &Projection,
    ) -> Fallible<PlaceTy> {
        match projection {
            Projection::Deref => match self.inspect_ty(&base_ty.to_ty())? {
                RigidTy {
                    name: RigidName::Ref(_),
                    parameters,
                } => Ok(parameters[1].downcast_to().unwrap()),
                _ => {
                    bail!("don't know how to dereference {:?}", base_ty);
                }
            },
            Projection::Field(f) => match self.inspect_ty(&base_ty.to_ty())? {
                RigidTy {
                    name: RigidName::AdtId(adt_id),
                    parameters,
                } => {
                    let variant_id = base_ty.variant_id();
                    let struct_defn = self.program.adt_named(&adt_id)?;
                    let struct_defn = struct_defn.binder.instantiate_with(&parameters)?;
                    let variant_defn = struct_defn.variant_named(&variant_id)?;
                    let field_defn = variant_defn.field_named(f)?;
                    Ok(field_defn.ty.upcast())
                }

                _ => {
                    bail!("don't know how to get a field from {:?}", base_ty);
                }
            },
            Projection::Index(local_id) => {
                let ty = self.check_local(local_id)?;
                self.check_eq(ty, ScalarId::Usize)?;
                match self.inspect_ty(&ty)?.data() {
                    TyData::RigidTy(_) => todo!(),
                    TyData::AliasTy(_) => todo!(),
                    TyData::PredicateTy(_) => todo!(),
                    TyData::Variable(_) => todo!(),
                }
            }
            Projection::Downcast(_) => todo!(),
        }
    }

    fn inspect_ty(&mut self, ty: &Ty) -> Fallible<RigidTy> {
        if self.env.is_unbound_inference_variable(ty) {
            self.prove_deferred_goals()?;
        }

        let ty = self.env.refresh_inference_variables(ty);
        match ty.data() {
            TyData::RigidTy(r) => Ok(r.clone()),
            TyData::AliasTy(a) => {
                let v: Ty = self
                    .env
                    .new_existential_variable(ParameterKind::Ty)
                    .upcast();
                self.prove_goal(a.normalizes_to(v))?;
                self.inspect_ty(&v)
            }
            TyData::PredicateTy(p) => match p {
                PredicateTy::ForAll(binder) => {
                    let inner_ty = self.env.instantiate_existentially(binder);
                    self.inspect_ty(&inner_ty)
                }
                PredicateTy::ImplicationTy(ImplicationTy { predicates, ty }) => {
                    self.prove_goal(Goal::all(predicates))?;
                    self.inspect_ty(ty)
                }
                PredicateTy::Exists(_) | PredicateTy::EnsuresTy(_) => {
                    bail!("cannot inspect exists, ensures: {p:?}")
                }
            },
            TyData::Variable(_) => bail!("need more inference information"),
        }
    }
}

impl PlaceTy {
    /// Returns the underlying type, ignoring the variant id in the case
    /// of downcasts.
    fn to_ty(&self) -> Ty {
        match self {
            PlaceTy::Ty(ty) => ty.clone(),
            PlaceTy::VariantTy(ty, _) => ty.clone(),
        }
    }

    /// Returns the variant id to be used for any field accesses.
    fn variant_id(&self) -> VariantId {
        match self {
            PlaceTy::Ty(_) => VariantId::for_struct(),
            PlaceTy::VariantTy(_, id) => id.clone(),
        }
    }
}
