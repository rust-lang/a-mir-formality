#![cfg(FIXME)]

use anyhow::bail;
use formality_decl::grammar::{FnBoundData, Program, VariantId};
use formality_logic::{Db, Env, GoalResult};
use formality_types::{
    cast::{Downcast, To, Upcast},
    grammar::{
        Fallible, Goal, Hypothesis, ImplicationTy, Parameter, ParameterKind, PredicateTy,
        RigidName, RigidTy, ScalarId, Ty, TyData,
    },
};

use crate::grammar::{
    BasicBlockDecl, LocalDecl, LocalId, LocalsAndBlocks, MirFnBody, Operand, Place, PlaceTy,
    Projection, Rvalue, Statement, Terminator,
};

pub fn type_check(
    db: &Db,
    program: &Program,
    env: &Env,
    assumptions: &[Hypothesis],
    fn_body: &MirFnBody,
) -> Fallible<()> {
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
        assumptions,
        deferred_goals: vec![],
        local_decls: &local_decls,
        basic_block_decls: &basic_block_decls,
    }
    .check()
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
    }

    fn prove_goals(&mut self, goals: impl Upcast<Vec<Goal>>) -> Fallible<()> {
        let goals = goals.upcast();
        for g in goals {
            self.prove_goal(g)?;
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

    fn check_terminator(&mut self, t: &Terminator) -> Fallible<()> {
        match t {
            Terminator::Goto(_)
            | Terminator::Resume
            | Terminator::Abort
            | Terminator::Return
            | Terminator::Unreachable => Ok(()),

            Terminator::Drop(place, _) | Terminator::DropAndReplace(place, _) => {
                self.check_place(place)?;
                Ok(())
            }

            Terminator::Call(func, args, destination, _) => {
                let func_ty = self.check_operand(func)?;
                let arg_tys = self.check_operands(args)?;
                let destination_ty = self.check_place(destination)?;

                let (input_tys, output_ty): (Vec<Ty>, Ty) = match self.inspect_ty(&func_ty)? {
                    RigidTy {
                        name: RigidName::FnDef(f),
                        parameters,
                    } => {
                        let fn_decl = self.program.fn_named(&f)?;
                        let FnBoundData {
                            input_tys,
                            output_ty,
                            where_clauses,
                        } = fn_decl.binder.instantiate_with(&parameters)?;

                        self.prove_goals(where_clauses)?;

                        (input_tys, output_ty)
                    }

                    RigidTy {
                        name: RigidName::FnPtr(num_args),
                        parameters,
                    } => {
                        if num_args != arg_tys.len() {
                            bail!(
                                "wrong number of arguments, expected {num_args:?} found {}",
                                arg_tys.len()
                            );
                        }

                        let (output_ty, input_tys) = parameters.split_last().unwrap();

                        (
                            input_tys
                                .iter()
                                .map(|i| {
                                    i.downcast::<Ty>()
                                        .ok_or_else(|| anyhow::format_err!("ill-kinded fn ptr"))
                                })
                                .collect::<Fallible<_>>()?,
                            output_ty
                                .downcast::<Ty>()
                                .ok_or_else(|| anyhow::format_err!("ill-kinded fn ptr"))?,
                        )
                    }

                    _ => bail!("cannot call values of type `{func_ty:?}`"),
                };

                self.prove_goals(
                    input_tys
                        .iter()
                        .zip(&arg_tys)
                        .map(|(e, a)| Goal::sub(a, e))
                        .chain(Some(Goal::sub(output_ty, destination_ty)))
                        .collect::<Vec<_>>(),
                )?;

                Ok(())
            }
        }
    }

    fn check_operands(&mut self, operands: &[Operand]) -> Fallible<Vec<Ty>> {
        operands.iter().map(|o| self.check_operand(o)).collect()
    }

    fn check_operand(&mut self, operand: &Operand) -> Fallible<Ty> {
        match operand {
            Operand::Move(place) | Operand::Copy(place) => self.check_place(place),
            Operand::Const(_) => todo!(),
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

    fn check_rvalue(&mut self, _rvalue: &Rvalue) -> Fallible<Ty> {
        todo!()
    }

    fn check_place(&mut self, place: &Place) -> Fallible<Ty> {
        let Place {
            local_id,
            projections,
        } = place;

        let mut place_ty: PlaceTy = self.check_local(local_id)?.upcast();
        for projection in projections {
            place_ty = self.check_projection(&place_ty, projection)?;
        }

        match place_ty {
            PlaceTy::Ty(ty) => Ok(ty),
            PlaceTy::VariantTy(..) => {
                bail!("expected place with ordinary type, found `{place_ty:?}`")
            }
        }
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
                } => Ok(parameters[1].downcast().unwrap()),
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
                    Ok(field_defn.ty.to())
                }

                _ => {
                    bail!("don't know how to get a field from {:?}", base_ty);
                }
            },
            Projection::Index(local_id) => {
                let ty = self.check_local(local_id)?;
                self.check_eq(&ty, ScalarId::Usize)?;
                match self.inspect_ty(&ty)? {
                    RigidTy { .. } => {
                        bail!("FIXME: index paths not yet supported")
                    }
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
                self.prove_goal(a.normalizes_to(&v))?;
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
