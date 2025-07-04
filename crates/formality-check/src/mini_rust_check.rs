use std::iter::zip;

use formality_core::{Fallible, Map, Upcast};
use formality_prove::Env;
use formality_rust::grammar::minirust::{self, ArgumentExpression, BasicBlock, BbId, LocalId, PlaceExpression, ValueExpression};
use formality_rust::grammar::minirust::PlaceExpression::Local;
use formality_rust::grammar::minirust::ValueExpression::{Load, Fn};
use formality_rust::grammar::minirust::ArgumentExpression::{ByValue, InPlace};
use formality_types::grammar::{Ty, Wcs, Relation};

use anyhow::bail;
use crate::Check;
use crate::CrateItem;

impl Check<'_> {
    pub(crate) fn check_body(
        &self,
        env: &Env,
        output_ty: impl Upcast<Ty>,
        fn_assumptions: &Wcs,
        body: minirust::Body,
        all_fn: &Vec<CrateItem>,
        declared_input_tys: Vec<Ty>,
    ) -> Fallible<()> {
        // Type-check:
        //
        // (1) Check that all the types declared for each local variable are well-formed
        // (2) Check that the type of the returned local is compatible with the declared return type
        // (3) Check that the statements in the body are valid

        let output_ty: Ty = output_ty.upcast();

        // (1) Check that all the types declared for each local variable are well-formed
        for lv in &body.locals {
            self.prove_goal(&env, &fn_assumptions, lv.ty.well_formed())?;
        }
        let local_variables: Map<LocalId, Ty> = body
            .locals
            .iter()
            .map(|lv| (lv.id.clone(), lv.ty.clone()))
            .collect();

        // (2) Check if the actual return type is the subtype of the declared return type.
        self.prove_goal(
            &env,
            fn_assumptions,
            Relation::sub(&local_variables[&body.ret], &output_ty),
        )?;

        let mut env = TypeckEnv {
            env: env.clone(),
            output_ty,
            local_variables,
            blocks: body.blocks.clone(),
            ret_id: body.ret,
            ret_place_is_initialised: false,
            declared_input_tys,
        };

        // (3) Check statements in body are valid
        for block in body.blocks {
            self.check_block(&mut env, fn_assumptions, &block, all_fn)?;
        }

        Ok(())
    }
    
    fn check_block(&self, env: &mut TypeckEnv, fn_assumptions: &Wcs, block: &minirust::BasicBlock, all_fn: &Vec<CrateItem>) -> Fallible<()> {
        for statement in  &block.statements {
            self.check_statement(env, fn_assumptions, statement, all_fn)?;
        }

        self.check_terminator(env, fn_assumptions, &block.terminator, all_fn)?;

        Ok(())
    }

    fn check_statement(&self, typeck_env: &mut TypeckEnv, fn_assumptions: &Wcs,statement: &minirust::Statement, all_fn: &Vec<CrateItem>) -> Fallible<()> {
        match statement {
            minirust::Statement::Assign(place_expression, value_expression) => {
                // Check if the place expression is well-formed.
                let place_ty = self.check_place(typeck_env, place_expression)?;
                // Check if the value expression is well-formed.
                let value_ty = self.check_value(typeck_env, value_expression, all_fn)?;
                // Check that the type of the value is a subtype of the place's type
                self.prove_goal(
                    &typeck_env.env,
                    fn_assumptions,
                    Relation::sub(value_ty, place_ty),
                )?;
                // Record if the return place has been initialised.
                if *place_expression == PlaceExpression::Local(typeck_env.ret_id.clone()) {
                        typeck_env.ret_place_is_initialised = true;
                }
            }
            minirust::Statement::PlaceMention(place_expression) => {
                // Check if the place expression is well-formed.
                self.check_place(typeck_env, place_expression)?;
                // FIXME: check that access the place is allowed per borrowck rules
            }
        }
        Ok(())
    }

    fn check_terminator(&self, typeck_env: &TypeckEnv, fn_assumptions: &Wcs, terminator: &minirust::Terminator, all_fn: &Vec<CrateItem>) -> Fallible<()> {
        match terminator {
            minirust::Terminator::Goto(bb_id) => {
                // Check that the basic block `bb_id` exists.
                self.check_block_exist(typeck_env, bb_id)?;
            }
            minirust::Terminator::Call { callee, generic_arguments:_, arguments: actual_arguments, ret, next_block } => {
                // Function is part of the value expression, so we will check if the function exists in check_value.
                self.check_value(typeck_env, callee, all_fn)?;
                // Check if the numbers of arguments passed equals to number of arguments declared.
                let actual_arg_num = actual_arguments.len();
                let declared_arg_num = typeck_env.declared_input_tys.len();
                if actual_arg_num != declared_arg_num {
                    bail!("The numbers of arguments passed to Terminator::Call is {actual_arg_num}, but the declared function argument number is {declared_arg_num}");
                }
                let arguments = zip(typeck_env.declared_input_tys.clone(), actual_arguments);
                for (declared_ty, actual_argument) in arguments {
                    // Check if the arguments are well formed.
                    let actual_ty = self.check_argument_expression(typeck_env, actual_argument, all_fn)?;
                    // Check if the actual argument type passed in is the subtype of expect argument type.
                    self.prove_goal(
                        &typeck_env.env,
                        fn_assumptions,
                        Relation::sub( &actual_ty, &declared_ty),
                    )?;

                }
                // Check if ret is well-formed. 
                let actual_return_ty = self.check_place(typeck_env, ret)?;
                // Check that the fn's declared return type is a subtype of the type of the local variable `ret`
                self.prove_goal(
                    &typeck_env.env,
                    fn_assumptions,
                    Relation::sub( &typeck_env.output_ty, &actual_return_ty),
                )?;

                // Check that the next block is valid.
                let Some(bb_id) = next_block else {
                    bail!("There should be next block for Terminator::Call, but it does not exist!");
                };
                self.check_block_exist(typeck_env, bb_id)?;
            }
            minirust::Terminator::Return => {
                // Check that the return local variable has been initialized
                if !typeck_env.ret_place_is_initialised {
                    bail!("The return local variable has not been initialized.")
                }
            }
        }
        Ok(())
    }

    // Check if the place expression is well-formed, and return the type of the place expression.
    // FIXME: there might be a way to use prove_goal for this.
    fn check_place(&self, env: &TypeckEnv, place: &PlaceExpression) -> Fallible<Ty> {
        let place_ty;
        match place {
            Local(local_id) => {
                let Some((_, ty)) = env.local_variables.iter().find(|(declared_local_id, _)| *declared_local_id == local_id) else {
                    bail!("PlaceExpression::Local: unknown local name") 
                };
                place_ty = ty;
            }
        }
        Ok(place_ty.clone())
    }

    // Check if the value expression is well-formed, and return the type of the value expression.
    fn check_value(&self, env: &TypeckEnv, value: &ValueExpression, all_fn: &Vec<CrateItem>) -> Fallible<Ty> {
        let value_ty;
        match value {
            Load(place_expression) => {
                value_ty = self.check_place(env, place_expression)?;
                Ok(value_ty)
                // FIXME(tiif): minirust checks if the type of the value is sized, maybe we should do that.
            }
            // Check if the function called is in declared in current crate.
            // FIXME (tiif): tidy up the code here
            Fn(fn_called) => {
                for item in all_fn {
                    match item {
                        CrateItem::Fn(fn_declared) => {
                            if fn_called == fn_declared {
                                value_ty = env.output_ty.clone();
                                return Ok(value_ty);
                            }
                        }
                        _ => {
                            bail!("only CrateItem::Function should be here")
                        }
                    }
                }
               bail!("The function called is not declared in current crate")
            }
        }
    }

    fn check_argument_expression(&self, env: &TypeckEnv, arg_expr: &ArgumentExpression, all_fn: &Vec<CrateItem>) -> Fallible<Ty> {
        let ty;
        match arg_expr {
            ByValue(val_expr) => {
                ty = self.check_value(env, val_expr, all_fn)?;
            },
            InPlace(place_expr) => {
                ty = self.check_place(env, place_expr)?;
            }
        }
        Ok(ty)
    }

    fn check_block_exist(&self, env: &TypeckEnv, id: &BbId) -> Fallible<()> {
        for block in env.blocks.iter() {
            if *id == block.id {
                return Ok(())
            }
        }
        bail!("Basic block {:?} does not exist", id)
    }
}

struct TypeckEnv {
    env: Env,

    /// The declared return type from the function signature.
    output_ty: Ty,

    /// Type of each local variable, as declared.
    local_variables: Map<LocalId, Ty>,

    /// All basic blocks of current body.
    blocks: Vec<BasicBlock>,

    /// local_id of return place,
    ret_id: LocalId,

    /// Record if the return place has been initialised.
    ret_place_is_initialised: bool,

    /// All declared argument type of the function.
    declared_input_tys: Vec<Ty>,
}
