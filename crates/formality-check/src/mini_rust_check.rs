use std::iter::zip;

use formality_core::{Fallible, Map, Upcast};
use formality_prove::Env;
use formality_rust::grammar::minirust::ArgumentExpression::{ByValue, InPlace};
use formality_rust::grammar::minirust::PlaceExpression::Local;
use formality_rust::grammar::minirust::ValueExpression::{Constant, Fn, Load};
use formality_rust::grammar::minirust::{
    self, ty_is_int, ArgumentExpression, BasicBlock, BbId, LocalId, PlaceExpression,
    ValueExpression,
};
use formality_rust::grammar::FnBoundData;
use formality_types::grammar::{CrateId, FnId};
use formality_types::grammar::{Relation, Ty, Wcs};

use crate::{Check, CrateItem};
use anyhow::bail;

impl Check<'_> {
    pub(crate) fn check_body(
        &self,
        env: &Env,
        output_ty: impl Upcast<Ty>,
        fn_assumptions: &Wcs,
        body: minirust::Body,
        declared_input_tys: Vec<Ty>,
        crate_id: &CrateId,
    ) -> Fallible<()> {
        // Type-check:
        //
        // (1) Check that all the types declared for each local variable are well-formed
        // (2) Check whether the number of declared function parameters matches the number of arguments provided.
        // (3) Check that the type of the returned local is compatible with the declared return type
        // (4) Check that the statements in the body are valid

        let output_ty: Ty = output_ty.upcast();

        // (1) Check that all the types declared for each local variable are well-formed
        for lv in &body.locals {
            self.prove_goal(&env, &fn_assumptions, lv.ty.well_formed())?;
        }

        // Check whether the local_id in function arguments are declared.
        for function_arg_id in &body.args {
            if body
                .locals
                .iter()
                .find(|declared_decl| declared_decl.id == *function_arg_id)
                .is_none()
            {
                bail!("Function argument {:?} is not declared, consider declaring them with `let {:?}: type;`", function_arg_id, function_arg_id);
            }
        }

        // Check whether the local_id in the return place are declared.
        if body
            .locals
            .iter()
            .find(|declared_decl| declared_decl.id == body.ret)
            .is_none()
        {
            bail!("Function return place {:?} is not declared, consider declaring them with `let {:?}: type;`", body.ret, body.ret);
        }

        let local_variables: Map<LocalId, Ty> = body
            .locals
            .iter()
            .map(|lv| (lv.id.clone(), lv.ty.clone()))
            .collect();

        // (2) Check whether the number of declared function parameters matches the number of arguments provided.
        if declared_input_tys.len() != body.args.len() {
            bail!(
                "Function argument number mismatch: expected {} arguments, but found {}",
                declared_input_tys.len(),
                body.args.len()
            );
        }

        // (3) Check if the actual return type is the subtype of the declared return type.
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
            callee_input_tys: Map::new(),
            crate_id: crate_id.clone(),
        };

        // (4) Check statements in body are valid
        for block in body.blocks {
            self.check_block(&mut env, fn_assumptions, &block)?;
        }

        Ok(())
    }

    fn check_block(
        &self,
        env: &mut TypeckEnv,
        fn_assumptions: &Wcs,
        block: &minirust::BasicBlock,
    ) -> Fallible<()> {
        for statement in &block.statements {
            self.check_statement(env, fn_assumptions, statement)?;
        }

        self.check_terminator(env, fn_assumptions, &block.terminator)?;

        Ok(())
    }

    fn check_statement(
        &self,
        typeck_env: &mut TypeckEnv,
        fn_assumptions: &Wcs,
        statement: &minirust::Statement,
    ) -> Fallible<()> {
        match statement {
            minirust::Statement::Assign(place_expression, value_expression) => {
                // Check if the place expression is well-formed.
                let place_ty = self.check_place(typeck_env, place_expression)?;

                // Check if the value expression is well-formed.
                let value_ty = self.check_value(typeck_env, value_expression)?;

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

    fn check_terminator(
        &self,
        typeck_env: &mut TypeckEnv,
        fn_assumptions: &Wcs,
        terminator: &minirust::Terminator,
    ) -> Fallible<()> {
        match terminator {
            minirust::Terminator::Goto(bb_id) => {
                // Check that the basic block `bb_id` exists.
                self.check_block_exists(typeck_env, bb_id)?;
            }
            minirust::Terminator::Call {
                callee,
                generic_arguments: _,
                arguments: actual_arguments,
                ret,
                next_block,
            } => {
                // Function is part of the value expression, so we will check if the function exists in check_value.
                self.check_value(typeck_env, callee)?;

                // Get argument information from the callee.
                let Fn(callee_fn_id) = callee else {
                    unreachable!("Callee must exists in Terminator::Call");
                };

                let callee_fn_bound_data = typeck_env.callee_input_tys.get(callee_fn_id).unwrap();
                let callee_declared_input_tys = callee_fn_bound_data.input_tys.clone();
                // Check if the numbers of arguments passed equals to number of arguments declared.
                if callee_declared_input_tys.len() != actual_arguments.len() {
                    bail!("Function arguments number mismatch: the number expected is {:?}, the actual number is {:?}", callee_declared_input_tys.len(), actual_arguments.len());
                }

                let arguments = zip(callee_declared_input_tys, actual_arguments);
                for (declared_ty, actual_argument) in arguments {
                    // Check if the arguments are well formed.
                    let actual_ty = self.check_argument_expression(typeck_env, actual_argument)?;
                    // Check if the actual argument type passed in is the subtype of expect argument type.
                    self.prove_goal(
                        &typeck_env.env,
                        fn_assumptions,
                        Relation::sub(&actual_ty, &declared_ty),
                    )?;
                }

                // Check whether ret place is well-formed.
                let actual_return_ty = self.check_place(typeck_env, ret)?;

                // Check if the fn's declared return type is a subtype of the type of the local variable `ret`
                self.prove_goal(
                    &typeck_env.env,
                    fn_assumptions,
                    Relation::sub(&typeck_env.output_ty, &actual_return_ty),
                )?;

                // Check the validity of next bb_id.
                if let Some(bb_id) = next_block {
                    self.check_block_exists(typeck_env, bb_id)?;
                };
            }
            minirust::Terminator::Return => {
                // Check if the return local variable has been initialized
                if !typeck_env.ret_place_is_initialised {
                    bail!("The return local variable has not been initialized.")
                }
            }

            minirust::Terminator::Switch {
                switch_value,
                switch_targets,
                fallback,
            } => {
                // Check if the value is well-formed.
                let value_ty = self.check_value(typeck_env, switch_value).unwrap();

                if !ty_is_int(value_ty) {
                    bail!("The value used for switch must be an int.");
                }

                // Ensure all bbid are valid.
                for switch_target in switch_targets {
                    self.check_block_exists(typeck_env, &switch_target.target)?;
                }
                self.check_block_exists(typeck_env, fallback)?;
            }
        }
        Ok(())
    }

    // Check if the place expression is well-formed, and return the type of the place expression.
    fn check_place(&self, env: &TypeckEnv, place: &PlaceExpression) -> Fallible<Ty> {
        let place_ty;
        match place {
            Local(local_id) => {
                // Check if place id is a valid local id.
                let Some((_, ty)) = env
                    .local_variables
                    .iter()
                    .find(|(declared_local_id, _)| *declared_local_id == local_id)
                else {
                    bail!(
                        "PlaceExpression::Local: unknown local name `{:?}`",
                        local_id
                    )
                };
                place_ty = ty;
            }
        }
        Ok(place_ty.clone())
    }

    // Check if the value expression is well-formed, and return the type of the value expression.
    fn check_value(&self, typeck_env: &mut TypeckEnv, value: &ValueExpression) -> Fallible<Ty> {
        let value_ty;
        match value {
            Load(place_expression) => {
                value_ty = self.check_place(typeck_env, place_expression)?;
                Ok(value_ty)
            }
            Fn(fn_id) => {
                // Check if the function called is in declared in current crate.

                // Find the crate that is currently being typeck.
                let curr_crate = self
                    .program
                    .crates
                    .iter()
                    .find(|c| c.id == typeck_env.crate_id)
                    .unwrap();

                // Find the callee from current crate.
                let callee = curr_crate.items.iter().find(|item| {
                    match item {
                        CrateItem::Fn(fn_declared) => {
                            if fn_declared.id == *fn_id {
                                let fn_bound_data =
                                    typeck_env.env.instantiate_universally(&fn_declared.binder);
                                // Store the callee information in typeck_env, we will need this when type checking Terminator::Call.
                                typeck_env
                                    .callee_input_tys
                                    .insert(fn_declared.id.clone(), fn_bound_data);
                                return true;
                            }
                            false
                        }
                        _ => false,
                    }
                });

                // If the callee is not found, return error.
                if callee.is_none() {
                    bail!("The function called is not declared in current crate")
                }
                value_ty = typeck_env.output_ty.clone();
                Ok(value_ty)
            }
            Constant(constant) => {
                // If the actual value overflows / does not match the type of the constant,
                // it will be rejected by the parser.
                Ok(constant.get_ty())
            }
        }
    }

    fn check_argument_expression(
        &self,
        env: &mut TypeckEnv,
        arg_expr: &ArgumentExpression,
    ) -> Fallible<Ty> {
        let ty;
        match arg_expr {
            ByValue(val_expr) => {
                ty = self.check_value(env, val_expr)?;
            }
            InPlace(place_expr) => {
                ty = self.check_place(env, place_expr)?;
            }
        }
        Ok(ty)
    }

    fn check_block_exists(&self, env: &TypeckEnv, id: &BbId) -> Fallible<()> {
        for block in env.blocks.iter() {
            if *id == block.id {
                return Ok(());
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

    /// All declared argument type of current function.
    declared_input_tys: Vec<Ty>,

    /// All information of callee.
    callee_input_tys: Map<FnId, FnBoundData>,

    /// The id of the crate where this function resides.
    /// We need this to access information about other functions
    /// declared in the current crate.
    crate_id: CrateId,
}
