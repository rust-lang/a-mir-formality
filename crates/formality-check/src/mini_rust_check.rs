use formality_core::{Fallible, Map, Upcast};
use formality_prove::Env;
use formality_rust::grammar::minirust::{self, LocalId};
use formality_types::grammar::{Ty, Wcs, Relation};

use crate::Check;

impl Check<'_> {
    pub(crate) fn check_body(
        &self,
        env: &Env,
        output_ty: impl Upcast<Ty>,
        fn_assumptions: &Wcs,
        body: minirust::Body,
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

        let env = TypeckEnv {
            env: env.clone(),
            output_ty,
            local_variables,
        };

        // (3) Check statements in body are valid
        for block in &body.blocks {
            self.check_block(&env, block)?;
        }

        Ok(())
    }
    
    fn check_block(&self, env: &TypeckEnv, block: &minirust::BasicBlock) -> Fallible<()> {
        for statement in  &block.statements {
            self.check_statement(env, statement)?;
        }

        self.check_terminator(env, &block.terminator)?;

        Ok(())
    }

    fn check_statement(&self, _env: &TypeckEnv, statement: &minirust::Statement) -> Fallible<()> {
        match statement {
            minirust::Statement::Assign(_place_expression, _value_expression) => {
                // FIXME: check that the place and value are well-formed
                // FIXME: check that the type of the value is a subtype of the place's type
                todo!();
            }
            minirust::Statement::PlaceMention(_place_expression) => {
                // FIXME: check that the place is well-formed
                // FIXME: check that access the place is allowed per borrowck rules
                todo!();
            }
        }
    }

    fn check_terminator(&self, _env: &TypeckEnv, terminator: &minirust::Terminator) -> Fallible<()> {
        match terminator {
            minirust::Terminator::Goto(_bb_id) => {
                // FIXME: Check that the basic block `bb_id` exists
                todo!();
            }
            minirust::Terminator::Call { callee:_, generic_arguments:_, arguments:_, ret:_, next_block:_ } => {
                // FIXME: check that the callee is something callable
                // FIXME: check that the arguments are well formed and their types are subtypes of the expected argument types
                // FIXME: check that the next block is valid
                // FIXME: check that the fn's declared return type is a subtype of the type of the local variable `ret`
                todo!();
            }
            minirust::Terminator::Return => {
                // FIXME: Check that the return local variable has been initialized
                todo!();
            }
        }
    }
}

struct TypeckEnv {
    env: Env,

    /// The declared return type from the function signature.
    output_ty: Ty,

    /// Type of each local variable, as declared.
    local_variables: Map<LocalId, Ty>,
}
