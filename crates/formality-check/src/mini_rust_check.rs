use std::collections::BTreeSet;
use std::iter::zip;
use std::sync::Arc;

use formality_core::judgment::{ProofTree, Proven};
use formality_core::{cast_impl, judgment_fn, set, Downcast, Fallible, Map, Set, Upcast};
use formality_prove::{prove_normalize, AdtDeclBoundData, AdtDeclVariant, Constraints, Decls, Env};
use formality_rust::grammar::minirust::ArgumentExpression::{ByValue, InPlace};
use formality_rust::grammar::minirust::ValueExpression::{Constant, Fn, Load, Ref, Struct};
use formality_rust::grammar::minirust::{
    self, ArgumentExpression, BasicBlock, BbId, LocalId, PlaceExpression, ValueExpression,
};
use formality_rust::grammar::minirust::{BodyBound, PlaceExpression::*};
use formality_rust::grammar::Program;
use formality_types::grammar::{
    AdtId, Binder, CrateId, Parameter, Relation, RigidName, RigidTy, Ty, TyData, VariantId, Wcs,
};
use formality_types::rust::Fold;

use crate::borrow_check::nll;
use crate::{Check, CrateItem, Debug, ProvenSet, ToWcs, Visit};
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
    ) -> Fallible<ProofTree> {
        // ----------------------------------------------------------------------
        // Type check the fn signature

        let output_ty: Ty = output_ty.upcast();
        let mut proof_tree = ProofTree::new(format!("check_body"), None, vec![]);

        //  Check that all the types declared for each local variable are well-formed
        for lv in &body.params {
            proof_tree.children.push(self.prove_goal(
                &env,
                &fn_assumptions,
                lv.ty.well_formed(),
            )?);
        }

        let parameters: Map<LocalId, Ty> = body
            .params
            .iter()
            .map(|lv| (lv.id.clone(), lv.ty.clone()))
            .collect();

        // Check whether the local_id in function arguments are declared.
        for function_arg_id in &body.args {
            if !parameters.contains_key(function_arg_id) {
                bail!("Function argument {:?} is not declared, consider declaring them with `let {:?}: type;`", function_arg_id, function_arg_id);
            }
        }
        let Some(body_ret_ty) = parameters.get(&body.ret) else {
            bail!("return variable {:?} is not declared, consider declaring them with `let {:?}: type;`", body.ret, body.ret);
        };

        // Check if the actual return type is the subtype of the declared return type.
        proof_tree.children.push(self.prove_goal(
            &env,
            fn_assumptions,
            Relation::sub(body_ret_ty, &output_ty),
        )?);

        // ----------------------------------------------------------------------
        // Type check the fn body

        // Create inference variables for each of the regions bound in the MIR body.
        let mut env = env.clone();
        let BodyBound { locals, blocks } = env.instantiate_existentially(&body.binder);
        let blocks = Arc::new(blocks);

        // Create list of local variables, beginning with the parameters, and then adding in the
        // local function variables that (may) reference existential lifetimes.
        let local_variables: Map<LocalId, Ty> = parameters
            .into_iter()
            .chain(locals.iter().map(|lv| (lv.id.clone(), lv.ty.clone())))
            .collect();

        // Check whether the number of declared function parameters matches the number of arguments provided.
        if declared_input_tys.len() != body.args.len() {
            bail!(
                "Function argument number mismatch: expected {} arguments, but found {}",
                declared_input_tys.len(),
                body.args.len()
            );
        }

        let mut env = TypeckEnv {
            program: Arc::new(self.program.clone()),
            env: env.clone(),
            output_ty,
            local_variables,
            blocks: blocks.clone(),
            ret_id: body.ret,
            declared_input_tys,
            crate_id: crate_id.clone(),
            fn_args: body.args.clone(),
            decls: self.decls.clone(),
        };

        // Check that basic blocks are well-typed
        let mut pending_outlives = set![];
        for block in &*blocks {
            proof_tree.children.push(env.check_block(
                fn_assumptions,
                block,
                &mut pending_outlives,
            )?);
        }

        proof_tree
            .children
            .push(nll::borrow_check(&env, &fn_assumptions, &pending_outlives)?);

        Ok(proof_tree)
    }
}

impl TypeckEnv {
    fn check_block(
        &mut self,
        fn_assumptions: &Wcs,
        block: &minirust::BasicBlock,
        pending_outlives: &mut Set<PendingOutlives>,
    ) -> Fallible<ProofTree> {
        let mut proof_tree = ProofTree::new(format!("check_block({:?})", block.id), None, vec![]);

        for statement in &block.statements {
            let (new_outlives, pt) = check_statement(self.clone(), fn_assumptions.clone(), statement.clone())
                .into_singleton()?;
            pending_outlives.extend(new_outlives);
            proof_tree.children.push(pt);
        }

        proof_tree.children.push(self.check_terminator(
            fn_assumptions,
            &block.terminator,
            pending_outlives,
        )?);

        Ok(proof_tree)
    }
}

judgment_fn! {
    fn check_statement(
        env: TypeckEnv,
        fn_assumptions: Wcs,
        statement: minirust::Statement,
    ) => Set<PendingOutlives> {
        debug(statement, fn_assumptions, env)

        (
            (let place_ty = env.check_place(&fn_assumptions, &place, &mut set![])?)
            (check_value(&env, &fn_assumptions, &value) => (value_ty, value_outlives))
            (env.prove_goal(Location, &fn_assumptions, Relation::sub(value_ty.clone(), place_ty.clone())) => sub_outlives)
            (let outlives = { let mut s = value_outlives.clone(); s.extend(sub_outlives); s })
            --- ("assign")
            (check_statement(env, fn_assumptions, minirust::Statement::Assign(place, value)) => outlives)
        )

        (
            (let _place_ty = env.check_place(&fn_assumptions, &place, &mut set![])?)
            --- ("place-mention")
            (check_statement(env, fn_assumptions, minirust::Statement::PlaceMention(place)) => Set::<PendingOutlives>::new())
        )

        (
            (if env.find_local_id(&local_id).is_some())
            --- ("storage-live")
            (check_statement(env, _fn_assumptions, minirust::Statement::StorageLive(local_id)) => Set::<PendingOutlives>::new())
        )

        (
            (if let Some((local_id, _)) = env.find_local_id(&local_id))
            (if local_id != env.ret_id)
            (if !env.fn_args.iter().any(|fn_arg| local_id == *fn_arg))
            --- ("storage-dead")
            (check_statement(env, _fn_assumptions, minirust::Statement::StorageDead(local_id)) => Set::<PendingOutlives>::new())
        )
    }
}

judgment_fn! {
    fn check_value(
        env: TypeckEnv,
        fn_assumptions: Wcs,
        value: ValueExpression,
    ) => (Ty, Set<PendingOutlives>) {
        debug(value, fn_assumptions, env)

        (
            (let place_ty = env.check_place(&fn_assumptions, &place, &mut set![])?)
            --- ("load")
            (check_value(env, fn_assumptions, Load(place)) => (place_ty, Set::<PendingOutlives>::new()))
        )

        (
            (let curr_crate = env.program.crates.iter().find(|c| c.id == env.crate_id).unwrap())
            (let fn_declared = curr_crate.items.iter().find_map(|item| match item {
                CrateItem::Fn(fn_declared) if fn_declared.id == fn_id => Some(fn_declared),
                _ => None,
            }))
            (if let Some(fn_declared) = fn_declared)
            (let value_ty = Ty::rigid(RigidName::FnDef(fn_declared.id.clone()), Vec::<Parameter>::new()))
            --- ("fn")
            (check_value(env, fn_assumptions, Fn(fn_id)) => (value_ty, Set::<PendingOutlives>::new()))
        )

        (
            (let value_ty = constant.get_ty())
            --- ("constant")
            (check_value(env, fn_assumptions, Constant(constant)) => (value_ty, Set::<PendingOutlives>::new()))
        )

        (
            (env.prove_goal(Location, &fn_assumptions, ty.well_formed()) => wf_outlives)
            (if let Some(adt_id) = ty.get_adt_id())
            (let (_, AdtDeclBoundData { where_clause: _, variants }) = env.decls.adt_decl(&adt_id).binder.open())
            (let AdtDeclVariant { name, fields } = variants.last().unwrap())
            (if *name == VariantId::for_struct())
            (let struct_field_tys: Vec<Ty> = fields.iter().map(|field| field.ty.clone()).collect())
            (if value_expressions.len() == struct_field_tys.len())
            (for_all(value_expression in &value_expressions)
                (if let Constant(_) = value_expression)
                (check_value(&env, &fn_assumptions, value_expression) => (_ty, _outlives)))
            (let value_tys: Vec<Ty> = value_expressions.iter().map(|v| match v { Constant(c) => c.get_ty(), _ => unreachable!() }).collect())
            (env.prove_goal(Location, &fn_assumptions, Wcs::all_sub(value_tys, struct_field_tys)) => sub_outlives)
            (let outlives = { let mut s = wf_outlives.clone(); s.extend(sub_outlives); s })
            --- ("struct")
            (check_value(env, fn_assumptions, Struct(value_expressions, ty)) => (ty.clone(), outlives))
        )

        (
            (let place_ty = env.check_place(&fn_assumptions, &place_expr, &mut set![])?)
            (let value_ty = place_ty.ref_ty_of_kind(ref_kind, &borrow_lt))
            --- ("ref")
            (check_value(env, fn_assumptions, Ref(ref_kind, borrow_lt, place_expr)) => (value_ty, Set::<PendingOutlives>::new()))
        )
    }
}

impl TypeckEnv {
    fn check_terminator(
        &mut self,
        fn_assumptions: &Wcs,
        terminator: &minirust::Terminator,
        pending_outlives: &mut Set<PendingOutlives>,
    ) -> Fallible<ProofTree> {
        let mut proof_tree =
            ProofTree::new(format!("check_terminator({terminator:?})"), None, vec![]);

        match terminator {
            minirust::Terminator::Goto(bb_ids) => {
                // Check that the basic block `bb_id` exists.
                for bb_id in bb_ids {
                    self.check_block_exists(bb_id)?;
                }

                if bb_ids.len() == 0 {
                    // Idle thought that will be lost to the sands of time:
                    //
                    // Maybe we should allow this but it must be dead code or unreachable or something?!
                    bail!("Terminator::Goto must have at least one target basic block.")
                }
            }
            minirust::Terminator::Call {
                callee,
                generic_arguments: _,
                arguments: actual_arguments,
                ret,
                next_block,
            } => {
                // Function is part of the value expression, so we will check if the function exists in check_value.
                let (callee_ty, callee_pt) =
                    self.check_value(fn_assumptions, callee, pending_outlives)?;
                proof_tree.children.push(callee_pt);

                // Get argument information from the callee type.
                let Fn(_callee_fn_id) = callee else {
                    unreachable!("Callee must exists in Terminator::Call");
                };

                // Extract function signature from FnDef type
                let TyData::RigidTy(rigid_ty) = callee_ty.data() else {
                    bail!("Expected FnDef type for function value");
                };
                let RigidName::FnDef(fn_id) = &rigid_ty.name else {
                    bail!("Expected FnDef type for function value");
                };

                // Find the function declaration to get the signature
                let curr_crate = self
                    .program
                    .crates
                    .iter()
                    .find(|c| c.id == self.crate_id)
                    .unwrap();

                let fn_declared = curr_crate
                    .items
                    .iter()
                    .find_map(|item| match item {
                        CrateItem::Fn(fn_declared) if fn_declared.id == *fn_id => Some(fn_declared),
                        _ => None,
                    })
                    .unwrap();

                let fn_bound_data = self.env.instantiate_universally(&fn_declared.binder);
                let callee_declared_input_tys = fn_bound_data.input_tys.clone();

                // Check if the numbers of arguments passed equals to number of arguments declared.
                if callee_declared_input_tys.len() != actual_arguments.len() {
                    bail!("Function arguments number mismatch: the number expected is {:?}, the actual number is {:?}", callee_declared_input_tys.len(), actual_arguments.len());
                }

                let arguments = zip(callee_declared_input_tys, actual_arguments);
                for (declared_ty, actual_argument) in arguments {
                    // Check if the arguments are well formed.
                    let (actual_ty, arg_pt) = self.check_argument_expression(
                        fn_assumptions,
                        actual_argument,
                        pending_outlives,
                    )?;
                    proof_tree.children.push(arg_pt);
                    // Check if the actual argument type passed in is the subtype of expect argument type.
                    let (new_outlives, pt) = self
                        .prove_goal(Location, fn_assumptions, Relation::sub(&actual_ty, &declared_ty))
                        .into_singleton()?;
                    pending_outlives.extend(new_outlives);
                    proof_tree.children.push(pt);
                }

                // Check whether ret place is well-formed.
                let actual_return_ty = self.check_place(fn_assumptions, ret, pending_outlives)?;

                // Check if the fn's declared return type is a subtype of the type of the local variable `ret`
                let (new_outlives, pt) = self
                    .prove_goal(
                        Location,
                        fn_assumptions,
                        Relation::sub(&fn_bound_data.output_ty, &actual_return_ty),
                    )
                    .into_singleton()?;
                pending_outlives.extend(new_outlives);
                proof_tree.children.push(pt);

                // Check the validity of next bb_id.
                if let Some(bb_id) = next_block {
                    self.check_block_exists(bb_id)?;
                };
            }
            minirust::Terminator::Return => {
                // TODO: Check if the return local variable has been initialized (filed as issue)
                // if !self.ret_place_is_initialised {
                //     bail!("The return local variable has not been initialized.")
                // }
            }

            minirust::Terminator::Switch {
                switch_value,
                switch_targets,
                fallback,
            } => {
                // Check if the value is well-formed.
                let (value_ty, value_pt) =
                    self.check_value(fn_assumptions, switch_value, pending_outlives)?;
                proof_tree.children.push(value_pt);

                let (new_outlives, pt) = self
                    .prove_judgment(Location, &fn_assumptions, value_ty, ty_is_int)
                    .into_singleton()?;
                pending_outlives.extend(new_outlives);
                proof_tree.children.push(pt);

                // Ensure all bbid are valid.
                for switch_target in switch_targets {
                    self.check_block_exists(&switch_target.target)?;
                }
                self.check_block_exists(fallback)?;
            }
        }
        Ok(proof_tree)
    }

    /// Hacky method that type-checks a place that has already been type-checked.
    /// Asserts therefore that the resulting pending outlives were already pending.
    ///
    /// I am a horrible monster and I pray for death, but I have no other option. --nikomatsakis
    pub(crate) fn check_place_hackola(
        &self,
        fn_assumptions: &Wcs,
        place: &PlaceExpression,
        outlives: &Set<PendingOutlives>,
    ) -> Fallible<Ty> {
        let mut new_outlives = set![];
        let ty = self.check_place(fn_assumptions, place, &mut new_outlives)?;
        for constraint in new_outlives {
            if !outlives.contains(&constraint) {
                panic!(
                    "unexpected outlives constraint generated during check_place: {:?}",
                    constraint
                );
            }
        }
        Ok(ty)
    }

    // Check if the place expression is well-formed, and return the type of the place expression.
    pub(crate) fn check_place(
        &self,
        fn_assumptions: &Wcs,
        place: &PlaceExpression,
        pending_outlives: &mut Set<PendingOutlives>,
    ) -> Fallible<Ty> {
        let place_ty;
        match place {
            Local(local_id) => {
                // Check if place id is a valid local id.
                let Some((_, ty)) = self.find_local_id(local_id) else {
                    bail!(
                        "PlaceExpression::Local: unknown local name `{:?}`",
                        local_id
                    )
                };
                place_ty = ty;
            }
            Field(field_projection) => {
                let ty = self
                    .check_place(fn_assumptions, &field_projection.root, pending_outlives)
                    .unwrap();

                // FIXME(merge-typeck-borrowck): We eventually want to do normalization here, so check_place should be
                // a judgment fn. This should be done after we merge mir typeck and borrowck together.
                let Some(adt_id) = ty.get_adt_id() else {
                    bail!("The local used for field projection is not adt.")
                };

                let (
                    _,
                    AdtDeclBoundData {
                        where_clause: _,
                        variants,
                    },
                ) = self.decls.adt_decl(&adt_id).binder.open();
                let AdtDeclVariant { name, fields } = variants.last().unwrap();

                if *name != VariantId::for_struct() {
                    bail!("The local used for field projection must be struct.")
                }

                // Check if the index is valid for the tuple.
                if field_projection.index >= fields.len() {
                    bail!("The field index used in PlaceExpression::Field is invalid.")
                }

                place_ty = fields[field_projection.index].ty.clone();
            }
            Deref(value_expr) => match self
                .check_place(fn_assumptions, value_expr, pending_outlives)?
                .data()
            {
                TyData::RigidTy(rigid_ty) => match &rigid_ty.name {
                    RigidName::Ref(_ref_kind) => {
                        place_ty = rigid_ty.parameters[1]
                            .as_ty()
                            .expect("well-kinded reference")
                            .clone();
                    }
                    RigidName::AdtId(adt_id) => {
                        if *adt_id == AdtId::new("Box") {
                            todo!("box magic")
                        }
                        bail!("cannot deref an ADT of type {adt_id:?}")
                    }
                    RigidName::ScalarId(_)
                    | RigidName::Tuple(_)
                    | RigidName::FnPtr(_)
                    | RigidName::FnDef(_) => {
                        bail!("cannot deref a value of type {rigid_ty:?}")
                    }
                },

                TyData::AliasTy(_alias_ty) => todo!("alias normalization"),
                TyData::PredicateTy(_predicate_ty) => todo!("predicate types"),
                TyData::Variable(_core_variable) => panic!("type inference variables unexpected"),
            },
        }
        Ok(place_ty.clone())
    }

    fn find_local_id(&self, local_id: &LocalId) -> Option<(LocalId, Ty)> {
        if let Some((local_id, ty)) = self
            .local_variables
            .iter()
            .find(|(declared_local_id, _)| *declared_local_id == local_id)
        {
            return Some((local_id.clone(), ty.clone()));
        }
        return None;
    }

    // Check if the value expression is well-formed, and return the type of the value expression.
    fn check_value(
        &self,
        fn_assumptions: &Wcs,
        value: &ValueExpression,
        pending_outlives: &mut Set<PendingOutlives>,
    ) -> Fallible<Proven<Ty>> {
        let mut proof_tree = ProofTree::new(format!("check_value({value:?})"), None, vec![]);
        let value_ty;
        match value {
            Load(place_expression) => {
                value_ty = self.check_place(fn_assumptions, place_expression, pending_outlives)?;
            }
            Fn(fn_id) => {
                // Check if the function called is in declared in current crate.

                // Find the crate that is currently being typeck.
                let curr_crate = self
                    .program
                    .crates
                    .iter()
                    .find(|c| c.id == self.crate_id)
                    .unwrap();

                // Find the callee from current crate.
                let fn_declared = curr_crate.items.iter().find_map(|item| match item {
                    CrateItem::Fn(fn_declared) if fn_declared.id == *fn_id => Some(fn_declared),
                    _ => None,
                });

                let Some(fn_declared) = fn_declared else {
                    bail!("The function called is not declared in current crate")
                };

                // Return FnDef type
                value_ty = Ty::rigid(
                    RigidName::FnDef(fn_declared.id.clone()),
                    Vec::<Parameter>::new(), // TODO: should extract parameters from binder
                );
            }
            Constant(constant) => {
                // If the actual value overflows / does not match the type of the constant,
                // it will be rejected by the parser.
                value_ty = constant.get_ty();
            }
            Struct(value_expressions, ty) => {
                // Check if the adt is well-formed.
                let (new_outlives, pt) = self
                    .prove_goal(Location, &fn_assumptions, ty.well_formed())
                    .into_singleton()?;
                pending_outlives.extend(new_outlives);
                proof_tree.children.push(pt);

                let Some(adt_id) = ty.get_adt_id() else {
                    bail!("The type used in ValueExpression::Struct must be adt")
                };

                // Make sure that the adt is struct.
                let (
                    _,
                    AdtDeclBoundData {
                        where_clause: _,
                        variants,
                    },
                ) = self.decls.adt_decl(&adt_id).binder.open();

                let AdtDeclVariant { name, fields } = variants.last().unwrap();

                if *name != VariantId::for_struct() {
                    bail!("This type used in ValueExpression::Struct should be a struct")
                }

                // Check if the number of value provided match the number of field.
                let struct_field_tys: Vec<Ty> =
                    fields.iter().map(|field| field.ty.clone()).collect();

                if value_expressions.len() != struct_field_tys.len() {
                    bail!("The length of ValueExpression::Tuple does not match the type of the ADT declared")
                }

                let mut value_tys: Vec<Ty> = Vec::new();

                for value_expression in value_expressions {
                    let Constant(_) = value_expression else {
                        bail!("Only Constant is supported in ValueExpression::Struct for now.")
                    };
                    let (ty, pt) =
                        self.check_value(fn_assumptions, value_expression, pending_outlives)?;
                    value_tys.push(ty);
                    proof_tree.children.push(pt);
                }

                // Make sure all the types supplied are the subtype of declared types.
                let (new_outlives, pt) = self
                    .prove_goal(Location, &fn_assumptions, Wcs::all_sub(value_tys, struct_field_tys))
                    .into_singleton()?;
                pending_outlives.extend(new_outlives);
                proof_tree.children.push(pt);

                value_ty = ty.clone();
            }
            Ref(ref_kind, borrow_lt, place_expr) => {
                let place_ty = self.check_place(fn_assumptions, place_expr, pending_outlives)?;
                value_ty = place_ty.ref_ty_of_kind(*ref_kind, borrow_lt);
            }
        }
        Ok((value_ty, proof_tree))
    }

    fn check_argument_expression(
        &self,
        fn_assumptions: &Wcs,
        arg_expr: &ArgumentExpression,
        pending_outlives: &mut Set<PendingOutlives>,
    ) -> Fallible<Proven<Ty>> {
        match arg_expr {
            ByValue(val_expr) => self.check_value(fn_assumptions, val_expr, pending_outlives),
            InPlace(place_expr) => {
                let ty = self.check_place(fn_assumptions, place_expr, pending_outlives)?;
                Ok((
                    ty,
                    ProofTree::new(
                        format!("check_argument_expression({arg_expr:?})"),
                        None,
                        vec![],
                    ),
                ))
            }
        }
    }

    fn check_block_exists(&self, id: &BbId) -> Fallible<()> {
        for block in self.blocks.iter() {
            if *id == block.id {
                return Ok(());
            }
        }
        bail!("Basic block {:?} does not exist", id)
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Clone, Hash)]
pub struct TypeckEnv {
    /// Program being typechecked that contains this functon
    pub program: Arc<Program>,

    /// The environment (set of universal, existential variables)
    pub env: Env,

    /// The declared return type from the function signature.
    pub output_ty: Ty,

    /// Type of each local variable, as declared.
    pub local_variables: Map<LocalId, Ty>,

    /// All basic blocks of current body.
    pub blocks: Arc<Vec<BasicBlock>>,

    /// local_id of return place,
    pub ret_id: LocalId,

    /// All declared argument type of current function.
    pub declared_input_tys: Vec<Ty>,

    /// The id of the crate where this function resides.
    /// We need this to access information about other functions
    /// declared in the current crate.
    pub crate_id: CrateId,

    /// LocalId of function argument.
    pub fn_args: Vec<LocalId>,

    pub decls: Decls,
}

impl TypeckEnv {
    pub fn basic_block(&self, bb_id: &BbId) -> Fallible<&BasicBlock> {
        Ok(self
            .blocks
            .iter()
            .find(|bb| bb.id == *bb_id)
            .ok_or_else(|| anyhow::anyhow!("Basic block {:?} not found", bb_id))?)
    }
}

cast_impl!(TypeckEnv);

/// A pending outlives constraint that we incurred during typechecking.
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Hash)]
pub struct PendingOutlives {
    /// The location where this outlives obligation was incurred.
    pub location: Location,

    /// The `a` in `a: b`
    pub a: Parameter,

    /// The `b` in `a: b`
    pub b: Parameter,
}

cast_impl!(PendingOutlives);

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Hash)]
pub struct Location;

impl TypeckEnv {
    /// Prove the goal in this environment, returning any pending outlive constraints that are required
    /// for the goal to be true.
    fn prove_goal(
        &self,
        location: Location,
        assumptions: impl ToWcs,
        goal: impl ToWcs + Debug,
    ) -> ProvenSet<Set<PendingOutlives>> {
        let goal: Wcs = goal.to_wcs();
        self.prove_judgment(location, assumptions, goal.to_wcs(), formality_prove::prove)
    }

    /// Prove the goal with the function `judgment_fn`,
    /// returning the pending outlive constraints that are required
    /// for the goal to be true.
    ///
    /// One of the difference between this prove_judgment and the one in impl Check is
    /// that this version can accept existential variable, which is needed for handling lifetime.
    /// In the compiler, we insert existential variables for all
    /// lifetimes that appear in the MIR body, and I expect we will do the same here.
    fn prove_judgment<G>(
        &self,
        location: Location,
        assumptions: impl ToWcs,
        goal: G,
        judgment_fn: impl FnOnce(Decls, Env, Wcs, G) -> ProvenSet<Constraints>,
    ) -> ProvenSet<Set<PendingOutlives>>
    where
        G: Debug + Visit + Clone,
    {
        let assumptions: Wcs = assumptions.to_wcs();

        assert!(self.env.encloses((&assumptions, &goal)));

        // Prove the goal using the given judgment + assumptions.
        // We allow pending outlives so that outlives constraints can be deferred
        // and later verified by the borrow checker.
        let cs = judgment_fn(
            self.decls.clone(),
            self.env.with_allow_pending_outlives(true),
            assumptions.clone(),
            goal.clone(),
        );

        // Each member `c \in cs` is a valid way corresponds
        // to a set of constraints which, if true, mean the goal is true.
        //
        // i.e., `\forall c \in cs. (c => (assumptions => goal))`
        let cs = match cs.into_map() {
            Ok(cs) => cs,
            Err(e) => return ProvenSet::from(*e),
        };

        // The set of constraints is always non-empty or else the judgment is considered to have failed.
        assert!(!cs.is_empty());

        // If there is anything *unconditionally true*, that's great
        if let Some((_, proof_tree)) = cs.iter().find(|(c, _)| c.unconditionally_true()) {
            return ProvenSet::singleton((set![], proof_tree.clone()));
        }

        // Each `c` in `cs` is a set of [`Constraints`][] that, if they hold,
        // imply the judgment is true. These are all independent. In the trait
        // solver, when we have multiple choices, we explore them ALL, simulating
        // nondeterministic choice. This is what judgment functions do.
        // But we don't want to do that here, we are writing
        // Rust code that is deterministic and only explores a single option.
        // This matches the compiler's behavior, where the trait solver picks the "best"
        // choice of the options it can see, and returns those constraints to the
        // type checker. In our version, the trait solver gives *all* the constraints
        // and the type checker decices the "best" choice. Same basic idea.
        //
        // So which one do we want? We can take any `c` in `cs` and it will be *sound*
        // but it may not be *complete*. In particular, if it has more constraints than
        // are necessary, it could lead us to conclude that the code does not type
        // check -- when it COULD have type checked if we had picked a better choice.
        //
        // Example: suppose that `cs` returns two choices: {
        //     [], // unconditionally true
        //     ['a: 'b], // only true if 'a: 'b
        // }
        //
        // If we pick the second one, and `'a: 'b` does not hold, we get an error
        // in the borrow checker later on. But it wasn't necessary.\
        //
        // So what we do is we look for a *minimal result* -- if there isn't one,
        // for now, we fail with ambiguity.

        // Convert these to pending-outlives; if conversion fails, bail because we don't know
        // whether it would be more minimal than the others or not.
        let mut pending_outlives_sets: Vec<(Set<PendingOutlives>, ProofTree)> = vec![];
        for (c, proof_tree) in &cs {
            // Ignore constraints that only prove the goal "might" be true, irrelevant here
            if !c.known_true {
                continue;
            }

            // We already filtered this above
            assert!(c.known_true);

            // We don't have any existential variables, so there can't be a substitution
            assert!(c.substitution().is_empty());

            match self.convert_to_pending_outlives(&location, c) {
                Some(p_o) => pending_outlives_sets.push((p_o, proof_tree.clone())),
                None => {
                    return ProvenSet::failed(
                        format!("prove_judgment({goal:?})"),
                        format!("failed to convert `{c:?}` to pending-outlives"),
                    );
                }
            }
        }

        // Find the minimal set of the remaining solutions.
        let mut pending_outlives_iter = pending_outlives_sets.into_iter();
        let Some(mut pending_outlives_minimal) = pending_outlives_iter.next() else {
            return ProvenSet::failed(
                format!("prove_judgment({goal:?})"),
                format!("final constraint set had only ambiguous elements: {cs:#?}"),
            );
        };
        for pending_outlives in pending_outlives_iter {
            // If these outlives constraints are not ordered with respect to `pending_outlives_minimal`, then bail.
            if !pending_outlives.0.is_subset(&pending_outlives_minimal.0)
                && !pending_outlives_minimal.0.is_subset(&pending_outlives.0)
            {
                return ProvenSet::failed(
                    format!("prove_judgment({goal:?})"),
                    format!(
                        "no relationship between `{pending_outlives:?}` and `{pending_outlives_minimal:?}`"
                    ),
                );
            }

            // If this set of outlives is a subset of the previous minimal, then use it instead.
            if pending_outlives.0.is_subset(&pending_outlives_minimal.0) {
                pending_outlives_minimal = pending_outlives;
            }
        }

        ProvenSet::singleton(pending_outlives_minimal)
    }

    // Convert the pending goals into a series of `PendingOutlives`.
    //
    // The `Constraints` struct contains a set of "pending where-clauses"
    // which must still be proven. In practice, the final result of the
    // top-level judgments we use in the type checker should only have
    // pending outlives requests. This function checks that this is true,
    // converting to a set of `PendingOutlives`, and returns `None` if
    // any other sort of where-clause is found.
    fn convert_to_pending_outlives(
        &self,
        location: &Location,
        c: &Constraints,
    ) -> Option<BTreeSet<PendingOutlives>> {
        let mut c_outlives = BTreeSet::default();

        for pending in c.env.pending() {
            match pending.downcast::<Relation>() {
                Some(Relation::Outlives(a, b)) => {
                    c_outlives.insert(PendingOutlives {
                        location: location.clone(),
                        a,
                        b,
                    });
                }

                _ => {
                    // give up
                    return None;
                }
            }
        }

        Some(c_outlives)
    }

    /// Instantiate the given binder universally in this environment,
    pub fn instantiate_universally<T>(&self, binder: &Binder<T>) -> (T, TypeckEnv)
    where
        T: Fold + Clone,
    {
        let mut env = self.env.clone();
        let value = env.instantiate_universally(binder);
        (
            value,
            TypeckEnv {
                env,
                ..self.clone()
            },
        )
    }
}

judgment_fn! {
    fn ty_is_int(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        ty: Parameter,
    ) => Constraints {
        debug(assumptions, ty, env)
        // If the type that we are currently checking is rigid, check if it is an int.
        // If the type can be normalized, normalize until rigid then check if it is an int.
        // For the rest of the case, it should fail.

        (
            (prove_normalize(&decl, &env, &assumptions, ty) => (c1, p))!
            (let assumptions = c1.substitution().apply(&assumptions))
            (ty_is_int(&decl, &env, assumptions, p) => c2)
            ----------------------------- ("alias_ty is int")
            (ty_is_int(decl, env, assumptions, ty) => c2)
        )

        (
            (if id.is_int())
            ----------------------------- ("rigid_ty is int")
            (ty_is_int(_decls, _env, _assumptions, RigidTy {name: RigidName::ScalarId(id), parameters: _}) => Constraints::none(env))
        )

    }
}

mod test;
