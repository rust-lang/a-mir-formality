use std::collections::BTreeSet;
use std::sync::Arc;

use formality_core::judgment::ProofTree;
use formality_core::{Downcast, Fallible, Map, Set, Upcast, cast_impl, judgment_fn};
use formality_prove::{prove_normalize, AdtDeclBoundData, AdtDeclVariant, Constraints, Decls, Env};
use formality_rust::grammar::minirust::ArgumentExpression::{ByValue, InPlace};
use formality_rust::grammar::minirust::ValueExpression::{Constant, Fn, Load, Ref, Struct};
use formality_rust::grammar::minirust::{
    self, ArgumentExpression, BasicBlock, BbId, LocalId, PlaceExpression, ValueExpression,
};
use formality_rust::grammar::minirust::{BodyBound, PlaceExpression::*};
use formality_rust::grammar::{Fn as FnDecl, Program};
use formality_types::grammar::{
    AdtId, Binder, CrateId, FnId, Parameter, Relation, RigidName, RigidTy, Ty, TyData, VariantId,
    Wcs,
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
        let (pending_outlives, blocks_pt) = check_blocks(
            env.clone(),
            (),
            fn_assumptions.clone(),
            (*blocks).clone(),
        ).into_singleton()?;
        proof_tree.children.push(blocks_pt);

        proof_tree
            .children
            .push(nll::borrow_check(&env, &fn_assumptions, &pending_outlives)?);

        Ok(proof_tree)
    }
}

judgment_fn! {
    fn check_blocks(
        env: TypeckEnv,
        outlives: Set<PendingOutlives>,
        fn_assumptions: Wcs,
        blocks: Vec<minirust::BasicBlock>,
    ) => Set<PendingOutlives> {
        debug(blocks, fn_assumptions, env, outlives)

        (
            // Check all blocks
            (for_all(block in &blocks) with(outlives)
                (check_block(&env, outlives, &fn_assumptions, block) => outlives))
            --- ("blocks")
            (check_blocks(env, outlives, fn_assumptions, blocks) => outlives)
        )
    }
}

judgment_fn! {
    fn check_block(
        env: TypeckEnv,
        outlives: Set<PendingOutlives>,
        fn_assumptions: Wcs,
        block: minirust::BasicBlock,
    ) => Set<PendingOutlives> {
        debug(block, fn_assumptions, env, outlives)

        (
            // Check all statements
            (for_all(statement in &block.statements) with(outlives)
                (check_statement(&env, outlives, &fn_assumptions, statement) => outlives))

            // Check terminator
            (check_terminator(&env, outlives, &fn_assumptions, &block.terminator) => outlives)
            --- ("block")
            (check_block(env, outlives, fn_assumptions, block) => outlives)
        )
    }
}

judgment_fn! {
    fn check_statement(
        env: TypeckEnv,
        outlives: Set<PendingOutlives>,
        fn_assumptions: Wcs,
        statement: minirust::Statement,
    ) => Set<PendingOutlives> {
        debug(statement, fn_assumptions, env, outlives)

        (
            (check_place(&env, outlives, &fn_assumptions, &place) => (place_ty, outlives))
            (check_value(&env, outlives, &fn_assumptions, &value) => (value_ty, outlives))
            (env.prove_goal(outlives, Location, &fn_assumptions, Relation::sub(value_ty.clone(), place_ty.clone())) => outlives)
            --- ("assign")
            (check_statement(env, outlives, fn_assumptions, minirust::Statement::Assign(place, value)) => outlives)
        )

        (
            (check_place(&env, outlives, &fn_assumptions, &place) => (_place_ty, outlives))
            --- ("place-mention")
            (check_statement(env, outlives, fn_assumptions, minirust::Statement::PlaceMention(place)) => outlives)
        )

        (
            (if env.find_local_id(&local_id).is_some())
            --- ("storage-live")
            (check_statement(env, outlives, _fn_assumptions, minirust::Statement::StorageLive(local_id)) => outlives)
        )

        (
            (if let Some((local_id, _)) = env.find_local_id(&local_id))
            (if local_id != env.ret_id)
            (if !env.fn_args.iter().any(|fn_arg| local_id == *fn_arg))
            --- ("storage-dead")
            (check_statement(env, outlives, _fn_assumptions, minirust::Statement::StorageDead(local_id)) => outlives)
        )
    }
}

judgment_fn! {
    fn check_value(
        env: TypeckEnv,
        outlives: Set<PendingOutlives>,
        fn_assumptions: Wcs,
        value: ValueExpression,
    ) => (Ty, Set<PendingOutlives>) {
        debug(value, fn_assumptions, env, outlives)

        (
            (check_place(&env, outlives, &fn_assumptions, &place) => (place_ty, outlives))
            --- ("load")
            (check_value(env, outlives, fn_assumptions, Load(place)) => (place_ty, outlives))
        )

        (
            (if let Some(fn_decl) = env.fn_decl(&fn_id))
            (let value_ty = Ty::rigid(RigidName::FnDef(fn_decl.id.clone()), Vec::<Parameter>::new()))
            --- ("fn")
            (check_value(env, outlives, _fn_assumptions, Fn(fn_id)) => (value_ty, outlives))
        )

        (
            (let value_ty = constant.get_ty())
            --- ("constant")
            (check_value(_env, outlives, _fn_assumptions, Constant(constant)) => (value_ty, outlives))
        )

        (
            (env.prove_goal(outlives, Location, &fn_assumptions, ty.well_formed()) => outlives)
            (if let Some((adt_id, parameters)) = ty_is_adt(&ty))
            (let AdtDeclBoundData { where_clause: _, variants } = env.decls.adt_decl(&adt_id).binder.instantiate_with(&parameters)?)
            (let AdtDeclVariant { name, fields } = variants.last().unwrap())
            (if *name == VariantId::for_struct())
            (if value_expressions.len() == fields.len())
            (for_all(pair in value_expressions.iter().zip(fields)) with(outlives)
                (let (value_expression, field) = pair)
                (check_value(&env, outlives, &fn_assumptions, value_expression) => (value_ty, outlives))
                (env.prove_goal(outlives, Location, &fn_assumptions, Relation::sub(value_ty, &field.ty)) => outlives))
            --- ("struct")
            (check_value(env, outlives, fn_assumptions, Struct(value_expressions, ty)) => (ty.clone(), outlives))
        )

        (
            (check_place(&env, outlives, &fn_assumptions, &place_expr) => (place_ty, outlives))
            (let value_ty = place_ty.ref_ty_of_kind(ref_kind, &borrow_lt))
            --- ("ref")
            (check_value(env, outlives, fn_assumptions, Ref(ref_kind, borrow_lt, place_expr)) => (value_ty, outlives))
        )
    }
}

judgment_fn! {
    fn check_place(
        env: TypeckEnv,
        outlives: Set<PendingOutlives>,
        fn_assumptions: Wcs,
        place: PlaceExpression,
    ) => (Ty, Set<PendingOutlives>) {
        debug(place, fn_assumptions, env, outlives)

        (
            (if let Some((_, ty)) = env.find_local_id(&local_id))
            --- ("local")
            (check_place(env, outlives, _fn_assumptions, Local(local_id)) => (ty, outlives))
        )

        (
            (check_place(&env, outlives, &fn_assumptions, &*field_projection.root) => (root_ty, outlives))
            (if let Some((adt_id, parameters)) = ty_is_adt(&root_ty))
            (let AdtDeclBoundData { where_clause: _, variants } = env.decls.adt_decl(&adt_id).binder.instantiate_with(&parameters)?)
            (let AdtDeclVariant { name, fields } = variants.last().unwrap())
            (if *name == VariantId::for_struct())
            (if field_projection.index < fields.len())
            (let place_ty = fields[field_projection.index].ty.clone())
            --- ("field")
            (check_place(env, outlives, fn_assumptions, Field(field_projection)) => (place_ty, outlives))
        )

        (
            (check_place(&env, outlives, &fn_assumptions, &*value_expr) => (inner_ty, outlives))
            (if let TyData::RigidTy(rigid_ty) = inner_ty.data())
            (if let RigidName::Ref(_ref_kind) = &rigid_ty.name)
            (let place_ty = rigid_ty.parameters[1].as_ty().expect("well-kinded reference").clone())
            --- ("deref-ref")
            (check_place(env, outlives, fn_assumptions, Deref(value_expr)) => (place_ty, outlives))
        )
    }
}

judgment_fn! {
    fn check_argument_expression(
        env: TypeckEnv,
        outlives: Set<PendingOutlives>,
        fn_assumptions: Wcs,
        arg_expr: ArgumentExpression,
    ) => (Ty, Set<PendingOutlives>) {
        debug(arg_expr, fn_assumptions, env, outlives)

        (
            (check_value(&env, outlives, &fn_assumptions, &val_expr) => (ty, outlives))
            --- ("by-value")
            (check_argument_expression(env, outlives, fn_assumptions, ByValue(val_expr)) => (ty, outlives))
        )

        (
            (check_place(&env, outlives, &fn_assumptions, &place_expr) => (ty, outlives))
            --- ("in-place")
            (check_argument_expression(env, outlives, fn_assumptions, InPlace(place_expr)) => (ty, outlives))
        )
    }
}

judgment_fn! {
    fn check_terminator(
        env: TypeckEnv,
        outlives: Set<PendingOutlives>,
        fn_assumptions: Wcs,
        terminator: minirust::Terminator,
    ) => Set<PendingOutlives> {
        debug(terminator, fn_assumptions, env, outlives)

        (
            (if !bb_ids.is_empty())
            (for_all(bb_id in &bb_ids) (if env.block_exists(bb_id)))
            --- ("goto")
            (check_terminator(env, outlives, _fn_assumptions, minirust::Terminator::Goto(bb_ids)) => outlives)
        )

        (
            // Check callee value expression
            (check_value(&env, outlives, &fn_assumptions, &callee) => (callee_ty, outlives))

            // Extract FnDef from callee type
            (if let TyData::RigidTy(rigid_ty) = callee_ty.data())
            (if let RigidName::FnDef(fn_id) = &rigid_ty.name)

            // Find the function declaration
            (if let Some(fn_decl) = env.fn_decl(fn_id))

            // Instantiate the function signature universally (returns new env)
            (let (fn_bound_data, _env1) = env.instantiate_universally(&fn_decl.binder))
            (let callee_declared_input_tys = fn_bound_data.input_tys.clone())

            // Check argument count matches
            (if callee_declared_input_tys.len() == actual_arguments.len())

            // Check each argument and subtyping
            (for_all(arg_pair in callee_declared_input_tys.iter().cloned().zip(actual_arguments.iter().cloned()).collect::<Vec<_>>()) with(outlives)
                (let (declared_ty, actual_argument) = arg_pair)
                (check_argument_expression(&env, outlives, &fn_assumptions, &actual_argument) => (actual_ty, outlives))
                (env.prove_goal(outlives, Location, &fn_assumptions, Relation::sub(actual_ty, declared_ty.clone())) => outlives))

            // Check return place
            (check_place(&env, outlives, &fn_assumptions, &ret) => (actual_return_ty, outlives))
            (env.prove_goal(outlives, Location, &fn_assumptions, Relation::sub(&fn_bound_data.output_ty, &actual_return_ty)) => outlives)

            // Check next block exists if present
            (if next_block.as_ref().map_or(true, |bb_id| env.block_exists(bb_id)))
            --- ("call")
            (check_terminator(env, outlives, fn_assumptions, minirust::Terminator::Call {
                callee,
                generic_arguments: _,
                arguments: actual_arguments,
                ret,
                next_block,
            }) => outlives)
        )

        (
            --- ("return")
            (check_terminator(_env, outlives, _fn_assumptions, minirust::Terminator::Return) => outlives)
        )

        (
            // Check switch value
            (check_value(&env, outlives, &fn_assumptions, &switch_value) => (value_ty, outlives))
            (env.prove_judgment(outlives, Location, &fn_assumptions, value_ty, ty_is_int) => outlives)

            // Check all target blocks exist
            (for_all(switch_target in &switch_targets) (if env.block_exists(&switch_target.target)))
            (if env.block_exists(&fallback))
            --- ("switch")
            (check_terminator(env, outlives, fn_assumptions, minirust::Terminator::Switch {
                switch_value,
                switch_targets,
                fallback,
            }) => outlives)
        )
    }
}

impl TypeckEnv {
    /// Look up a function declaration by its id in the current crate.
    fn fn_decl(&self, fn_id: &FnId) -> Option<&FnDecl> {
        let curr_crate = self.program.crates.iter().find(|c| c.id == self.crate_id)?;
        curr_crate.items.iter().find_map(|item| match item {
            CrateItem::Fn(fn_decl) if fn_decl.id == *fn_id => Some(fn_decl),
            _ => None,
        })
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
        let ((ty, new_outlives), _pt) = check_place(
            self.clone(),
            outlives.clone(),
            fn_assumptions.clone(),
            place.clone(),
        ).into_singleton()?;
        for constraint in &new_outlives {
            if !outlives.contains(constraint) {
                panic!(
                    "unexpected outlives constraint generated during check_place: {:?}",
                    constraint
                );
            }
        }
        Ok(ty)
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

    fn check_block_exists(&self, id: &BbId) -> Fallible<()> {
        for block in self.blocks.iter() {
            if *id == block.id {
                return Ok(());
            }
        }
        bail!("Basic block {:?} does not exist", id)
    }

    fn block_exists(&self, id: &BbId) -> bool {
        self.blocks.iter().any(|block| block.id == *id)
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
    /// Prove the goal in this environment, accumulating any pending outlive constraints
    /// onto the input set and returning the result.
    fn prove_goal(
        &self,
        outlives: Set<PendingOutlives>,
        location: Location,
        assumptions: impl ToWcs,
        goal: impl ToWcs + Debug,
    ) -> ProvenSet<Set<PendingOutlives>> {
        let goal: Wcs = goal.to_wcs();
        self.prove_judgment(outlives, location, assumptions, goal.to_wcs(), formality_prove::prove)
    }

    /// Prove the goal with the function `judgment_fn`,
    /// accumulating pending outlive constraints onto the input set.
    ///
    /// One of the difference between this prove_judgment and the one in impl Check is
    /// that this version can accept existential variable, which is needed for handling lifetime.
    /// In the compiler, we insert existential variables for all
    /// lifetimes that appear in the MIR body, and I expect we will do the same here.
    fn prove_judgment<G>(
        &self,
        outlives: Set<PendingOutlives>,
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
            return ProvenSet::singleton((outlives, proof_tree.clone()));
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

        // Accumulate the new constraints onto the input set
        let (new_outlives, proof_tree) = pending_outlives_minimal;
        let combined: Set<PendingOutlives> = outlives.union(&new_outlives).cloned().collect();
        ProvenSet::singleton((combined, proof_tree))
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

/// Extract the ADT id and parameters from a type.
/// For now, only handles rigid types directly; normalization can be added later.
fn ty_is_adt(ty: &Ty) -> Option<(AdtId, Vec<Parameter>)> {
    match ty.data() {
        TyData::RigidTy(RigidTy {
            name: RigidName::AdtId(adt_id),
            parameters,
        }) => Some((adt_id.clone(), parameters.clone())),
        _ => None,
    }
}

mod test;
