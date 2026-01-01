use formality_core::{
    judgment::ProofTree, judgment_fn, set, term, Cons, Fallible, Set,
    Upcast,
};
use formality_prove::{prove, AdtDeclBoundData, AdtDeclVariant};
use formality_rust::grammar::minirust::{
    ArgumentExpression, BasicBlock, BbId, FieldProjection, PlaceExpression, Statement, Terminator,
    ValueExpression,
};
use formality_types::grammar::PredicateTy;
use formality_types::grammar::{
    AliasTy, Lt, LtData, Parameter, RefKind, Relation, RigidName, RigidTy, Ty, TyData, Variable,
    VariantId, Wcs,
};

use crate::{
    borrow_check::liveness::{
        places_live_before_basic_blocks, places_live_before_terminator, Assignment, LiveBefore,
        LivePlaces,
    },
    mini_rust_check::{ty_is_adt, Location, PendingOutlives, TypeckEnv},
};

/// Represents a loan that resulted from executing a borrow expression like `&'0 place`.
#[term]
struct Loan {
    /// The region `'0` of the resulting reference from this borrow.
    lt: Lt,

    /// The place being borrowed.
    place: PlaceExpression,

    /// The kind of borrow (shared, mutable, etc).
    kind: RefKind,
}

// Given this, the goal of the borrow checker (in some sense) is to find a minimal `LifetimeValue`
// for each existential variable such that all the outlives constraints are satisfied.
// If it cannot do so, that program does not type check.
//
// Once it has done so, it also checks whether any of the statements "violate the terms of a loan".
// Each loan (`&x` expression) has some associated `LifetimeValue`. It is considered live from the
// point in the CFG where the loan occurs to the end of the `LifetimeValue`. If there is a path from
// the loan, to a statement that violates the loan, and the entire path is within the `LifetimeValue`,
// that is an error.
//
// Or to say it another way:
//
// * For every path (L_0...L_n) that leads from a loan L to a statement (S that violates the terms of L),
//   there is some node L_i on the path that is not a member of the loan's lifetime.
//
// # Example
//
// ## Example A
//
// In this example:
//
// ```rust
// fn foo<'a>(x: &'a mut (u32,)) {
//     // Loc L0
//     let p: &'0 u32 = &'0 x.0; // Loan L has lifetime Locations(L0, L1)
//
//     // Loc L1
//     print(p);
//
//     // Loc L2
//     x.0 += 1; //
// }
// ```
//
// * The input constraints are
//     * From subtyping, `'a: '0`
//     * From liveness, `'0: Locations(L0)` and `'0: Locations(L1)`
// * There is a loan with lifetime `'0` that occurs at L0.
// * The minimal value for `'0` that satifies the constraints is Locations(L0, L1).
// * There is a statement at location L2 that violates the terms of the loan by mutating `(*x).0`.
// * There is a path (L0, L1, L2) from the loan to the statement. But it's ok, because L2 is a
//   member of that path but not of the loan's lifetime.
//
// ## Example B
//
// In contrast, in this example:
//
// ```rust
// fn foo<'a>(x: &'a mut (u32,)) {
//     // Loc L0
//     let p: &'0 u32 = &x.0; // Loan L has lifetime Locations(L0, L1)
//
//     // Loc L1
//     print(p);
//
//     // Loc L2
//     x.0 += 1;
//
//     // Loc L3
//     print(p);
// }
// ```
//
// The minimal lifetime of the loan is Locations(L0, L1, L2, L3).
// There is a path (L0, L1, L2) from the loan to the statement.
// This fails type check because the entire path is a member of that lifetime.
//
// Our strategy:
//
// * For each statement that creates a loan L:
//   * We will "prove" the loan is not violated, which requires:
//     * Enumerating the paths that start at L and stopping when we reach either
//       (a) node in which the loan is not live or (b) a node with no successors or
//       (c) a cycle.

/// A place that is being accessed and the way in which it is being accessed.
#[term]
struct Access {
    /// The kind of access
    kind: AccessKind,

    /// The place being accessed
    place: PlaceExpression,
}

#[term]
enum AccessKind {
    /// Reading the value in the place
    Read,

    /// Writing a value to the place
    Write,

    /// Moving a value out of a place
    Move,
}

/// The borrow checker's job is to pick up where the type-checker left off:
/// Given the `TypeckEnv`, and a (populated) list of `pending_outlives`
/// constraints, it attempts to find values for the existential lifetime variables (inference variables)
/// that satisfy those pending-outlives constraints and which meet the borrow checker's rules.
pub fn borrow_check(
    env: &TypeckEnv,
    fn_assumptions: &Wcs,
    pending_outlives: &Set<PendingOutlives>,
) -> Fallible<ProofTree> {
    let mut proof_tree = ProofTree::new(format!("borrow_check"), None, vec![]);

    // Verify that all pending outlives between universal lifetime variables
    // can be proven from the fn_assumptions.
    proof_tree
        .children
        .push(verify_universal_outlives(env, fn_assumptions, pending_outlives).check_proven()?);

    // Start the check from the entry block.
    //
    // The judgment requires that loans in all blocks
    // reachable from the start are also respected.
    let Some(start_bb) = env.blocks.first() else {
        return Ok(proof_tree);
    };

    let stack: Vec<StackEntry> = vec![];
    let loans_live: Set<Loan> = set![];
    proof_tree.children.push(
        borrow_check_block(
            stack,
            env,
            fn_assumptions,
            loans_live,
            pending_outlives.clone(),
            &start_bb.id,
        )
        .check_proven()?,
    );
    Ok(proof_tree)
}

judgment_fn! {
    /// Verify that all pending outlives constraints between universal lifetime variables
    /// can be proven from the function's where-clause assumptions.
    fn verify_universal_outlives(
        env: TypeckEnv,
        fn_assumptions: Wcs,
        outlives: Set<PendingOutlives>,
    ) => () {
        debug(env, fn_assumptions, outlives)

        (
            (for_all(v in env.env.variables())
                (only_assumed_outlives(&env, &fn_assumptions, &outlives, *v) => ()))
            --- ("verify_universal_outlives")
            (verify_universal_outlives(env, fn_assumptions, outlives) => ())
        )
    }
}

judgment_fn! {
    /// For existential lifetimes, trivially succeeds.
    /// For universal lifetimes, checks all transitively outlived variables can be proven from assumptions.
    fn only_assumed_outlives(
        env: TypeckEnv,
        fn_assumptions: Wcs,
        outlives: Set<PendingOutlives>,
        v: Variable,
    ) => () {
        debug(env, fn_assumptions, outlives, v)

        // Non-universal variables (existentials) - trivially succeed
        (
            (if v.is_existential())!
            --- ("existential")
            (only_assumed_outlives(_env, _fn_assumptions, _outlives, v) => ())
        )

        // Universal lifetime variables - check all transitively outlived
        (
            (if v.is_universal())!
            (for_all(param in transitively_outlived_by(&env, &outlives, &v))
                (can_outlive(&env, &fn_assumptions, &outlives, &v, param) => ()))
            --- ("universal lifetime")
            (only_assumed_outlives(env, fn_assumptions, outlives, v) => ())
        )
    }
}

judgment_fn! {
    /// Check if lt_a can outlive param_b.
    /// For non-universal targets, trivially succeeds.
    /// For universal lifetime targets, must prove from assumptions.
    fn can_outlive(
        env: TypeckEnv,
        fn_assumptions: Wcs,
        outlives: Set<PendingOutlives>,
        param_a: Parameter,
        param_b: Parameter,
    ) => () {
        debug(param_a, param_b, fn_assumptions, env, outlives)

        trivial(param_a == param_b => ())

        // Prove that T: 'a -- implied because we're working over transitive outlives
        (
            (if var_b.is_existential())!
            --- ("existential target")
            (can_outlive(_env, _fn_assumptions, _outlives, _param_a, var_b: Variable) => ())
        )

        // Prove that T: !a -- must prove from assumptions
        (
            (if var_b.is_universal())!
            (prove(env.decls, env.env, fn_assumptions, Relation::outlives(param_a, var_b)) => c)
            (if c.unconditionally_true())
            --- ("universal target")
            (can_outlive(env, fn_assumptions, _outlives, param_a, var_b: Variable) => ())
        )
    }
}

/// Tracks the state when entering a basic block during borrow checking.
/// Used to detect cycles in the control flow graph: if we visit the same
/// block with the same set of live loans, we've reached a fixpoint and
/// can stop recursing. This enables handling of loops without infinite recursion.
///
/// Note: Strictly speaking, we should also include `env` and `fn_assumptions`
/// in this struct, but we omit them for now since they never change during
/// the traversal of a single function body.
#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Clone, Hash)]
struct StackEntry {
    outlives: Set<PendingOutlives>,
    loans_live_on_entry: Set<Loan>,
    bb_id: BbId,
}

formality_core::cast_impl!(StackEntry);

impl StackEntry {
    fn new(
        _env: impl Upcast<TypeckEnv>,
        _fn_assumptions: impl Upcast<Wcs>,
        loans_live_on_entry: impl Upcast<Set<Loan>>,
        outlives: impl Upcast<Set<PendingOutlives>>,
        bb_id: impl Upcast<BbId>,
    ) -> Self {
        Self {
            loans_live_on_entry: loans_live_on_entry.upcast(),
            bb_id: bb_id.upcast(),
            outlives: outlives.upcast(),
        }
    }
}

judgment_fn! {
    /// Prove that any loans issued in this basic block are respected.
    fn borrow_check_block(
        stack: Vec<StackEntry>,
        env: TypeckEnv,
        fn_assumptions: Wcs,
        loans_live_on_entry: Set<Loan>,
        outlives: Set<PendingOutlives>,
        bb_id: BbId,
    ) => () {
        debug(loans_live_on_entry, bb_id, fn_assumptions, env, outlives, stack)

        // Cycle detection: if we've already visited this block with the same live loans,
        // we've reached a fixpoint and can stop recursing. This handles loops in the CFG.
        //
        // FIXME(nikomatsakis): This is potentially more precise than a real implementation would be.
        // We check each path through the CFG independently, so two paths arriving at the
        // same block with different loan sets X and Y are checked separately. A real
        // implementation might merge to (X ∪ Y) and check once, which could reject
        // programs this accepts. An example of such a case can be found in the test 
        // `cfg_union_approx_cause_false_error`.
        (
            (let this_entry = StackEntry::new(&env, &fn_assumptions, &loans_live_on_entry, &outlives, &bb_id))
            (if stack.contains(&this_entry))!
            --- ("cycle")
            (borrow_check_block(stack, env, fn_assumptions, loans_live_on_entry, outlives, bb_id) => ())
        )

        (
            (let this_entry = StackEntry::new(&env, &fn_assumptions, &loans_live_on_entry, &outlives, &bb_id))
            (if !stack.contains(&this_entry))!
            (let stack = { let mut s = stack.clone(); s.push(this_entry); s })
            (let BasicBlock { id: _, statements, terminator } = env.basic_block(&bb_id)?)
            (let places_live_before_terminator = places_live_before_terminator(&env, &terminator))
            (for_all(i in 0..statements.len()) with(outlives, loans_live)
                (borrow_check_statement(&env,
                    &fn_assumptions,
                    loans_live,
                    outlives,
                    &statements[i],
                    &statements[i+1..].live_before(&env, &places_live_before_terminator),
                ) => (outlives, loans_live)))
            (borrow_check_terminator(&stack, &env, &fn_assumptions, loans_live, &outlives, &terminator) => ())
            --- ("basic block")
            (borrow_check_block(stack, env, fn_assumptions, loans_live, outlives, bb_id) => ())
        )
    }
}

judgment_fn! {
    /// Prove that any loans issued in this statement are respected.
    fn borrow_check_terminator(
        stack: Vec<StackEntry>,
        env: TypeckEnv,
        fn_assumptions: Wcs,
        loans_live_on_entry: Set<Loan>,
        outlives: Set<PendingOutlives>,
        terminator: Terminator,
    ) => () {
        debug(loans_live_on_entry, terminator, fn_assumptions, env, outlives, stack)

        (
            (for_all(bb in &bb_ids)
                (borrow_check_block(&stack, &env, &assumptions, &loans_live, &outlives, bb) => ()))
            --- ("goto")
            (borrow_check_terminator(stack, env, assumptions, loans_live, outlives, Terminator::Goto(bb_ids)) => ())
        )

        (
            (let successors: Vec<BbId> = switch_targets
                .iter()
                .map(|t| &t.target)
                .chain(Some(&fallback))
                .cloned()
                .collect())
            (let places_live = places_live_before_basic_blocks(&env, &successors))
            (borrow_check_value_expression(&env, &assumptions, loans_live, outlives, switch_value, places_live) => (_switch_ty, outlives, loans_live))
            (for_all(successor in &successors)
                (borrow_check_block(&stack, &env, &assumptions, &loans_live, &outlives, successor) => ()))
            --- ("switch")
            (borrow_check_terminator(stack, env, assumptions, loans_live, outlives, Terminator::Switch { switch_value, switch_targets, fallback }) => ())
        )

        (
            --- ("return")
            (borrow_check_terminator(_stack, _env, _assumptions, _loans_live, _outlives, Terminator::Return) => ())
        )

        (
            (let places_live = places_live_before_basic_blocks(&env, &next_block))

            (borrow_check_value_expression(
                &env,
                &assumptions,
                loans_live,
                outlives,
                callee,
                (&arguments, Assignment(&ret)).live_before(&env, &places_live),
            ) => (_callee_ty, outlives, loans_live))

            (borrow_check_argument_expressions(
                &env,
                &assumptions,
                loans_live,
                outlives,
                &arguments,
                Assignment(&ret).live_before(&env, &places_live),
            ) => (outlives, loans_live))

            // here `ret` would be assigned

            (for_all(next_block in next_block.iter())
                (borrow_check_block(&stack, &env, &assumptions, &loans_live, &outlives, next_block) => ()))
            --- ("call")
            (borrow_check_terminator(stack, env, assumptions, loans_live, outlives, Terminator::Call { callee, generic_arguments: _, arguments, ret, next_block }) => ())
        )
    }
}

judgment_fn! {
    /// Prove that any loans issued in this statement are respected.
    fn borrow_check_statement(
        env: TypeckEnv,
        fn_assumptions: Wcs,
        loans_live_on_entry: Set<Loan>,
        outlives: Set<PendingOutlives>,
        statement: Statement,
        places_live_on_exit: LivePlaces,
    ) => (Set<PendingOutlives>, Set<Loan>) {
        debug(loans_live_on_entry, statement, places_live_on_exit, fn_assumptions, env, outlives)

        (
            // does not issue any loans
            --- ("storage-live")
            (borrow_check_statement(_env, _assumptions, loans_live, outlives, Statement::StorageLive(_), _places_live) => (outlives, loans_live))
        )

        (
            // does not issue any loans
            --- ("storage-dead")
            (borrow_check_statement(_env, _assumptions, loans_live, outlives, Statement::StorageDead(_), _places_live) => (outlives, loans_live))
        )

        (
            (access_permitted_by_loans(env, assumptions, &loans_live, &outlives, Access { kind: AccessKind::Read, place: place_accessed }, places_live) => ())
            --- ("place-mention")
            (borrow_check_statement(_env, assumptions, loans_live, outlives, Statement::PlaceMention(place_accessed), places_live) => (outlives.clone(), loans_live.clone()))
        )

        (
            // Borrow-check and type-check the RHS value expression
            (borrow_check_value_expression(
                &env,
                &assumptions,
                loans_live,
                outlives,
                &value_rhs,
                Assignment(&place_lhs).live_before(&env, &places_live),
            ) => (value_ty, outlives, loans_live))

            // Borrow-check and type-check the LHS place expression
            (borrow_check_place_expression(
                &env,
                &assumptions,
                loans_live,
                outlives,
                place_lhs.clone(),
                &places_live,
            ) => (place_ty, outlives, loans_live))

            // Prove subtyping: value_ty <: place_ty
            (env.prove_goal(outlives, Location, &assumptions, Relation::sub(value_ty.clone(), place_ty)) => outlives)

            // Check write access is permitted
            (access_permitted_by_loans(
                &env,
                &assumptions,
                &loans_live,
                &outlives,
                Access::new(AccessKind::Write, &place_lhs),
                &places_live,
            ) => ())
            --- ("assign")
            (borrow_check_statement(env, assumptions, loans_live, outlives, Statement::Assign(place_lhs, value_rhs), places_live) => (outlives.clone(), loans_live.clone()))
        )
    }
}

judgment_fn! {
    /// Prove that any loans issued in thes value expressions (evaluated in this order) are respected.
    fn borrow_check_argument_expressions(
        env: TypeckEnv,
        fn_assumptions: Wcs,
        loans_live_on_entry: Set<Loan>,
        outlives: Set<PendingOutlives>,
        values: Vec<ArgumentExpression>,
        places_live_on_exit: LivePlaces,
    ) => (Set<PendingOutlives>, Set<Loan>) {
        debug(loans_live_on_entry, values, places_live_on_exit, fn_assumptions, env, outlives)

        (
            (for_all(i in 0..values.len()) with(outlives, loans_live)
                (borrow_check_argument_expression(&env, &assumptions, loans_live, outlives, &values[i],
                    &values[i+1..].live_before(&env, &places_live)) => (outlives, loans_live)))
            --- ("borrow_check_argument_expressions")
            (borrow_check_argument_expressions(env, assumptions, loans_live, outlives, values, places_live) => (outlives, loans_live))
        )
    }
}

judgment_fn! {
    /// Prove that any loans issued in thes value expressions (evaluated in this order) are respected.
    fn borrow_check_argument_expression(
        env: TypeckEnv,
        fn_assumptions: Wcs,
        loans_live_on_entry: Set<Loan>,
        outlives: Set<PendingOutlives>,
        value: ArgumentExpression,
        places_live_on_exit: LivePlaces,
    ) => (Set<PendingOutlives>, Set<Loan>) {
        debug(loans_live_on_entry, value, places_live_on_exit, fn_assumptions, env, outlives)

        (
            (access_permitted_by_loans(env, assumptions, &loans_live, &outlives, Access::new(AccessKind::Move, expr), places_live) => ())
            --- ("in-place")
            (borrow_check_argument_expression(env, assumptions, loans_live, outlives, ArgumentExpression::InPlace(expr), places_live) => (outlives.clone(), loans_live.clone()))
        )

        (
            (borrow_check_value_expression(env, assumptions, loans_live, outlives, expr, places_live) => (_expr_ty, outlives, loans_live))
            --- ("by-value")
            (borrow_check_argument_expression(env, assumptions, loans_live, outlives, ArgumentExpression::ByValue(expr), places_live) => (outlives, loans_live))
        )
    }
}

judgment_fn! {
    /// Prove that any loans issued in thes value expressions (evaluated in this order) are respected.
    fn borrow_check_value_expressions(
        env: TypeckEnv,
        fn_assumptions: Wcs,
        loans_live_on_entry: Set<Loan>,
        outlives: Set<PendingOutlives>,
        values: Vec<ValueExpression>,
        places_live_on_exit: LivePlaces,
    ) => (Set<PendingOutlives>, Set<Loan>) {
        debug(loans_live_on_entry, values, places_live_on_exit, fn_assumptions, env, outlives)

        (
            (for_all(i in 0..values.len()) with(outlives, loans_live)
                (borrow_check_value_expression(&env, &assumptions, loans_live, outlives, &values[i],
                    &values[i+1..].live_before(&env, &places_live)) => (_value_ty, outlives, loans_live)))
            --- ("borrow_check_value_expressions")
            (borrow_check_value_expressions(env, assumptions, loans_live, outlives, values, places_live) => (outlives, loans_live))
        )
    }
}

judgment_fn! {
    /// Prove that any loans issued in this value expression are respected, and return its type.
    fn borrow_check_value_expression(
        env: TypeckEnv,
        fn_assumptions: Wcs,
        loans_live_on_entry: Set<Loan>,
        outlives: Set<PendingOutlives>,
        value: ValueExpression,
        places_live_on_exit: LivePlaces,
    ) => (Ty, Set<PendingOutlives>, Set<Loan>) {
        debug(loans_live_on_entry, value, places_live_on_exit, fn_assumptions, env, outlives)

        (
            (let value_ty = constant.get_ty())
            --- ("constant")
            (borrow_check_value_expression(_env, _assumptions, loans_live, outlives, ValueExpression::Constant(constant), _places_live) => (value_ty, outlives, loans_live))
        )

        (
            (if let Some(fn_decl) = env.fn_decl(&fn_id))
            (let value_ty = Ty::rigid(RigidName::FnDef(fn_decl.id.clone()), Vec::<Parameter>::new()))
            --- ("fn")
            (borrow_check_value_expression(env, _assumptions, loans_live, outlives, ValueExpression::Fn(fn_id), _places_live) => (value_ty, outlives, loans_live))
        )

        (
            (access_permitted_by_loans(&env, &assumptions, &loans_live, &outlives, Access::new(AccessKind::Read, &place), &places_live) => ())
            (borrow_check_place_expression(&env, &assumptions, loans_live.clone(), outlives.clone(), place.clone(), places_live.clone()) => (place_ty, outlives, loans_live))
            --- ("load")
            (borrow_check_value_expression(env, assumptions, loans_live, outlives, ValueExpression::Load(place), places_live) => (place_ty, outlives, loans_live))
        )

        (
            (env.prove_goal(outlives, Location, &assumptions, ty.well_formed()) => outlives)
            (if let Some((adt_id, parameters)) = ty_is_adt(&ty))
            (let AdtDeclBoundData { where_clause: _, variants } = env.decls.adt_decl(&adt_id).binder.instantiate_with(&parameters)?)
            (let AdtDeclVariant { name, fields } = variants.last().unwrap())
            (if *name == VariantId::for_struct())
            (if field_values.len() == fields.len())
            (let loans_live_for_loop = loans_live.clone())
            (for_all(pair in field_values.iter().zip(fields)) with(outlives, loans_live_for_loop)
                (let (value_expression, field) = pair)
                (borrow_check_struct_field(&env, &assumptions, loans_live_for_loop, outlives, value_expression, &field.ty, &places_live) => (outlives, loans_live_for_loop)))
            --- ("struct")
            (borrow_check_value_expression(env, assumptions, loans_live, outlives, ValueExpression::Struct(field_values, ty), places_live) => (ty.clone(), outlives, loans_live_for_loop))
        )

        (
            // In order to create a `&`-borrow, we need to be able to read the place.
            (access_permitted_by_loans(&env, &assumptions, &loans_live, &outlives, Access::new(AccessKind::Read, &place), &places_live) => ())
            (borrow_check_place_expression(&env, &assumptions, loans_live.clone(), outlives.clone(), place.clone(), places_live.clone()) => (place_ty, outlives, loans_live))
            (let value_ty = place_ty.ref_ty_of_kind(ref_kind, &lt))
            --- ("ref")
            (borrow_check_value_expression(env, assumptions, loans_live, outlives, ValueExpression::Ref(ref_kind @ RefKind::Shared, lt, place), places_live) => (value_ty, outlives.clone(), Cons(Loan::new(&lt, &place, RefKind::Shared), &loans_live)))
        )

        (
            // In order to create a `&mut`-borrow, we need to be able to write the place.
            (access_permitted_by_loans(&env, &assumptions, &loans_live, &outlives, Access::new(AccessKind::Write, &place), &places_live) => ())
            (borrow_check_place_expression(&env, &assumptions, loans_live.clone(), outlives.clone(), place.clone(), places_live.clone()) => (place_ty, outlives, loans_live))
            (let value_ty = place_ty.ref_ty_of_kind(ref_kind, &lt))
            --- ("ref-mut")
            (borrow_check_value_expression(env, assumptions, loans_live, outlives, ValueExpression::Ref(ref_kind @ RefKind::Mut, lt, place), places_live) => (value_ty, outlives.clone(), Cons(Loan::new(&lt, &place, RefKind::Mut), &loans_live)))
        )
    }
}

judgment_fn! {
    /// Borrow-check a place expression, returning its type.
    fn borrow_check_place_expression(
        env: TypeckEnv,
        fn_assumptions: Wcs,
        loans_live: Set<Loan>,
        outlives: Set<PendingOutlives>,
        place: PlaceExpression,
        places_live_on_exit: LivePlaces,
    ) => (Ty, Set<PendingOutlives>, Set<Loan>) {
        debug(loans_live, place, places_live_on_exit, fn_assumptions, env, outlives)

        (
            (if let Some((_, ty)) = env.find_local_id(&local_id))
            --- ("local")
            (borrow_check_place_expression(env, _assumptions, loans_live, outlives, PlaceExpression::Local(local_id), _places_live) => (ty, outlives, loans_live))
        )

        (
            (borrow_check_place_expression(&env, &assumptions, loans_live, outlives, &*field_projection.root, places_live) => (root_ty, outlives, loans_live))
            (if let Some((adt_id, parameters)) = ty_is_adt(&root_ty))
            (let AdtDeclBoundData { where_clause: _, variants } = env.decls.adt_decl(&adt_id).binder.instantiate_with(&parameters)?)
            (let AdtDeclVariant { name, fields } = variants.last().unwrap())
            (if *name == VariantId::for_struct())
            (if field_projection.index < fields.len())
            (let place_ty = fields[field_projection.index].ty.clone())
            --- ("field")
            (borrow_check_place_expression(env, assumptions, loans_live, outlives, PlaceExpression::Field(field_projection), places_live) => (place_ty, outlives, loans_live))
        )

        (
            (borrow_check_place_expression(&env, &assumptions, loans_live, outlives, &*place_expr, places_live) => (inner_ty, outlives, loans_live))
            (if let TyData::RigidTy(rigid_ty) = inner_ty.data())
            (if let RigidName::Ref(_ref_kind) = &rigid_ty.name)
            (let place_ty = rigid_ty.parameters[1].as_ty().expect("well-kinded reference").clone())
            --- ("deref-ref")
            (borrow_check_place_expression(env, assumptions, loans_live, outlives, PlaceExpression::Deref(place_expr), places_live) => (place_ty, outlives, loans_live))
        )
    }
}

judgment_fn! {
    /// Borrow-check a struct field value expression and prove it subtypes the expected field type.
    fn borrow_check_struct_field(
        env: TypeckEnv,
        fn_assumptions: Wcs,
        loans_live: Set<Loan>,
        outlives: Set<PendingOutlives>,
        value_expression: ValueExpression,
        expected_field_ty: Ty,
        places_live: LivePlaces,
    ) => (Set<PendingOutlives>, Set<Loan>) {
        debug(loans_live, value_expression, expected_field_ty, places_live, fn_assumptions, env, outlives)

        (
            (borrow_check_value_expression(&env, &assumptions, loans_live, outlives, value_expression, places_live) => (value_ty, outlives, loans_live))
            (env.prove_goal(outlives, Location, &assumptions, Relation::sub(value_ty, expected_field_ty.clone())) => outlives)
            --- ("borrow_check_struct_field")
            (borrow_check_struct_field(env, assumptions, loans_live, outlives, value_expression, expected_field_ty, places_live) => (outlives.clone(), loans_live.clone()))
        )
    }
}

judgment_fn! {
    /// Prove that none of the borrows in `borrowed` does not affect `place`.
    fn access_permitted_by_loans(
        env: TypeckEnv,
        assumptions: Wcs,
        loans_live_before_access: Set<Loan>,
        outlives: Set<PendingOutlives>,
        access: Access,
        places_live_after_access: LivePlaces,
    ) => () {
        debug(loans_live_before_access, access, places_live_after_access, assumptions, env, outlives)

        (
            (for_all(loan in &loans_live_before_access)
                (access_permitted_by_loan(&env, &assumptions, loan, &outlives, &access, &places_live_after_access) => ()))
            --- ("access_permitted_by_loans")
            (access_permitted_by_loans(env, assumptions, loans_live_before_access, outlives, access, places_live_after_access) => ())
        )
    }
}

judgment_fn! {
    /// Prove that the borrow `borrow` does not affect `place`.
    fn access_permitted_by_loan(
        env: TypeckEnv,
        assumptions: Wcs,
        loan: Loan,
        outlives: Set<PendingOutlives>,
        access: Access,
        places_live_after_access: LivePlaces,
    ) => () {
        debug(loan, access, places_live_after_access, assumptions, env, outlives)

        (
            (if place_disjoint_from_place(&loan.place, &access.place))
            --- ("borrow of disjoint places")
            (access_permitted_by_loan(_env, _assumptions, loan, _outlives, access, _places_live_after_access) => ())
        )

        (
            // e.g. something like `let x = &y; read(y); ...` is ok even if `x` is live
            --- ("read-shared is ok")
            (access_permitted_by_loan(
                _env,
                _assumptions,
                Loan { kind: RefKind::Shared, .. },
                _outlives,
                Access { kind: AccessKind::Read, .. },
                _live_places,
            ) => ())
        )

        (
            (if !place_disjoint_from_place(&loan.place, &access.place))! // just for convenience
            (loan_not_required_by_live_places(env, assumptions, &loan, &outlives, places_live_after_access) => ())
            (loan_cannot_outlive_universal_regions(env, assumptions, &outlives, &loan) => ())
            --- ("borrows of disjoint places")
            (access_permitted_by_loan(_env, _assumptions, loan, _outlives, access, _places_live_after_access) => ())
        )
    }
}

// EXAMPLE
//
// trait Foo { type Bar<'a>; }
//
// fn generic1<T: Foo>() {
//     let r = &'a ...;
//     let p: <T as Foo>::Bar<'a> = ...;
//     // if `p` is live, then `'a` must be live
// }
//
// fn generic2<T: Foo>() where for<'x> T: Foo<Bar<'x> = u32> {
//     let r = &'a ...;
//     let p: <T as Foo>::Bar<'a> = ...;
//     // if `p` is live, then `'a` doesn't have to be live
// }
//
// fn generic3<T: Foo, U>() where for<'x> T: Foo<Bar<'x> = U> {
//     let r = &'a ...;
//     let p: <T as Foo>::Bar<'a> = ...;
//     // if `p` is live, then `'a` doesn't have to be live
// }

// Design note:
//
// We decided to formulate this in "positive" form -- i.e., prove the loan is not required vs searching for liveness -- because of the interaction
// with alias types that we foresee.
//
// One observation w.r.t. alias types: one question that comes up is what we consider live - the unnormalized or normalized alias.
// We realized that since we're in borrow checking, we should *always* be able to normalize an alias, but we likely still want to formulate these rules for both.
// However, this provides some insight into what regions are live in the unnormalized form, particularly relevant to outlives bounds on opaque types.
// Particularly, the live regions in the *unnormalized* form should be a superset of all live regions found in the *normalized* form.

judgment_fn! {
    /// Prove that the loan does not outlive any universal regions.
    fn loan_cannot_outlive_universal_regions(
        env: TypeckEnv,
        assumptions: Wcs,
        outlives: Set<PendingOutlives>,
        loan: Loan,
    ) => () {
        debug(loan, assumptions, env, outlives)

        // Observation: we don't look at the `assumptions`
        //
        // ```
        // fn foo<'a>()
        // where
        //     for<'b> Vec<&'b ()>: 'a, // <-- we would not consider this
        // {
        // }
        // ```
        //
        // if we DID consider this, it would imply that all
        // borrows are always live.
        //
        // This is an interesting corner cases.

        (
            (let outlived_by_loan = transitively_outlived_by(&env, &outlives, &loan.lt))
            (if outlived_by_loan.iter().all(|p| match p {
                // If `'0: T` then `'0` must hold for entire fn body...
                Parameter::Ty(_) => false,

                Parameter::Lt(lt) => match lt.data() {
                    // If `'0: 'static` then `'0` must hold for entire fn body...
                    LtData::Static => false,

                    // If `'0: 'a` for some lifetime parameter `'a`, then `'0` must hold for entire fn body...
                    LtData::Variable(Variable::UniversalVar(_)) => false,

                    // If `'0: '1`, that's fine.
                    LtData::Variable(Variable::ExistentialVar(_)) => true,

                    LtData::Variable(Variable::BoundVar(_)) => panic!("cannot outlive a bound var"),
                },

                // Not really clear what this would mean
                Parameter::Const(_) => panic!("cannot outlive a constant"),
            }))
            --- ("loan_not_required_by_universal_regions")
            (loan_cannot_outlive_universal_regions(env, _assumptions, outlives, loan) => ())
        )
    }
}

judgment_fn! {
    /// Prove that the `loan` is not required to be live by
    /// an access to any of the places in `places_live_after_access`.
    ///
    /// For each `place_live \in places_live_after_acecess`...
    ///
    ///    let x = &'lt_loan place_borrowed;
    ///    ...
    ///    access(place_accessed); // conflicts with the borrow
    ///    ...
    ///    access(place_live)
    ///
    /// ...show that `place_live` does not require data derived from `x`.
    fn loan_not_required_by_live_places(
        env: TypeckEnv,
        assumptions: Wcs,
        loan: Loan,
        outlives: Set<PendingOutlives>,
        places_live_after_access: LivePlaces,
    ) => () {
        debug(loan, places_live_after_access, assumptions, env, outlives)

        (
            (for_all(live_place in &places_live_after_access)
                (loan_not_required_by_live_place(&env, &assumptions, &loan, &outlives, live_place) => ()))
            --- ("loan_not_required_by_live_places")
            (loan_not_required_by_live_places(env, assumptions, loan, outlives, places_live_after_access) => ())
        )
    }
}

judgment_fn! {
    /// For some `place_live` where
    ///
    ///    let x = &'lt_loan place_borrowed;
    ///    ...
    ///    access(place_accessed); // conflicts with the borrow
    ///    ...
    ///    access(place_live) // <-- an access like this occurs later
    ///
    /// ...show that `place_live` does not require data derived from `x`.
    fn loan_not_required_by_live_place(
        env: TypeckEnv,
        assumptions: Wcs,
        loan: Loan,
        outlives: Set<PendingOutlives>,
        live_place: PlaceExpression,
    ) => () {
        debug(loan, live_place, assumptions, env, outlives)

        (
            (let live_place_ty = env.check_place_hackola(&assumptions, &live_place, &outlives)?)
            (loan_not_required_by_parameter(&env, &assumptions, &loan, &outlives, live_place_ty) => ())
            (loan_not_required_by_live_place_prefix(&env, &assumptions, &loan, &outlives, &live_place) => ())
            --- ("loan is not required by type")
            (loan_not_required_by_live_place(
                env,
                assumptions,
                loan,
                outlives,
                live_place,
            ) => ())
        )
    }
}

judgment_fn! {
    /// For some `place_live` where
    ///
    ///    let x = &'lt_loan place_borrowed;
    ///    ...
    ///    access(place_accessed); // conflicts with the borrow
    ///    ...
    ///    access(place_live) // <-- an access like this occurs later
    ///
    /// ...show that `place_live` does not require data derived from `x`.
    fn loan_not_required_by_live_place_prefix(
        env: TypeckEnv,
        assumptions: Wcs,
        loan: Loan,
        outlives: Set<PendingOutlives>,
        live_place: PlaceExpression,
    ) => () {
        debug(loan, live_place, assumptions, env, outlives)

        (
            --- ("local")
            (loan_not_required_by_live_place_prefix(_env, _assumptions, _loan, _outlives, PlaceExpression::Local(_)) => ())
        )

        (
            (loan_not_required_by_live_place_prefix(env, assumptions, loan, &outlives, &*root) => ())
            --- ("no prefix")
            (loan_not_required_by_live_place_prefix(env, assumptions, loan, outlives, PlaceExpression::Field(FieldProjection { root, index: _ })) => ())
        )

        (
            (loan_not_required_by_live_place(env, assumptions, loan, &outlives, &*ptr) => ())
            --- ("field")
            (loan_not_required_by_live_place_prefix(env, assumptions, loan, outlives, PlaceExpression::Deref(ptr)) => ())
        )
    }
}

judgment_fn! {
    /// For some `place_live` which has type `place_live_ty` where
    ///
    ///    let x = &'lt_loan place_borrowed;
    ///    ...
    ///    access(place_accessed); // conflicts with the borrow
    ///    ...
    ///    access(place_live) // <-- an access like this occurs later
    ///
    /// ...show that `place_live_ty` does not require data derived from `x`.
    fn loan_not_required_by_parameter(
        env: TypeckEnv,
        assumptions: Wcs,
        loan: Loan,
        outlives: Set<PendingOutlives>,
        live_parameter: Parameter,
    ) => () {
        debug(loan, live_parameter, assumptions, env, outlives)

        (
            (loan_not_required_by_parameters(env, assumptions, loan, &outlives, parameters) => ())
            --- ("rigid-ty")
            (loan_not_required_by_parameter(env, assumptions, loan, outlives, RigidTy { name: _, parameters }) => ())
        )

        (
            // Per <https://rust-lang.github.io/rfcs/1214-projections-lifetimes-and-wf.html>,
            // if we can prove that the loan is not required by any of the aliases's parameters,
            // then we know it is not required by the alias, even if we can't normalize the alias.
            //
            // FIXME(incomplete, #224): We should adjust to account for the bounds we know to hold on the alias
            // which might allow us to prove the loan is not required in other ways.
            //
            // In the compiler we also use bivariance in some bizarre hacky way here, but I *think* that's
            // just a hack because we always have ALL lifetime parameters, even though we don't really need
            // them all.
            (loan_not_required_by_parameters(env, assumptions, loan, &outlives, parameters) => ())
            --- ("alias-ty RFC 1214")
            (loan_not_required_by_parameter(env, assumptions, loan, outlives, AliasTy { name: _, parameters }) => ())
        )

        // (
        //     // (if we can normalize to T)
        //     // (and we can prove it of T)
        //     // (we are happy)
        //     --- ("alias-ty normalization")
        //     (loan_not_required_by_parameter(env, assumptions, loan, AliasTy { name: _, parameters }) => ())
        // )

        (
            (let (parameter, env1) = env.instantiate_universally(&binder))
            (loan_not_required_by_parameter(env1, assumptions, loan, &outlives, parameter) => ())
            --- ("for-all-type")
            (loan_not_required_by_parameter(env, assumptions, loan, outlives, PredicateTy::ForAll(binder)) => ())
        )

        (
            // All the loans in the universal type were decided by the caller
            // and hence cannot possibly include THIS loan.
            //
            // This is a subtle rule, and this condition is enforced
            // elsewhere, but it is needed to make this clause correct on its own.
            // Consider the following scenario:
            //
            // ```
            // fn foo<'a, T>(q: T) {
            //     let mut p = 22;
            //     let r = &'a p;     // <-- note that this is explicitly annotated with 'a
            //     p += 1;
            //     use(q);
            // }
            // ```
            //
            // Now, `q` is live, and hence `T` is live. The question is
            // whether `T` may reference the borrow that resulted from
            // `&'a p`. In this example, it does not, but consider the next one:
            //
            // ```
            // fn foo<'a, T>(q: T)
            // where T: Trait<'a>,
            // {
            //     let mut p = 22;
            //     let r = &'a p;
            //     T::observe(&q, r);    // <-- note that this is explicitly annotated with 'a
            //     p += 1;
            //     use(q);
            // }
            // ```
            //
            // Here, `T::observe(&q, r)` might have "squirreled away" the reference `r`
            // and thus the loan may in fact be accessed when we `use(q)`.
            //
            // By enforcing that the loan does not escape to any universal region,
            // we ensure this is not a problem.
            (loan_cannot_outlive_universal_regions(env, assumptions, &outlives, &loan) => ())
            --- ("universal-variable")
            (loan_not_required_by_parameter(env, assumptions, loan, outlives, Variable::UniversalVar(_v)) => ())
        )

        (
            //
            // ```
            // let mut x = 22;
            // let p = &'0 x;
            // let q: &'1 x = p; // requires `'0: '1`
            // x += 1;
            // use(q); // could this use data derived from `p`? (obviously yes)
            // ```
            //
            // In this case, we would be invoked here with `'1`.
            (loan_cannot_outlive(env, assumptions, loan, &outlives, live_lt) => ())
            --- ("lifetime")
            (loan_not_required_by_parameter(env, assumptions, loan, outlives, live_lt: Lt) => ())
        )
    }
}

judgment_fn! {
    /// Prove that the loan does not outlive any universal regions.
    fn loan_cannot_outlive(
        env: TypeckEnv,
        assumptions: Wcs,
        loan: Loan,
        outlives: Set<PendingOutlives>,
        lifetime: Lt,
    ) => () {
        debug(loan, lifetime, assumptions, env, outlives)

        (
            (let outlived_by_loan = transitively_outlived_by(&env, &outlives, &loan.lt))
            (if !outlived_by_loan.contains(&lifetime.upcast()))
            --- ("loan_cannot_outlive")
            (loan_cannot_outlive(env, _assumptions, loan, outlives, lifetime) => ())
        )
    }
}

judgment_fn! {
    /// For some `place_live` which has type `place_live_ty` where
    ///
    ///    let x = &'lt_loan place_borrowed;
    ///    ...
    ///    access(place_accessed); // conflicts with the borrow
    ///    ...
    ///    access(place_live) // <-- an access like this occurs later
    ///
    /// ...show that `place_live_ty` does not require data derived from `x`.
    fn loan_not_required_by_parameters(
        env: TypeckEnv,
        assumptions: Wcs,
        loan: Loan,
        outlives: Set<PendingOutlives>,
        live_parameters: Vec<Parameter>,
    ) => () {
        debug(loan, live_parameters, assumptions, env, outlives)

        (
            (for_all(param in &live_parameters)
                (loan_not_required_by_parameter(&env, &assumptions, &loan, &outlives, param) => ()))
            --- ("loan_not_required_by_parameters")
            (loan_not_required_by_parameters(env, assumptions, loan, outlives, live_parameters) => ())
        )
    }
}

/// Given a region `r`, find a set of all regions `r1` where `r: r1` transitively
/// according to the `pending_outlives` in `env`.
fn transitively_outlived_by(
    _env: &TypeckEnv,
    pending_outlives: &Set<PendingOutlives>,
    start_lt: impl Upcast<Parameter>,
) -> Set<Parameter> {
    let start_lt = start_lt.upcast();
    let mut reachable = Set::new();

    reachable.insert(start_lt.clone());
    let mut worklist = vec![start_lt.clone()];

    while let Some(current) = worklist.pop() {
        for PendingOutlives { location: _, a, b } in pending_outlives.iter() {
            if *a == current {
                if reachable.insert(b.clone()) {
                    worklist.push(b.clone());
                }
            }
        }
    }

    reachable
}

fn place_disjoint_from_place(place_a: &PlaceExpression, place_b: &PlaceExpression) -> bool {
    let prefixes_a = place_prefixes(place_a);
    let prefixes_b = place_prefixes(place_b);
    !prefixes_a.contains(place_b) && !prefixes_b.contains(place_a)
}

/// Returns a set containing `place` and all prefixes of `place`.
fn place_prefixes(place: &PlaceExpression) -> Set<PlaceExpression> {
    let mut prefixes = Set::new();
    let mut current = place;
    loop {
        prefixes.insert(current.clone());
        match current {
            PlaceExpression::Local(_) => break,
            PlaceExpression::Deref(place_expression) => current = place_expression,
            PlaceExpression::Field(FieldProjection { root, .. }) => current = root,
        }
    }
    prefixes
}

#[test]
fn test_locals_are_disjoint() {
    use formality_types::rust::term;
    let place_a: PlaceExpression = term("local(a)");
    let place_b: PlaceExpression = term("local(b)");
    assert!(place_disjoint_from_place(&place_a, &place_b));
}

#[test]
fn test_local_plus_field() {
    use formality_types::rust::term;
    let place_a: PlaceExpression = term("local(a)");
    let place_b: PlaceExpression = term("local(a).0");
    assert!(!place_disjoint_from_place(&place_a, &place_b));
}

#[test]
fn test_two_different_fields() {
    use formality_types::rust::term;
    let place_a: PlaceExpression = term("local(a).0");
    let place_b: PlaceExpression = term("local(a).1");
    assert!(place_disjoint_from_place(&place_a, &place_b));
}
