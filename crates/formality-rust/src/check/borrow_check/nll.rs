use crate::check::borrow_check::env::TypeckEnv;
use crate::check::borrow_check::flow_state::{FlowState, Loan, PendingOutlives};
use crate::check::borrow_check::outlives::transitively_outlived_by;
use crate::check::borrow_check::typed_place_expression::{
    TypedPlaceExpr, TypedPlaceExpressionData,
};

use crate::grammar::expr::{Block, Expr, ExprData, Init, PlaceExpr, PlaceExprData, Stmt};
use crate::grammar::{
    AliasTy, FieldName, Lt, LtData, Parameter, RefKind, Relation, RigidName, RigidTy, ScalarId,
    Struct, StructBoundData, Ty, TyData, ValueId, Variable, Wcs, WhereClause,
};
use crate::grammar::{FnBoundData, PredicateTy};
use formality_core::{judgment_fn, term, ProvenSet, Set, Union, Upcast};

use crate::check::borrow_check::liveness::{Assignment, Either, LiveBefore, LivePlaces};

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
    place: TypedPlaceExpr,
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

judgment_fn! {
    /// Prove that any loans issued in this basic block are respected.
    pub fn borrow_check(
        env: TypeckEnv,
        assumptions: Wcs,
        state: FlowState,
        block: Block,
    ) => () {
        debug(assumptions, env, state, block)

        (
            (borrow_check_block(env, assumptions, state, block, LivePlaces::default()) => _state)
            ------------------------------------------------------------ ("borrow_check")
            (borrow_check(env, assumptions, state, block) => ())
        )
    }
}

judgment_fn! {
    /// Prove that any loans issued in this basic block are respected.
    fn borrow_check_block(
        env: TypeckEnv,
        assumptions: Wcs,
        state: FlowState,
        block: Block,
        places_live_on_exit: LivePlaces,
    ) => FlowState {
        debug(assumptions, env, state, block)
        assert(state.check_invariants())

        (
            (let state = state.push_scope(&env.env, label, places_live_on_exit)?)
            (for_all(i in 0..stmts.len()) with(env, state)
                (borrow_check_statement(
                    env,
                    assumptions,
                    state,
                    &stmts[i],
                    stmts[i+1..].live_before(env, &state, places_live_on_exit),
                ) => (env, state)))
            // Drop locals declared in this scope (reverse declaration order)
            (let locals_to_drop = state.locals_dropped_in_innermost_scope())
            (drop_places(env, assumptions, state, locals_to_drop, places_live_on_exit) => state)
            (let state = state.pop_scope(label))
            ------------------------------------------------------------ ("basic block")
            (borrow_check_block(env, assumptions, state, Block { label, stmts }, places_live_on_exit) => state)
        )
    }
}

judgment_fn! {
    /// Prove that any loans issued in this statement are respected.
    fn borrow_check_statement(
        env: TypeckEnv,
        assumptions: Wcs,
        state: FlowState,
        statement: Stmt,
        places_live_on_exit: LivePlaces,
    ) => (TypeckEnv, FlowState) {
        debug(state, statement, places_live_on_exit, assumptions, env)

        (
            // The variable being declared is not yet live before this `let`,
            // so remove it from places_live_on_exit before checking the initializer (if any).
            (for_all(init in init.into_iter()) with (state) // FIXME: should make syntax for this
                (let Init { expr } = init)
                (borrow_check_expr_has_ty(env,
                    assumptions,
                    state,
                    expr,
                    ty,
                    LiveBefore::live_before(&Assignment(id), env, &state, &places_live_on_exit),
                ) => state))

            (let state = state.with_local_in_scope(&env.env, label, id, ty)?)
            ------------------------------------------------------------ ("let")
            (borrow_check_statement(env, assumptions, state, Stmt::Let { label, id, ty, init }, places_live_on_exit) => (env, state))
        )

        (
            // Check the condition has type bool
            (borrow_check_expr_has_ty(
                env,
                assumptions,
                state,
                condition,
                Ty::bool(),
                Either(then_block, else_block).live_before(env, &state, places_live_on_exit),
            ) => state)

            // Check both branches
            (borrow_check_block(env, assumptions, state, then_block, places_live_on_exit) => then_state)
            (borrow_check_block(env, assumptions, state, else_block, places_live_on_exit) => else_state)

            // Join the flow states from both branches
            (let state: FlowState = Union((then_state, else_state)).upcast())
            ------------------------------------------------------------ ("if")
            (borrow_check_statement(env, assumptions, state, Stmt::If { condition, then_block, else_block }, places_live_on_exit) => (env, state))
        )

        (
            (borrow_check_expr(
                env,
                assumptions,
                state,
                expr,
                places_live_on_exit
            ) => (_init_ty, state))
            ------------------------------------------------------------ ("expr")
            (borrow_check_statement(env, assumptions, state, Stmt::Expr { expr }, places_live_on_exit) => (env, state))
        )

        (
            // Loop statement: create scope with continue_live_places, iterate to fixed point.
            // Use Stmt::Loop::live_before which builds a LivenessContext that includes
            // the loop's own label, so that continue/break inside the body resolve correctly.
            (let continue_live = Stmt::loop_(label, body).live_before(&env, &state, places_live_on_exit))
            (let state = state.push_continue_scope(&env.env, label, places_live_on_exit, continue_live)?)
            (borrow_check_loop(env, assumptions, state, body, places_live_on_exit) => state)
            (let state = state.pop_scope(label))
            ------------------------------------------------------------ ("loop")
            (borrow_check_statement(env, assumptions, state, Stmt::Loop { label, body }, places_live_on_exit) => (env, state))
        )

        (
            // break targets any in-scope label (block or loop).
            // Drop locals from all scopes up to and including the target.
            (if state.scope_has_label(label))!
            (let locals_to_drop = state.locals_dropped_to_label(label))
            (drop_places(env, assumptions, state, locals_to_drop, places_live_on_exit) => state)
            (let state = state.with_break(label))
            ------------------------------------------------------------ ("break")
            (borrow_check_statement(env, assumptions, state, Stmt::Break { label }, places_live_on_exit) => (env, state))
        )

        (
            // continue targets only loop labels (scopes with continue_live_places).
            // Drop locals from all scopes up to and including the target.
            (if state.scope_has_label(label))!
            (if let Some(places_live_on_continue) = state.live_after_continue(label))
            (let locals_to_drop = state.locals_dropped_to_label(label))
            (drop_places(env, assumptions, state, locals_to_drop, places_live_on_continue) => state)
            (let state = state.with_continue(label))
            ------------------------------------------------------------ ("continue")
            (borrow_check_statement(env, assumptions, state, Stmt::Continue { label }, places_live_on_exit) => (env, state))
        )

        (
            // Return: check the return expression and verify it's assignable to the output type
            (borrow_check_expr(env, assumptions, state, expr, LivePlaces::default()) => (expr_ty, state))
            (if let Some(output_ty) = &env.output_ty)
            (prove_assignable(env, assumptions, state, expr_ty, output_ty) => state)
            ------------------------------------------------------------ ("return")
            (borrow_check_statement(env, assumptions, state, Stmt::Return { expr }, _places_live_on_exit) => (env, state))
        )

        (
            // Nested block statement
            (borrow_check_block(env, assumptions, state, block, places_live_on_exit) => state)
            ------------------------------------------------------------ ("block")
            (borrow_check_statement(env, assumptions, state, Stmt::Block(block), places_live_on_exit) => (env, state))
        )

        (
            // Existential variables: open the binder and check the inner block
            (let (env, subst, block) = env.instantiate_existentially(binder))
            (borrow_check_block(env, assumptions, state, block, places_live_on_exit) => state)
            (let state = state.pop_subst(&env.env, subst))
            ------------------------------------------------------------ ("exists")
            (borrow_check_statement(env, assumptions, state, Stmt::Exists { binder }, places_live_on_exit) => (env, state))
        )
    }
}

judgment_fn! {
    /// Prove that any loans issued in this value expression are respected, and return its type.
    fn borrow_check_expr_has_ty(
        env: TypeckEnv,
        assumptions: Wcs,
        state: FlowState,
        expr: Expr,
        ty: Ty,
        places_live_on_exit: LivePlaces,
    ) => FlowState {
        debug(state, expr, ty, places_live_on_exit, assumptions, env)

        (
            (borrow_check_expr(env, assumptions, state, expr, places_live_on_exit) => (ty_expr, state))
            (prove_assignable(env, assumptions, state, ty_expr, ty) => state)
            ------------------------------------------------------------ ("block")
            (borrow_check_expr_has_ty(env, assumptions, state, expr, ty, places_live_on_exit) => state)
        )
    }
}

judgment_fn! {
    /// Prove that any loans issued in this value expression are respected, and return its type.
    fn borrow_check_expr(
        env: TypeckEnv,
        assumptions: Wcs,
        state: FlowState,
        expr: Expr,
        places_live_on_exit: LivePlaces,
    ) => (Ty, FlowState) {
        debug(state, expr, places_live_on_exit, assumptions, env)

        (
            // Borrow-check and type-check the RHS value expression
            (borrow_check_expr(
                env,
                assumptions,
                state,
                expr,
                Assignment(place).live_before(env, &state, places_live_on_exit),
            ) => (value_ty, state))

            // Borrow-check and type-check the LHS place expression
            (borrow_check_place_expr(
                env,
                assumptions,
                state,
                place,
            ) => (place, state))

            // Prove subtyping: value_ty <: place_ty
            (prove_assignable(env, assumptions, state, value_ty, &place.ty) => state)

            // Check write access is permitted
            (access_permitted(
                env,
                assumptions,
                state,
                Access::new(AccessKind::Write, place),
                places_live_on_exit,
            ) => state)

            (let state = kill_loans(place, state))
            ------------------------------------------------------------ ("assign")
            (borrow_check_expr(env, assumptions, state, ExprData::Assign { place, expr }, places_live_on_exit) => (Ty::unit(), state))
        )

        (
            // Check the callee
            (borrow_check_expr(
                env,
                assumptions,
                state,
                callee,
                args.live_before(env, &state, places_live),
            ) => (callee_ty, state))

            // We only support calling FnDef right now
            (prove_ty_is_rigid(env, assumptions, state, callee_ty) => (RigidTy { name: RigidName::FnDef(fn_id), parameters }, state))

            // Find the function declaration and instantiate it
            (let fn_decl = env.program.fn_named(fn_id)?)
            (let FnBoundData { input_args, output_ty, where_clauses, body: _ } =
                fn_decl.binder.instantiate_with(parameters)?)

            // Check argument count matches
            (let input_tys: Vec<Ty> = input_args.iter().map(|a| a.ty.clone()).collect())
            (if input_tys.len() == args.len())

            (for_all(i in 0..args.len()) with(state)
                (borrow_check_expr_has_ty(env, assumptions, state, &args[i], &input_tys[i], places_live) => state))

            (prove_where_clauses(env, assumptions, state, where_clauses) => state)
            ------------------------------------------------------------ ("call")
            (borrow_check_expr(env, assumptions, state, ExprData::Call { callee, args }, places_live) => (output_ty, state))
        )

        (
            ------------------------------------------------------------ ("literal")
            (borrow_check_expr(_env, _assumptions, state, ExprData::Literal { value: _, ty }, _places_live_on_exit) => (ty, state))
        )

        (
            ------------------------------------------------------------ ("true")
            (borrow_check_expr(_env, _assumptions, state, ExprData::True, _places_live_on_exit) => (ScalarId::Bool, state))
        )

        (
            ------------------------------------------------------------ ("false")
            (borrow_check_expr(_env, _assumptions, state, ExprData::False, _places_live_on_exit) => (ScalarId::Bool, state))
        )

        (
            (borrow_check_place_expr(
                env,
                assumptions,
                state,
                place,
            ) => (place, state))

            // Check that the access required by the borrow is permitted
            (let access_kind = match kind {
                RefKind::Shared => AccessKind::Read,
                RefKind::Mut => AccessKind::Write,
            })
            (access_permitted(
                env,
                assumptions,
                state,
                Access::new(access_kind, place),
                places_live_on_exit,
            ) => state)

            // Introduce the loan
            (let state = state.with_loan(Loan::new(lt, place, kind)))
            (let ty = place.ty.ref_ty_of_kind(kind, lt))
            ------------------------------------------------------------ ("ref")
            (borrow_check_expr(env, assumptions, state, ExprData::Ref { kind, lt, place }, places_live_on_exit) => (ty, state))
        )

        (
            (borrow_check_place_expr(env, assumptions, state, place) => (place, state))
            (access_permitted(env, assumptions, state, Access::new(AccessKind::Read, place), places_live_on_exit) => state)
            // FIXME: need to check that either the type is copy or this place can be moved from
            ------------------------------------------------------------ ("place")
            (borrow_check_expr(env, assumptions, state, ExprData::Place(place), places_live_on_exit) => (&place.ty, state))
        )

        (
            // A bare function name used as a value (e.g., `foo` in `foo(args)`).
            // Only applies when `id` is NOT a local variable.
            (if let PlaceExprData::Var(id) = place.data())
            (if !state.has_local(id))!
            (let fn_decl = env.program.fn_named(id)?)
            (if fn_decl.binder.len() == 0)
            (let ty: Ty = RigidTy { name: RigidName::FnDef(ValueId::clone(id)), parameters: vec![] }.upcast())
            // FIXME: check where clauses from fn
            ------------------------------------------------------------ ("fn-name")
            (borrow_check_expr(env, _assumptions, state, ExprData::Place(place), _places_live_on_exit) => (ty, state))
        )

        (
            // A function name with explicit type arguments (e.g., `foo::<'a, T>(args)`).
            (let fn_decl = env.program.fn_named(id)?)
            (if fn_decl.binder.len() == args.len())
            (let ty: Ty = RigidTy { name: RigidName::FnDef(ValueId::clone(id)), parameters: args.to_vec() }.upcast())
            // FIXME: check where clauses from fn
            ------------------------------------------------------------ ("turbofish")
            (borrow_check_expr(env, _assumptions, state, ExprData::Turbofish { id, args }, _places_live_on_exit) => (ty, state))
        )

        // fn foo<'a, T>() where T: 'a { }
        // let x = for<'a> { foo::<'a, &'b u32>() }
        //    -- introduce placeholder 'a
        //    -- *try* to prove &'b u32: 'a
        //    -- *try* to prove 'b: 'a ===> succeed with a pending outlives
        //  -- for<'a> -- would hvae to check that there are no pending outlives (that can't be proven)
        //                fail

        // let x = for<'a> where('b: 'a) { foo::<'a, &'b u32>() }
        //    -- introduce placeholder 'a
        //    -- *try* to prove &'b u32: 'a
        //    -- *try* to prove 'b: 'a ===> succeed with a pending outlives
        //  -- for<'a> -- would hvae to check that there are no pending outlives (that can't be proven)
        //                fail

        (
            // Struct construction: `Foo { field1: expr1, field2: expr2 }`
            (let Struct { id: _, binder } = env.program.struct_named(&adt_id)?)
            (if turbofish.parameters.len() == binder.len())
            (let StructBoundData { where_clauses, fields } = binder.instantiate_with(&turbofish.parameters)?)

            // Check that the provided field names exactly match the struct definition
            (let expected_names: Set<&FieldName> = fields.iter().map(|f| &f.name).collect())
            (let provided_names: Set<&FieldName> = field_exprs.iter().map(|fe| &fe.name).collect())
            (if expected_names == provided_names)

            // Type-check each field expression
            (for_all(i in 0..field_exprs.len()) with(state)
                (field in fields)
                (if field.name == field_exprs[i].name)
                (borrow_check_expr_has_ty(env, assumptions, state, &field_exprs[i].value, &field.ty, places_live_on_exit) => state))

            (prove_where_clauses(env, assumptions, state, where_clauses) => state)

            (let ty = RigidTy::new(adt_id, &turbofish.parameters))
            ------------------------------------------------------------ ("struct")
            (borrow_check_expr(env, assumptions, state, ExprData::Struct { adt_id, turbofish, field_exprs }, places_live_on_exit) => (ty, state))
        )
    }
}

judgment_fn! {
    /// Borrow-check a place expression, returning its type.
    fn borrow_check_loop(
        env: TypeckEnv,
        assumptions: Wcs,
        state: FlowState,
        body: Block,
        places_live_on_exit: LivePlaces,
    ) => FlowState {
        debug(state, body, places_live_on_exit, assumptions, env)

        (
            (borrow_check_block(env, assumptions, state, body, places_live_on_exit) => state)
            // FIXME: add continues here
            (borrow_check_loop(env, assumptions, state, body, places_live_on_exit) => state)
            ------------------------------------------------------------ ("loop")
            (borrow_check_loop(env, assumptions, state, body, places_live_on_exit) => state)
        )

        (
            (borrow_check_block(env, assumptions, state0, body, places_live_on_exit) => state1)
            (if state0 == state1)
            ------------------------------------------------------------ ("fixed-point")
            (borrow_check_loop(env, assumptions, state0, body, places_live_on_exit) => state1)
        )
    }
}

judgment_fn! {
    /// Borrow-check a place expression, returning its type.
    fn borrow_check_place_expr(
        env: TypeckEnv,
        assumptions: Wcs,
        state: FlowState,
        place: PlaceExpr,
    ) => (TypedPlaceExpr, FlowState) {
        debug(place, state, assumptions, env)

        (
            (let ty = state.local_variable(&local_id)?)
            ------------------------------------------------------------ ("local")
            (borrow_check_place_expr(_env, _assumptions, state, PlaceExprData::Var(local_id)) => (
                TypedPlaceExpr::new(ty, TypedPlaceExpressionData::local(local_id)),
                state,
            ))
        )

        (
            (borrow_check_place_expr(env, assumptions, state, prefix) => (prefix_typed, state))
            (prove_ty_is_rigid(env, assumptions, state, &prefix_typed.ty) => (RigidTy { name: RigidName::AdtId(adt_id), parameters }, state))
            (let Struct { id: _, binder } = env.program.struct_named(&adt_id)?)
            (let StructBoundData { where_clauses, fields } = binder.instantiate_with(&parameters)?)
            (prove_where_clauses(env, assumptions, state, where_clauses) => state)
            (field in fields)
            (if field.name == *field_name)
            ------------------------------------------------------------ ("struct field")
            (borrow_check_place_expr(env, assumptions, state, PlaceExprData::Field { prefix, field_name }) => (
                TypedPlaceExpr::new(&field.ty, TypedPlaceExpressionData::field(prefix_typed, field_name)),
                state,
            ))
        )

        (
            (borrow_check_place_expr(env, assumptions, state, prefix) => (prefix_typed, state))
            (prove_ty_is_rigid(env, assumptions, state, &prefix_typed.ty) => (RigidTy { name: RigidName::Tuple(arity), parameters }, state))
            (if field_index < arity)
            (if let Parameter::Ty(field_ty) = &parameters[*field_index])
            ------------------------------------------------------------ ("tuple field")
            (borrow_check_place_expr(env, assumptions, state, PlaceExprData::Field { prefix, field_name: FieldName::Index(field_index) }) => (
                TypedPlaceExpr::new(field_ty, TypedPlaceExpressionData::field(prefix_typed, field_index)),
                state,
            ))
        )

        (
            (borrow_check_place_expr(env, assumptions, state, place) => (place_typed, state))
            ------------------------------------------------------------ ("parens")
            (borrow_check_place_expr(env, assumptions, state, PlaceExprData::Parens(place)) => (place_typed, state))
        )

        (
            (borrow_check_place_expr(env, assumptions, state, prefix) => (prefix_typed, state))
            // FIXME: to generalize beyond references, we probably want to add a `prove_rigid_ty_is_deref` judgment that results in the referent ty
            (prove_ty_is_rigid(env, assumptions, state, &prefix_typed.ty) => (RigidTy { name: RigidName::Ref(_ref_kind), parameters }, state))
            (if let Parameter::Ty(referent_ty) = &parameters[1])
            ------------------------------------------------------------ ("deref-ref")
            (borrow_check_place_expr(env, assumptions, state, PlaceExprData::Deref { prefix }) => (
                TypedPlaceExpr::new(referent_ty, TypedPlaceExpressionData::deref(prefix_typed)),
                state,
            ))
        )
    }
}

/// Prove that any loans issued in thes value expressions (evaluated in this order) are respected.
fn kill_loans(overwritten_place: &TypedPlaceExpr, state: &FlowState) -> FlowState {
    let mut current = state.current.clone();

    current
        .loans_live
        .retain(|loan| !overwritten_place.is_prefix_of(&loan.place));

    FlowState {
        scopes: state.scopes.clone(),
        current,
        breaks: state.breaks.clone(),
        continues: state.continues.clone(),
    }
}

judgment_fn! {
    /// Check that dropping the given locals (in the order provided) is permitted.
    /// Locals should be provided in reverse declaration order (LIFO drop order).
    fn drop_places(
        env: TypeckEnv,
        assumptions: Wcs,
        state: FlowState,
        places: Vec<TypedPlaceExpr>,
        places_live_after_drop: LivePlaces,
    ) => FlowState {
        debug(state, places, places_live_after_drop, assumptions, env)

        (
            (for_all(place in places) with(state)
                (access_permitted_by_loans(env, assumptions, state, Access::new(AccessKind::Write, place), places_live_after_drop) => state))
            ------------------------------------------------------------ ("drop_places")
            (drop_places(env, assumptions, state, places, places_live_after_drop) => state)
        )
    }
}

judgment_fn! {
    /// Check that the given access is permitted. Currently this just checks
    /// that no live loans conflict with the access; in the future, this will
    /// also check initialization, moves, etc.
    fn access_permitted(
        env: TypeckEnv,
        assumptions: Wcs,
        state: FlowState,
        access: Access,
        places_live_after_access: LivePlaces,
    ) => FlowState {
        debug(state, access, places_live_after_access, assumptions, env)

        (
            (access_permitted_by_loans(env, assumptions, state, access, places_live_after_access) => state)
            ------------------------------------------------------------ ("access_permitted")
            (access_permitted(env, assumptions, state, access, places_live_after_access) => state)
        )
    }
}

judgment_fn! {
    /// Prove that none of the borrows in `borrowed` does not affect `place`.
    fn access_permitted_by_loans(
        env: TypeckEnv,
        assumptions: Wcs,
        state: FlowState,
        access: Access,
        places_live_after_access: LivePlaces,
    ) => FlowState {
        debug(state, access, places_live_after_access, assumptions, env)

        (
            (for_all(loan in &state.current.loans_live) with(state)
                (access_permitted_by_loan(env, assumptions, state, loan, access, places_live_after_access) => state))
            ------------------------------------------------------------ ("access_permitted_by_loans")
            (access_permitted_by_loans(env, assumptions, state, access, places_live_after_access) => state)
        )
    }
}

judgment_fn! {
    /// Prove that the borrow `borrow` does not affect `place`.
    fn access_permitted_by_loan(
        env: TypeckEnv,
        assumptions: Wcs,
        state: FlowState,
        loan: Loan,
        access: Access,
        places_live_after_access: LivePlaces,
    ) => FlowState {
        debug(loan, state, access, places_live_after_access, assumptions, env)

        (
            // If the borrowed place and the accesed place are disjoint, then there is no problem.

            (if place_disjoint_from_place(&loan.place, &access.place))
            ------------------------------------------------------------ ("borrow of disjoint places")
            (access_permitted_by_loan(_env, _assumptions, state, loan, access, _places_live_after_access) => state)
        )

        (
            // Shared loans permit reads.
            ------------------------------------------------------------ ("read-shared is ok")
            (access_permitted_by_loan(
                _env,
                _assumptions,
                state,
                Loan { kind: RefKind::Shared, .. },
                Access { kind: AccessKind::Read, .. },
                _live_places,
            ) => state)
        )

        (
            // Allow a write to a place P (or some prefix thereof) from a loan of `*P` (or some suffix thereof)
            // if P is a borrowed reference.
            //
            // Rationale: If you have a loan of (e.g.) `*foo`, and `foo` is a borrowed reference,
            // then you can write to `foo` (or some prefix of `foo`) without violating
            // the borrowed data, because `foo` is just a pointer.

            (place_loaned in place_loaned.all_prefixes())
            (if let TypedPlaceExpressionData::Deref(place_loaned_ref) = place_loaned.data())
            (prove_ty_is_ref(env, assumptions, state, &place_loaned_ref.ty) => state)
            (if place_accessed.is_prefix_of(place_loaned_ref))
            ------------------------------------------------------------ ("write-indirect")
            (access_permitted_by_loan(
                env,
                assumptions,
                state,
                Loan { lt: _, place: place_loaned, kind: _ },
                Access { kind: AccessKind::Write, place: place_accessed },
                _live_places,
            ) => state)
        )

        (
            // Allow the access if the loan has expired (its originated lifetime is not requied to outlive
            // any live lifetime. Live lifetimes include lifetimes that appear in live places and universal lifetimes.

            (if !place_disjoint_from_place(&loan.place, &access.place))! // just for convenience
            (loan_not_required_by_live_places(env, assumptions, state, loan, places_live_after_access) => state)
            (loan_cannot_outlive_universal_regions(env, assumptions, &state.current.outlives, &loan) => ())
            ------------------------------------------------------------ ("loan is dead")
            (access_permitted_by_loan(env, assumptions, state, loan, access, places_live_after_access) => state)
        )
    }
}

judgment_fn! {
    /// Prove that any loans issued in thes value expressions (evaluated in this order) are respected.
    fn prove_ty_is_ref(
        env: TypeckEnv,
        assumptions: Wcs,
        state: FlowState,
        ty: Ty,
    ) => FlowState {
        debug(env, assumptions, state, ty)

        (
            (prove_ty_is_rigid(env, assumptions, state, ty) => (RigidTy { name: RigidName::Ref(..), .. }, state))
            ------------------------------------------------------------ ("ref")
            (prove_ty_is_ref(_env, _assumptions, state, ty) => state)
        )
    }
}

judgment_fn! {
    /// Prove that any loans issued in thes value expressions (evaluated in this order) are respected.
    fn prove_ty_is_rigid(
        env: TypeckEnv,
        assumptions: Wcs,
        state: FlowState,
        ty: Ty,
    ) => (RigidTy, FlowState) {
        debug(env, assumptions, state, ty)

        (
            ------------------------------------------------------------ ("rigid")
            (prove_ty_is_rigid(_env, _assumptions, state, TyData::RigidTy(rigid_ty)) => (rigid_ty, state))
        )

        (
            (prove_normalize_ty(env, assumptions, state, ty) => (normalized_ty, state))
            (prove_ty_is_rigid(env, assumptions, state, normalized_ty) => (rigid_ty, state))
            ------------------------------------------------------------ ("normalize")
            (prove_ty_is_rigid(env, assumptions, state, ty) => (rigid_ty, state))
        )
    }
}

judgment_fn! {
    /// Prove that `a` is assignable to `b`.
    fn prove_assignable(
        env: TypeckEnv,
        assumptions: Wcs,
        state: FlowState,
        a: Ty,
        b: Ty,
    ) => FlowState {
        debug(state, a, b, env, assumptions)

        (
            (prove_ty_is_rigid(env, assumptions, state, a) => (RigidTy { name: RigidName::Never, .. }, state))!
            ------------------------------------------------------------ ("never-to-b")
            (prove_assignable(env, assumptions, state, a, _b) => state)
        )

        (
            (prove_sub_type(env, assumptions, state, a, b) => state)
            ------------------------------------------------------------ ("subtype")
            (prove_assignable(env, assumptions, state, a, b) => state)
        )
    }
}

fn prove_sub_type(
    env: &TypeckEnv,
    assumptions: &Wcs,
    state: &FlowState,
    a: impl Upcast<Parameter>,
    b: impl Upcast<Parameter>,
) -> ProvenSet<FlowState> {
    TypeckEnv::prove_goal(env, assumptions, state, Relation::sub(a, b))
}

fn prove_where_clauses(
    env: &TypeckEnv,
    assumptions: &Wcs,
    state: &FlowState,
    where_clauses: &[WhereClause],
) -> ProvenSet<FlowState> {
    TypeckEnv::prove_goal(env, assumptions, state, where_clauses)
}

fn prove_normalize_ty(
    env: &TypeckEnv,
    assumptions: &Wcs,
    state: &FlowState,
    ty: &Ty,
) -> ProvenSet<(Ty, FlowState)> {
    TypeckEnv::prove_normalize(env, assumptions, state, ty)
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
            ------------------------------------------------------------ ("loan_not_required_by_universal_regions")
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
        state: FlowState,
        loan: Loan,
        places_live_after_access: LivePlaces,
    ) => FlowState {
        debug(loan, state, places_live_after_access, assumptions, env)

        (
            (for_all(live_place in places_live_after_access) with(state)
                (borrow_check_place_expr(env, assumptions, state, live_place) => (live_place_typed, state))
                (loan_not_required_by_live_place(env, assumptions, &state.current.outlives, loan, live_place_typed) => ()))
            ------------------------------------------------------------ ("loan_not_required_by_live_places")
            (loan_not_required_by_live_places(env, assumptions, state, loan, places_live_after_access) => state)
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
        outlives: Set<PendingOutlives>,
        loan: Loan,
        live_place: TypedPlaceExpr,
    ) => () {
        debug(loan, live_place, assumptions, env)

        (
            (loan_not_required_by_parameter(env, assumptions, outlives, loan, &live_place.ty) => ())
            (loan_not_required_by_live_place_prefix(env, assumptions, outlives, loan, live_place) => ())
            ------------------------------------------------------------ ("loan is not required by type")
            (loan_not_required_by_live_place(
                env,
                assumptions,
                outlives,
                loan,
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
        outlives: Set<PendingOutlives>,
        loan: Loan,
        live_place: TypedPlaceExpr,
    ) => () {
        debug(outlives, loan, live_place, assumptions, env)

        (
            (if let None = live_place.prefix())!
            ------------------------------------------------------------ ("no prefix")
            (loan_not_required_by_live_place_prefix(_env, _assumptions, _outlives, _loan, live_place) => ())
        )

        (
            (if let Some(prefix) = live_place.prefix())!
            (loan_not_required_by_live_place(env, assumptions, outlives, loan, prefix) => ())
            ------------------------------------------------------------ ("prefix")
            (loan_not_required_by_live_place_prefix(env, assumptions, outlives, loan, live_place) => ())
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
        outlives: Set<PendingOutlives>,
        loan: Loan,
        live_parameter: Parameter,
    ) => () {
        debug(loan, live_parameter, assumptions, env, outlives)

        (
            (loan_not_required_by_parameters(env, assumptions, outlives, loan, parameters) => ())
            ------------------------------------------------------------ ("rigid-ty")
            (loan_not_required_by_parameter(env, assumptions, outlives, loan, RigidTy { name: _, parameters }) => ())
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
            (loan_not_required_by_parameters(env, assumptions, outlives, loan, parameters) => ())
            ------------------------------------------------------------ ("alias-ty RFC 1214")
            (loan_not_required_by_parameter(env, assumptions, outlives, loan, AliasTy { name: _, parameters }) => ())
        )

        // (
        //     // (if we can normalize to T)
        //     // (and we can prove it of T)
        //     // (we are happy)
        //     ------------------------------------------------------------ ("alias-ty normalization")
        //     (loan_not_required_by_parameter(env, assumptions, loan, AliasTy { name: _, parameters }) => ())
        // )

        (
            (let (env1, _, parameter) = env.instantiate_universally(&binder))
            (loan_not_required_by_parameter(env1, assumptions, outlives, loan, parameter) => ())
            ------------------------------------------------------------ ("for-all-type")
            (loan_not_required_by_parameter(env, assumptions, outlives, loan, PredicateTy::ForAll(binder)) => ())
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
            (loan_cannot_outlive_universal_regions(env, assumptions, outlives, &loan) => ())
            ------------------------------------------------------------ ("universal-variable")
            (loan_not_required_by_parameter(env, assumptions, outlives, loan, Variable::UniversalVar(_v)) => ())
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
            (loan_cannot_outlive(env, assumptions, outlives, loan, live_lt) => ())
            ------------------------------------------------------------ ("lifetime")
            (loan_not_required_by_parameter(env, assumptions, outlives, loan, live_lt: Lt) => ())
        )
    }
}

judgment_fn! {
    /// Prove that the loan does not outlive any universal regions.
    fn loan_cannot_outlive(
        env: TypeckEnv,
        assumptions: Wcs,
        outlives: Set<PendingOutlives>,
        loan: Loan,
        lifetime: Lt,
    ) => () {
        debug(loan, lifetime, assumptions, env, outlives)

        (
            (let outlived_by_loan = transitively_outlived_by(env, outlives, &loan.lt))
            (if !outlived_by_loan.contains(&lifetime.upcast()))
            ------------------------------------------------------------ ("loan_cannot_outlive")
            (loan_cannot_outlive(env, _assumptions, outlives, loan, lifetime) => ())
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
        outlives: Set<PendingOutlives>,
        loan: Loan,
        live_parameters: Vec<Parameter>,
    ) => () {
        debug(loan, live_parameters, assumptions, env, outlives)

        (
            (for_all(param in live_parameters)
                (loan_not_required_by_parameter(env, assumptions, outlives, loan, param) => ()))
            ------------------------------------------------------------ ("loan_not_required_by_parameters")
            (loan_not_required_by_parameters(env, assumptions, outlives, loan, live_parameters) => ())
        )
    }
}

fn place_disjoint_from_place(place_a: &TypedPlaceExpr, place_b: &TypedPlaceExpr) -> bool {
    let prefixes_a = place_a.all_prefixes();
    let prefixes_b = place_b.all_prefixes();
    !prefixes_a.contains(&place_b) && !prefixes_b.contains(&place_a)
}
