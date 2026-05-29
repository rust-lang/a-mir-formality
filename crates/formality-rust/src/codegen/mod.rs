//! Judgment-based codegen: translates formality-rust programs to MiniRust.

use crate::check::borrow_check::liveness::LivePlaces;
use crate::check::borrow_check::nll::{
    borrow_check_expr, borrow_check_place_expr, prove_ty_is_rigid,
};
use crate::check::borrow_check::typed_place_expression::TypedPlaceExpr;
use crate::grammar::{
    expr::{Block, Expr, PlaceExpr, Stmt},
    Crates, Fallible, RigidName, RigidTy, Ty,
};
use formality_core::judgment_fn;
use libspecr::prelude::Map;
use minirust_rs::lang;

mod helpers;
mod minirust;
mod scope;
mod seme_region;
mod test;

use helpers::*;
use minirust::*;
use scope::*;
use seme_region::SemeRegion;

formality_core::cast_impl!(SemeRegion);

// ===========================================================================
// Top-level entry point
// ===========================================================================

pub fn codegen_program(crates: &Crates) -> Fallible<lang::Program> {
    let crates = crate::check::with_core_crate(crates);
    let mut g = CodegenGlobal::new(&crates);
    let main_key = MonoKey {
        id: crate::rust::term("main"),
        args: vec![],
    };
    let (main_fn_name, g2) = g.ensure_fn(main_key);
    g = g2;
    let mut functions: Map<lang::FnName, lang::Function> = Map::new();
    while let Some((key, fn_name)) = g.next_pending(&functions) {
        let (function, g2) = unwrap_proven(codegen_function(g, key))?;
        g = g2;
        functions.insert(fn_name, function.into());
    }
    let main_ret_ty = functions
        .get(main_fn_name)
        .map(|f| f.locals.get(f.ret).unwrap())
        .unwrap();
    let (start_fn_name, start_fn) = build_start_function(&mut g, main_fn_name, main_ret_ty);
    functions.insert(start_fn_name, start_fn);
    Ok(lang::Program {
        functions,
        start: start_fn_name,
        globals: Map::new(),
        traits: Map::new(),
        vtables: Map::new(),
    })
}

// ===========================================================================
// Primary judgments
// ===========================================================================

judgment_fn! {
    fn codegen_function(
        global: CodegenGlobal,
        key: MonoKey,
    ) => (MiniRustFunction, CodegenGlobal) {
        debug(global, key)

        (
            (let (fn_data, body) = resolve_fn_body(global, key)?)
            (let cfn = CodegenFn::new(&global.crates, &fn_data.output_ty))
            (let (ret_local, arg_locals, scope, cfn) = setup_fn_args(cfn, fn_data)?)
            (codegen_block(global, cfn, scope, body) => (region, global, cfn))
            (let function = build_function(cfn, region, ret_local, arg_locals))
            ---- ("function")
            (codegen_function(global, key) => (function, global))
        )
    }
}

judgment_fn! {
    fn codegen_block(
        global: CodegenGlobal,
        cfn: CodegenFn,
        scope: CodegenScope,
        block: Block,
    ) => (SemeRegion, CodegenGlobal, CodegenFn) {
        debug(global, cfn, scope, block)

        (
            (if let None = &block.label)!
            (let region = cfn.fresh_region())
            (for_all(i in 0..block.stmts.len()) with(region, scope, global, cfn)
                (codegen_stmt(global, cfn, scope, &block.stmts[i]) => (stmt_region, scope, global, cfn))
                (let (region, cfn) = cfn.append_from(region, stmt_region)))
            ---- ("block")
            (codegen_block(global, cfn, scope, block) => (region, global, cfn))
        )

        (
            (if let Some(label) = &block.label)!
            (let (exit_block, cfn) = cfn.fresh_bb())
            (let scope = scope.with_label(&label.id, (), exit_block))
            (let region = cfn.fresh_region())
            (for_all(i in 0..block.stmts.len()) with(region, scope, global, cfn)
                (codegen_stmt(global, cfn, scope, &block.stmts[i]) => (stmt_region, scope, global, cfn))
                (let (region, cfn) = cfn.append_from(region, stmt_region)))
            (let (region, cfn) = cfn.terminate(region, terminator_goto(exit_block), Some(exit_block)))
            ---- ("labeled-block")
            (codegen_block(global, cfn, scope, block) => (region, global, cfn))
        )
    }
}

judgment_fn! {
    fn codegen_stmt(
        global: CodegenGlobal,
        cfn: CodegenFn,
        scope: CodegenScope,
        stmt: Stmt,
    ) => (SemeRegion, CodegenScope, CodegenGlobal, CodegenFn) {
        debug(global, cfn, scope, stmt)

        (
            (let (local, cfn) = cfn.alloc_temp(ty)?)
            (let scope = scope.push_var(id, local, ty)?)
            (codegen_expr_into(global, cfn, scope, local, &init.expr) => (region, global, cfn))
            ---- ("let-init")
            (codegen_stmt(global, cfn, scope, Stmt::Let { label: _, id, ty, init: Some(init) }) => (region, scope, global, cfn))
        )

        (
            (let (local, cfn) = cfn.alloc_temp(ty)?)
            (let scope = scope.push_var(id, local, ty)?)
            (let region = cfn.fresh_region())
            ---- ("let-no-init")
            (codegen_stmt(global, cfn, scope, Stmt::Let { label: _, id, ty, init: None }) => (region, scope, global, cfn))
        )

        (
            (codegen_expr_into(global, cfn, scope, scope.ret_local, expr) => (region, global, cfn))
            (let (region, cfn) = cfn.terminated(region, terminator_return()))
            ---- ("return")
            (codegen_stmt(global, cfn, scope, Stmt::Return { expr }) => (region, scope, global, cfn))
        )

        (
            (type_expr(cfn, scope, expr) => expr_ty)
            (let (temp, cfn) = cfn.alloc_temp(&expr_ty)?)
            (codegen_expr_into(global, cfn, scope, temp, expr) => (region, global, cfn))
            (let (region, cfn) = cfn.print_intrinsic(region, temp)?)
            ---- ("print")
            (codegen_stmt(global, cfn, scope, Stmt::Print { expr }) => (region, scope, global, cfn))
        )

        (
            (let (ct, cfn) = cfn.alloc_local(bool_ty()))
            (codegen_expr_into(global, cfn, scope, ct, condition) => (cond_region, global, cfn))
            (codegen_block(global, cfn, scope, then_block) => (then_region, global, cfn))
            (codegen_block(global, cfn, scope, else_block) => (else_region, global, cfn))
            (let (region, cfn) = cfn.branch_on_bool_from(cond_region, ct, then_region, else_region))
            ---- ("if")
            (codegen_stmt(global, cfn, scope, Stmt::If { condition, then_block, else_block }) => (region, scope, global, cfn))
        )

        (
            (type_expr(cfn, scope, expr) => expr_ty)
            (let (temp, cfn) = cfn.alloc_temp(&expr_ty)?)
            (codegen_expr_into(global, cfn, scope, temp, expr) => (region, global, cfn))
            ---- ("expr")
            (codegen_stmt(global, cfn, scope, Stmt::Expr { expr }) => (region, scope, global, cfn))
        )

        (
            (let label = require_label(label)?)
            (let (loop_start, cfn) = cfn.fresh_bb())
            (let (exit_block, cfn) = cfn.fresh_bb())
            (let scope = scope.with_label(&label.id, Some(loop_start), exit_block))
            (codegen_block(global, cfn, scope, body) => (body_region, global, cfn))
            (let (region, cfn) = build_loop(cfn, loop_start, exit_block, body_region)?)
            ---- ("loop")
            (codegen_stmt(global, cfn, scope, Stmt::Loop { label, body }) => (region, scope, global, cfn))
        )

        (
            (let (_, exit) = scope.lookup_label(label)?)
            (let region = cfn.fresh_region())
            (let (region, cfn) = cfn.terminated(region, terminator_goto(exit)))
            ---- ("break")
            (codegen_stmt(global, cfn, scope, Stmt::Break { label }) => (region, scope, global, cfn))
        )

        (
            (let (continue_target, _) = scope.lookup_label(label)?)
            (if let Some(start) = continue_target)
            (let region = cfn.fresh_region())
            (let (region, cfn) = cfn.terminated(region, terminator_goto(start)))
            ---- ("continue")
            (codegen_stmt(global, cfn, scope, Stmt::Continue { label }) => (region, scope, global, cfn))
        )

        (
            (codegen_block(global, cfn, scope, block) => (region, global, cfn))
            ---- ("block")
            (codegen_stmt(global, cfn, scope, Stmt::Block(block)) => (region, scope, global, cfn))
        )

        (
            (let block = instantiate_erased(binder)?)
            (codegen_block(global, cfn, scope, block) => (region, global, cfn))
            ---- ("exists")
            (codegen_stmt(global, cfn, scope, Stmt::Exists { binder }) => (region, scope, global, cfn))
        )
    }
}

judgment_fn! {
    fn codegen_expr_into(
        global: CodegenGlobal,
        cfn: CodegenFn,
        scope: CodegenScope,
        target: MiniRustLocal,
        expr: Expr,
    ) => (SemeRegion, CodegenGlobal, CodegenFn) {
        debug(global, cfn, scope, target, expr)

        (
            (let region = cfn.fresh_region())
            ---- ("literal")
            (codegen_expr_into(global, cfn, scope, target, Expr::Literal { value, ty }) => (
                region.assign(target, constant(value, ty)),
                global,
                cfn,
            ))
        )

        (
            // A function name used as a value — FnDef is zero-sized.
            (if scope.lookup_var(id).is_err())
            (if cfn.crates.fn_named(id).is_ok())
            (let region = cfn.fresh_region())
            ---- ("fn-as-value")
            (codegen_expr_into(global, cfn, scope, target, Expr::Place(PlaceExpr::Var(id))) => (
                region.assign(target, unit_value()),
                global,
                cfn,
            ))
        )

        (
            (resolve_place(cfn, scope, place_expr) => typed)
            (let source = typed_place_to_minirust(cfn, scope, &typed)?)
            (let region = cfn.fresh_region())
            ---- ("place")
            (codegen_expr_into(global, cfn, scope, target, Expr::Place(place_expr)) => (
                region.assign(target, load(source)),
                global,
                cfn,
            ))
        )

        (
            (let region = cfn.fresh_region())
            ---- ("true")
            (codegen_expr_into(global, cfn, scope, target, Expr::True) => (
                region.assign(target, bool_constant(true)),
                global,
                cfn,
            ))
        )

        (
            (let region = cfn.fresh_region())
            ---- ("false")
            (codegen_expr_into(global, cfn, scope, target, Expr::False) => (
                region.assign(target, bool_constant(false)),
                global,
                cfn,
            ))
        )

        (
            (resolve_place(cfn, scope, place) => typed_dest)
            (let dest = typed_place_to_minirust(cfn, scope, &typed_dest)?)
            (type_expr(cfn, scope, rhs) => rhs_ty)
            (let (rhs_temp, cfn) = cfn.alloc_temp(rhs_ty)?)
            (codegen_expr_into(global, cfn, scope, rhs_temp, rhs) => (region, global, cfn))
            ---- ("assign")
            (codegen_expr_into(global, cfn, scope, target, Expr::Assign { place, expr: rhs }) => (
                region
                    .assign(dest, load(rhs_temp))
                    .assign(target, unit_value()),
                global,
                cfn,
            ))
        )

        (
            // Turbofish only impacts the type.
            (let (_, global) = global.ensure_fn(MonoKey::new(id, args.to_vec())))
            (let region = cfn.fresh_region())
            ---- ("turbofish")
            (codegen_expr_into(global, cfn, scope, target, Expr::Turbofish { id, args }) => (
                region.assign(target, unit_value()),
                global,
                cfn,
            ))
        )

        (
            (type_expr(cfn, scope, callee) => callee_ty)
            (resolve_rigid(cfn, scope, callee_ty) => RigidTy { name: RigidName::FnDef(fn_id), parameters })
            (let (fn_name, global) = global.ensure_fn(MonoKey::new(fn_id, parameters)))
            // Evaluate callee for side effects (value is zero-sized for FnDef).
            (let (callee_temp, cfn) = cfn.alloc_temp(&callee_ty)?)
            (codegen_expr_into(global, cfn, scope, callee_temp, callee) => (region, global, cfn))
            // Evaluate arguments.
            (let (temps, cfn) = alloc_temps_for_args(cfn, scope, args)?)
            (for_all(i in 0..args.len()) with(region, global, cfn)
                (codegen_expr_into(global, cfn, scope, &temps[i], &args[i]) => (arg_region, global, cfn))
                (let (region, cfn) = cfn.append_from(region, arg_region)))
            (let (region, cfn) = cfn.call(region, fn_name, temps, target)?)
            ---- ("call")
            (codegen_expr_into(global, cfn, scope, target, Expr::Call { callee, args }) => (region, global, cfn))
        )

        (
            (resolve_place(cfn, scope, place) => typed)
            (let mr_place = typed_place_to_minirust(cfn, scope, typed)?)
            (let ref_value = addr_of(cfn, mr_place, kind, &typed.ty)?)
            (let region = cfn.fresh_region())
            ---- ("ref")
            (codegen_expr_into(global, cfn, scope, target, Expr::Ref { kind, lt: _, place }) => (
                region.assign(target, ref_value),
                global,
                cfn,
            ))
        )

        (
            (let fields = resolve_struct_fields(cfn, adt_id, turbofish)?)
            (let (temps, cfn) = alloc_temps_for_fields(cfn, fields)?)
            (let region = cfn.fresh_region())
            (for_all(i in 0..fields.len()) with(region, global, cfn)
                (let field_expr = find_field_expr(field_exprs, &fields[i])?)
                (codegen_expr_into(global, cfn, scope, &temps[i], field_expr) => (field_region, global, cfn))
                (let (region, cfn) = cfn.append_from(region, field_region)))
            (let struct_value = tuple_value(&temps, adt_id, turbofish, &cfn.crates)?)
            ---- ("struct")
            (codegen_expr_into(global, cfn, scope, target, Expr::Struct { field_exprs, adt_id, turbofish }) => (
                region.assign(target, struct_value),
                global,
                cfn,
            ))
        )
    }
}

// ===========================================================================
// Type resolution judgments
// ===========================================================================

judgment_fn! {
    fn resolve_place(
        cfn: CodegenFn,
        scope: CodegenScope,
        place: PlaceExpr,
    ) => TypedPlaceExpr {
        debug(cfn, scope, place)

        (
            (borrow_check_place_expr(
                &cfn.typeck_env,
                &cfn.assumptions,
                &scope.flow_state,
                place,
            ) => (typed_place, returned_state))
            (let () = assert_no_constraints(returned_state))
            ---- ("resolve")
            (resolve_place(cfn, scope, place) => typed_place)
        )
    }
}

judgment_fn! {
    fn resolve_rigid(
        cfn: CodegenFn,
        scope: CodegenScope,
        ty: Ty,
    ) => RigidTy {
        debug(cfn, scope, ty)

        (
            (prove_ty_is_rigid(
                &cfn.typeck_env,
                &cfn.assumptions,
                &scope.flow_state,
                ty,
            ) => (rigid_ty, returned_state))
            (let () = assert_no_constraints(returned_state))
            ---- ("resolve")
            (resolve_rigid(cfn, scope, ty) => rigid_ty)
        )
    }
}

judgment_fn! {
    fn type_expr(
        cfn: CodegenFn,
        scope: CodegenScope,
        expr: Expr,
    ) => Ty {
        debug(cfn, scope, expr)

        (
            (borrow_check_expr(
                &cfn.typeck_env,
                &cfn.assumptions,
                &scope.flow_state,
                expr,
                LivePlaces::default(),
            ) => (ty, state))
            (let () = assert_no_constraints(state))
            ---- ("type_expr")
            (type_expr(cfn, scope, expr) => ty)
        )
    }
}
