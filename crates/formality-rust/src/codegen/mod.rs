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
mod code_block;
mod test;

use helpers::*;
use minirust::*;
use scope::*;
use code_block::CodeBlock;

formality_core::cast_impl!(CodeBlock);

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
        let function;
        (function, g) = unwrap_proven(codegen_function(g, key))?;
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
            (codegen_block(global, cfn, scope, body) => (code, global, cfn))
            (let function = build_function(cfn, code, ret_local, arg_locals))
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
    ) => (CodeBlock, CodegenGlobal, CodegenFn) {
        debug(global, cfn, scope, block)

        (
            (if let None = &block.label)!
            (let code = cfn.fresh_code_block())
            (for_all(i in 0..block.stmts.len()) with(code, scope, global, cfn)
                (codegen_stmt(global, cfn, scope, &block.stmts[i]) => (stmt_code, scope, global, cfn))
                (let (code, cfn) = cfn.append_from(code, stmt_code)))
            ---- ("block")
            (codegen_block(global, cfn, scope, block) => (code, global, cfn))
        )

        (
            (if let Some(label) = &block.label)!
            (let (exit_block, cfn) = cfn.fresh_bb())
            (let scope = scope.with_label(&label.id, (), exit_block))
            (let code = cfn.fresh_code_block())
            (for_all(i in 0..block.stmts.len()) with(code, scope, global, cfn)
                (codegen_stmt(global, cfn, scope, &block.stmts[i]) => (stmt_code, scope, global, cfn))
                (let (code, cfn) = cfn.append_from(code, stmt_code)))
            (let (code, cfn) = cfn.terminate(code, terminator_goto(exit_block), Some(exit_block)))
            ---- ("labeled-block")
            (codegen_block(global, cfn, scope, block) => (code, global, cfn))
        )
    }
}

judgment_fn! {
    fn codegen_stmt(
        global: CodegenGlobal,
        cfn: CodegenFn,
        scope: CodegenScope,
        stmt: Stmt,
    ) => (CodeBlock, CodegenScope, CodegenGlobal, CodegenFn) {
        debug(global, cfn, scope, stmt)

        (
            (let (local, cfn) = cfn.alloc_temp(ty)?)
            (let scope = scope.push_var(id, local, ty)?)
            (codegen_expr_into(global, cfn, scope, local, &init.expr) => (code, global, cfn))
            ---- ("let-init")
            (codegen_stmt(global, cfn, scope, Stmt::Let { label: _, id, ty, init: Some(init) }) => (code, scope, global, cfn))
        )

        (
            (let (local, cfn) = cfn.alloc_temp(ty)?)
            (let scope = scope.push_var(id, local, ty)?)
            (let code = cfn.fresh_code_block())
            ---- ("let-no-init")
            (codegen_stmt(global, cfn, scope, Stmt::Let { label: _, id, ty, init: None }) => (code, scope, global, cfn))
        )

        (
            (codegen_expr_into(global, cfn, scope, scope.ret_local, expr) => (code, global, cfn))
            (let (code, cfn) = cfn.terminated(code, terminator_return()))
            ---- ("return")
            (codegen_stmt(global, cfn, scope, Stmt::Return { expr }) => (code, scope, global, cfn))
        )

        (
            (type_expr(cfn, scope, expr) => expr_ty)
            (let (temp, cfn) = cfn.alloc_temp(&expr_ty)?)
            (codegen_expr_into(global, cfn, scope, temp, expr) => (code, global, cfn))
            (let (code, cfn) = cfn.print_intrinsic(code, temp)?)
            ---- ("print")
            (codegen_stmt(global, cfn, scope, Stmt::Print { expr }) => (code, scope, global, cfn))
        )

        (
            (let (ct, cfn) = cfn.alloc_local(bool_ty()))
            (codegen_expr_into(global, cfn, scope, ct, condition) => (cond_code, global, cfn))
            (codegen_block(global, cfn, scope, then_block) => (then_code, global, cfn))
            (codegen_block(global, cfn, scope, else_block) => (else_code, global, cfn))
            (let (code, cfn) = cfn.branch_on_bool_from(cond_code, ct, then_code, else_code))
            ---- ("if")
            (codegen_stmt(global, cfn, scope, Stmt::If { condition, then_block, else_block }) => (code, scope, global, cfn))
        )

        (
            (type_expr(cfn, scope, expr) => expr_ty)
            (let (temp, cfn) = cfn.alloc_temp(&expr_ty)?)
            (codegen_expr_into(global, cfn, scope, temp, expr) => (code, global, cfn))
            ---- ("expr")
            (codegen_stmt(global, cfn, scope, Stmt::Expr { expr }) => (code, scope, global, cfn))
        )

        (
            (let label = require_label(label)?)
            (let (loop_start, cfn) = cfn.fresh_bb())
            (let (exit_block, cfn) = cfn.fresh_bb())
            (let scope = scope.with_label(&label.id, Some(loop_start), exit_block))
            (codegen_block(global, cfn, scope, body) => (body_code, global, cfn))
            (let (code, cfn) = build_loop(cfn, loop_start, exit_block, body_code)?)
            ---- ("loop")
            (codegen_stmt(global, cfn, scope, Stmt::Loop { label, body }) => (code, scope, global, cfn))
        )

        (
            (let (_, exit) = scope.lookup_label(label)?)
            (let code = cfn.fresh_code_block())
            (let (code, cfn) = cfn.terminated(code, terminator_goto(exit)))
            ---- ("break")
            (codegen_stmt(global, cfn, scope, Stmt::Break { label }) => (code, scope, global, cfn))
        )

        (
            (let (continue_target, _) = scope.lookup_label(label)?)
            (if let Some(start) = continue_target)
            (let code = cfn.fresh_code_block())
            (let (code, cfn) = cfn.terminated(code, terminator_goto(start)))
            ---- ("continue")
            (codegen_stmt(global, cfn, scope, Stmt::Continue { label }) => (code, scope, global, cfn))
        )

        (
            (codegen_block(global, cfn, scope, block) => (code, global, cfn))
            ---- ("block")
            (codegen_stmt(global, cfn, scope, Stmt::Block(block)) => (code, scope, global, cfn))
        )

        (
            (let block = instantiate_erased(binder)?)
            (codegen_block(global, cfn, scope, block) => (code, global, cfn))
            ---- ("exists")
            (codegen_stmt(global, cfn, scope, Stmt::Exists { binder }) => (code, scope, global, cfn))
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
    ) => (CodeBlock, CodegenGlobal, CodegenFn) {
        debug(global, cfn, scope, target, expr)

        (
            (let code = cfn.fresh_code_block())
            ---- ("literal")
            (codegen_expr_into(global, cfn, scope, target, Expr::Literal { value, ty }) => (
                code.assign(target, constant(value, ty)),
                global,
                cfn,
            ))
        )

        (
            // A function name used as a value — FnDef is zero-sized.
            (if scope.lookup_var(id).is_err())
            (if cfn.crates.fn_named(id).is_ok())
            (let code = cfn.fresh_code_block())
            ---- ("fn-as-value")
            (codegen_expr_into(global, cfn, scope, target, Expr::Place(PlaceExpr::Var(id))) => (
                code.assign(target, unit_value()),
                global,
                cfn,
            ))
        )

        (
            (resolve_place(cfn, scope, place_expr) => typed)
            (let source = typed_place_to_minirust(cfn, scope, &typed)?)
            (let code = cfn.fresh_code_block())
            ---- ("place")
            (codegen_expr_into(global, cfn, scope, target, Expr::Place(place_expr)) => (
                code.assign(target, load(source)),
                global,
                cfn,
            ))
        )

        (
            (let code = cfn.fresh_code_block())
            ---- ("true")
            (codegen_expr_into(global, cfn, scope, target, Expr::True) => (
                code.assign(target, bool_constant(true)),
                global,
                cfn,
            ))
        )

        (
            (let code = cfn.fresh_code_block())
            ---- ("false")
            (codegen_expr_into(global, cfn, scope, target, Expr::False) => (
                code.assign(target, bool_constant(false)),
                global,
                cfn,
            ))
        )

        (
            (resolve_place(cfn, scope, place) => typed_dest)
            (let dest = typed_place_to_minirust(cfn, scope, &typed_dest)?)
            (type_expr(cfn, scope, rhs) => rhs_ty)
            (let (rhs_temp, cfn) = cfn.alloc_temp(rhs_ty)?)
            (codegen_expr_into(global, cfn, scope, rhs_temp, rhs) => (code, global, cfn))
            ---- ("assign")
            (codegen_expr_into(global, cfn, scope, target, Expr::Assign { place, expr: rhs }) => (
                code
                    .assign(dest, load(rhs_temp))
                    .assign(target, unit_value()),
                global,
                cfn,
            ))
        )

        (
            // Turbofish always generates a zero-sized value.
            // The important bits (function name, arguments, etc) show up in the *type*.
            (let code = cfn.fresh_code_block())
            ---- ("turbofish")
            (codegen_expr_into(global, cfn, scope, target, Expr::Turbofish { .. }) => (
                code.assign(target, unit_value()),
                global,
                cfn,
            ))
        )

        (
            // Get the type of the calleee. This rule expects static dispatch,
            // so we require that the callee has a zero-sized `FnDef` type that
            // uniquely identifies a callee.
            (type_expr(cfn, scope, callee) => callee_ty)
            (resolve_rigid(cfn, scope, callee_ty) => RigidTy { name: RigidName::FnDef(fn_id), parameters })
            (let (fn_name, global) = global.ensure_fn(MonoKey::new(fn_id, parameters)))
            // Evaluate callee for side effects (value is zero-sized for FnDef).
            (let (callee_temp, cfn) = cfn.alloc_temp(&callee_ty)?)
            (codegen_expr_into(global, cfn, scope, callee_temp, callee) => (code, global, cfn))
            // Evaluate arguments into temporaries.
            (let (temps, cfn) = alloc_temps_for_args(cfn, scope, args)?)
            (for_all(i in 0..args.len()) with(code, global, cfn)
                (codegen_expr_into(global, cfn, scope, &temps[i], &args[i]) => (arg_code, global, cfn))
                (let (code, cfn) = cfn.append_from(code, arg_code)))
            // Construct the minirust function call.
            (let (code, cfn) = cfn.call(code, fn_name, temps, target)?)
            ---- ("call")
            (codegen_expr_into(global, cfn, scope, target, Expr::Call { callee, args }) => (code, global, cfn))
        )

        (
            (resolve_place(cfn, scope, place) => typed)
            (let mr_place = typed_place_to_minirust(cfn, scope, typed)?)
            (let ref_value = addr_of(cfn, mr_place, kind, &typed.ty)?)
            (let code = cfn.fresh_code_block())
            ---- ("ref")
            (codegen_expr_into(global, cfn, scope, target, Expr::Ref { kind, lt: _, place }) => (
                code.assign(target, ref_value),
                global,
                cfn,
            ))
        )

        (
            (let fields = resolve_struct_fields(cfn, adt_id, turbofish)?)
            (let (temps, cfn) = alloc_temps_for_fields(cfn, fields)?)
            (let code = cfn.fresh_code_block())
            (for_all(i in 0..fields.len()) with(code, global, cfn)
                (let field_expr = find_field_expr(field_exprs, &fields[i])?)
                (codegen_expr_into(global, cfn, scope, &temps[i], field_expr) => (field_code, global, cfn))
                (let (code, cfn) = cfn.append_from(code, field_code)))
            (let struct_value = tuple_value(&temps, adt_id, turbofish, &cfn.crates)?)
            ---- ("struct")
            (codegen_expr_into(global, cfn, scope, target, Expr::Struct { field_exprs, adt_id, turbofish }) => (
                code.assign(target, struct_value),
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
