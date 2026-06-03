//! Non-judgment helper functions for codegen.

use crate::check::borrow_check::flow_state::FlowState;
use crate::check::borrow_check::typed_place_expression::{
    TypedPlaceExpr, TypedPlaceExpressionData,
};
use crate::grammar;
use crate::grammar::{
    expr::{Block, Expr},
    Fallible, Lt, Parameter, ParameterKind, RigidName, Ty,
};
use crate::prove::prove::Env;
use formality_core::Upcast;
use libspecr::hidden::GcCow;
use libspecr::list;
use libspecr::prelude::{Int, List, Map};
use minirust_rs::lang;
use minirust_rs::mem::PtrType;

use super::minirust::*;
use super::scope::{CodegenFn, CodegenGlobal, CodegenScope, MonoKey};
use super::seme_region::SemeRegion;

// ===========================================================================
// Function setup and build
// ===========================================================================

/// Set up function arguments: alloc locals, build scope.
pub(super) fn setup_fn_args(
    cfn: impl Upcast<CodegenFn>,
    fn_data: &grammar::FnBoundData,
) -> Fallible<(
    lang::LocalName,
    List<lang::LocalName>,
    CodegenScope,
    CodegenFn,
)> {
    let mut cfn: CodegenFn = cfn.upcast();
    let ret_ty = minirust_ty(&cfn.crates, &fn_data.output_ty)?;
    let (ret_local, f2) = cfn.alloc_local(ret_ty);
    cfn = f2;
    let mut arg_locals = list![];
    let flow_state = FlowState::for_fn_body(&Env::default(), &fn_data.input_args)?;
    let mut scope = CodegenScope::new(ret_local, flow_state);
    for arg in &fn_data.input_args {
        let mr_ty = minirust_ty(&cfn.crates, &arg.ty)?;
        let (local, f2) = cfn.alloc_local(mr_ty);
        cfn = f2;
        arg_locals.push(local);
        scope = scope.push_var_no_flow(arg.id.clone(), local, arg.ty.clone());
    }
    Ok((ret_local, arg_locals, scope, cfn))
}

/// Build a `lang::Function` from codegen results.
pub(super) fn build_function(
    cfn: &CodegenFn,
    region: impl Upcast<SemeRegion>,
    ret_local: &lang::LocalName,
    arg_locals: &List<lang::LocalName>,
) -> lang::Function {
    let mut region: SemeRegion = region.upcast();
    let ret_local = *ret_local;
    let mut cfn = cfn.clone();
    if region.has_fallthrough() {
        region = region.with_stmt(lang::Statement::Assign {
            destination: lang::PlaceExpr::Local(ret_local),
            source: unit_value(),
        });
        (region, cfn) = cfn.terminate(region, lang::Terminator::Return, ());
    }
    let entry = region.entry();
    let mut blocks = region.into_blocks();
    let mut skip = vec![ret_local];
    for al in arg_locals.clone() {
        skip.push(al);
    }
    let sl: Vec<lang::Statement> = cfn
        .locals
        .iter()
        .filter(|(n, _)| !skip.contains(&n.0))
        .map(|(n, _)| lang::Statement::StorageLive(n.0))
        .collect();
    if let Some(mut bb) = blocks.remove(entry) {
        let mut ns: List<lang::Statement> = sl.into_iter().collect();
        for s in bb.statements {
            ns.push(s);
        }
        bb.statements = ns;
        blocks.insert(entry, bb);
    }
    let locals_map: Map<lang::LocalName, lang::Type> =
        cfn.locals.iter().map(|(k, v)| (k.0, v.0)).collect();
    lang::Function {
        locals: locals_map,
        args: arg_locals.clone(),
        ret: ret_local,
        calling_convention: lang::CallingConvention::Rust,
        blocks,
        start: entry,
    }
}

/// Resolve a MonoKey to its FnData and body block.
pub(super) fn resolve_fn_body(
    g: &CodegenGlobal,
    key: &MonoKey,
) -> Fallible<(grammar::FnBoundData, Block)> {
    let fn_def = g.crates.fn_named(&key.id)?;
    let fn_data = fn_def.binder.instantiate_with(&key.args)?;
    let body = match &fn_data.body {
        grammar::MaybeFnBody::FnBody(grammar::FnBody::Expr(b)) => b.clone(),
        _ => anyhow::bail!("function {:?} must have expression body", key.id),
    };
    Ok((fn_data, body))
}

/// Extract the single result from a ProvenSet, or error.
pub(super) fn unwrap_proven<T: std::fmt::Debug + Clone + Ord>(
    ps: formality_core::ProvenSet<T>,
) -> Fallible<T> {
    let proven = ps
        .into_singleton()
        .map_err(|e| anyhow::anyhow!("{}", e.format_leaves()))?;
    Ok(proven.0)
}

// ===========================================================================
// Borrow-checker wrappers
// ===========================================================================

pub(super) fn assert_no_constraints(state: &FlowState) {
    assert!(
        state.current.outlives.is_empty(),
        "codegen: unexpected outlives constraints: {:?}",
        state.current.outlives
    );
    assert!(
        state.current.loans_live.is_empty(),
        "codegen: unexpected live loans: {:?}",
        state.current.loans_live
    );
    assert!(
        state.breaks.is_empty(),
        "codegen: unexpected breaks: {:?}",
        state.breaks
    );
    assert!(
        state.continues.is_empty(),
        "codegen: unexpected continues: {:?}",
        state.continues
    );
}

pub(super) fn typed_place_to_minirust(
    cfn: &CodegenFn,
    s: &CodegenScope,
    typed: &TypedPlaceExpr,
) -> Fallible<lang::PlaceExpr> {
    match typed.data() {
        TypedPlaceExpressionData::Local(id) => {
            let (local, _) = s.lookup_var(id)?;
            Ok(lang::PlaceExpr::Local(local))
        }
        TypedPlaceExpressionData::Deref(prefix) => {
            let pp = typed_place_to_minirust(cfn, s, prefix)?;
            let pointee_ty = &typed.ty;
            Ok(lang::PlaceExpr::Deref {
                operand: GcCow::new(lang::ValueExpr::Load {
                    source: GcCow::new(pp),
                }),
                ty: minirust_ty(&cfn.crates, pointee_ty)?,
            })
        }
        TypedPlaceExpressionData::Field(prefix, field_name) => {
            let pp = typed_place_to_minirust(cfn, s, prefix)?;
            let prefix_rigid = match &prefix.ty {
                Ty::RigidTy(r) => r,
                _ => anyhow::bail!("field on non-rigid type"),
            };
            let idx = match &prefix_rigid.name {
                RigidName::AdtId(id) => {
                    struct_field_index(&cfn.crates, id, &prefix_rigid.parameters, field_name)?.0
                }
                RigidName::Tuple(_) => match field_name {
                    grammar::FieldName::Index(i) => *i,
                    _ => anyhow::bail!("non-index field on tuple"),
                },
                _ => anyhow::bail!("field on non-struct/tuple"),
            };
            Ok(lang::PlaceExpr::Field {
                root: GcCow::new(pp),
                field: Int::from(idx),
            })
        }
    }
}

// ===========================================================================
// Data construction helpers
// ===========================================================================

pub(super) fn alloc_temps_for_args(
    cfn: &CodegenFn,
    scope: &CodegenScope,
    args: &[Expr],
) -> Fallible<(Vec<MiniRustLocal>, CodegenFn)> {
    let mut f = cfn.clone();
    let mut temps = Vec::new();
    for arg in args {
        let arg_ty = unwrap_proven(super::type_expr(f.clone(), scope.clone(), arg.clone()))?;
        let (temp, f2) = f.alloc_temp(&arg_ty)?;
        f = f2;
        temps.push(temp);
    }
    Ok((temps, f))
}

pub(super) fn alloc_temps_for_fields(
    cfn: &CodegenFn,
    fields: &[grammar::Field],
) -> Fallible<(Vec<MiniRustLocal>, CodegenFn)> {
    let mut f = cfn.clone();
    let mut temps = Vec::new();
    for field in fields {
        let (temp, f2) = f.alloc_temp(&field.ty)?;
        f = f2;
        temps.push(temp);
    }
    Ok((temps, f))
}

pub(super) fn resolve_struct_fields(
    cfn: &CodegenFn,
    adt_id: &grammar::AdtId,
    turbofish: &grammar::expr::Turbofish,
) -> Fallible<Vec<grammar::Field>> {
    let s = cfn.crates.struct_named(adt_id)?;
    let bd = s.binder.instantiate_with(&turbofish.parameters)?;
    Ok(bd.fields)
}

pub(super) fn find_field_expr<'a>(
    field_exprs: &'a [grammar::expr::FieldExpr],
    field: &grammar::Field,
) -> Fallible<&'a Expr> {
    field_exprs
        .iter()
        .find(|fe| fe.name == field.name)
        .map(|fe| &fe.value)
        .ok_or_else(|| anyhow::anyhow!("missing field {:?}", field.name))
}

pub(super) fn instantiate_erased(binder: &grammar::Binder<Block>) -> Fallible<Block> {
    let params: Vec<Parameter> = binder
        .kinds()
        .iter()
        .map(|k| match k {
            ParameterKind::Lt => Ok(Lt::Erased.upcast()),
            ParameterKind::Ty => anyhow::bail!("exists with type param"),
            ParameterKind::Const => anyhow::bail!("exists with const param"),
        })
        .collect::<Fallible<_>>()?;
    binder.instantiate_with(&params)
}

pub(super) fn require_label(
    label: &Option<grammar::expr::Label>,
) -> Fallible<&grammar::expr::Label> {
    label
        .as_ref()
        .ok_or_else(|| anyhow::anyhow!("loop must have a label"))
}

pub(super) fn build_loop(
    cfn: &CodegenFn,
    loop_start: impl Upcast<MiniRustBb>,
    exit: impl Upcast<MiniRustBb>,
    body: impl Upcast<SemeRegion>,
) -> Fallible<(SemeRegion, CodegenFn)> {
    let loop_start: MiniRustBb = loop_start.upcast();
    let exit: MiniRustBb = exit.upcast();
    let body: SemeRegion = body.upcast();
    let region = SemeRegion::named(loop_start.into());
    let (mut region, mut cfn) = cfn.append(region, body);
    if region.has_fallthrough() {
        (region, cfn) = cfn.terminate(region, terminator_goto(loop_start), Some(exit));
    } else {
        (region, cfn) = cfn.terminate(region, lang::Terminator::Unreachable, Some(exit));
    }
    Ok((region, cfn))
}

/// Build the MiniRust `_start` function that serves as the program entry point.
///
/// MiniRust programs begin execution at a designated `start` function with C calling
/// convention. This function calls the user's `main`, then invokes the `Exit` intrinsic
/// to terminate the program. The structure is:
///
/// ```text
/// _start:
///   bb0: StorageLive(ret), StorageLive(main_ret)
///         → Call main() into bb1
///   bb1: → Intrinsic::Exit
/// ```
pub(super) fn build_start_function(
    g: &mut CodegenGlobal,
    main_fn_name: lang::FnName,
    main_ret_ty: lang::Type,
) -> (lang::FnName, lang::Function) {
    let (sr, g2) = g.fresh_fn();
    *g = g2;
    let start_ret = lang::LocalName(sr.0);
    let (smr, g2) = g.fresh_fn();
    *g = g2;
    let start_main_ret = lang::LocalName(smr.0);
    let (cb, g2) = g.fresh_fn();
    *g = g2;
    let call_bb = lang::BbName(cb.0);
    let (eb, g2) = g.fresh_fn();
    *g = g2;
    let exit_bb = lang::BbName(eb.0);

    let call_block = lang::BasicBlock {
        statements: list![
            lang::Statement::StorageLive(start_ret),
            lang::Statement::StorageLive(start_main_ret)
        ],
        terminator: lang::Terminator::Call {
            callee: lang::ValueExpr::Constant(
                lang::Constant::FnPointer(main_fn_name),
                lang::Type::Ptr(PtrType::FnPtr),
            ),
            calling_convention: lang::CallingConvention::Rust,
            arguments: list![],
            ret: lang::PlaceExpr::Local(start_main_ret),
            next_block: Some(exit_bb),
            unwind_block: None,
        },
        kind: lang::BbKind::Regular,
    };
    let exit_block = lang::BasicBlock {
        statements: list![],
        terminator: lang::Terminator::Intrinsic {
            intrinsic: lang::IntrinsicOp::Exit,
            arguments: list![],
            ret: lang::PlaceExpr::Local(start_ret),
            next_block: None,
        },
        kind: lang::BbKind::Regular,
    };

    let (sfn, g2) = g.fresh_fn();
    *g = g2;
    let start_fn_name = sfn;
    let mut locals = Map::new();
    locals.insert(start_ret, unit_ty());
    locals.insert(start_main_ret, main_ret_ty);
    let start_fn = lang::Function {
        locals,
        args: list![],
        ret: start_ret,
        calling_convention: lang::CallingConvention::C,
        blocks: {
            let mut m = Map::new();
            m.insert(call_bb, call_block);
            m.insert(exit_bb, exit_block);
            m
        },
        start: call_bb,
    };
    (start_fn_name, start_fn)
}
