//! Judgment-based codegen: translates formality-rust programs to MiniRust.
//!
//! Uses explicit state threading with `CodegenGlobal` (returned from all functions)
//! and `CodegenScope` (passed in but not returned).

use crate::grammar::{
    expr::{Block, Expr, ExprData, LabelId, PlaceExpr, PlaceExprData, Stmt},
    Crates, Fallible, Lt, Parameter, ParameterKind, RigidName, RigidTy, ScalarId, Ty, TyData,
    ValueId,
};
use formality_core::Upcast;
use libspecr::hidden::GcCow;
use libspecr::prelude::*;
use minirust_rs::lang;
use minirust_rs::mem::{PtrType, TupleHeadLayout};

mod seme_region;
use seme_region::SemeRegion;

mod test;

// ---------------------------------------------------------------------------
// Monomorphization key
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub(crate) struct MonoKey {
    id: ValueId,
    args: Vec<Parameter>,
}

// ---------------------------------------------------------------------------
// CodegenGlobal — monotonically grows, threaded through all functions
// ---------------------------------------------------------------------------

pub(crate) struct CodegenGlobal<'a> {
    crates: &'a Crates,
    local_counter: u32,
    bb_counter: u32,
    fn_counter: u32,
    locals: Vec<(lang::LocalName, lang::Type)>,
    /// MonoKey → FnName. Grows lazily as codegen discovers function references.
    fn_map: Vec<(MonoKey, lang::FnName)>,
}

// ---------------------------------------------------------------------------
// CodegenScope — extends as we enter blocks, discarded on exit
// ---------------------------------------------------------------------------

pub(crate) struct CodegenScope {
    vars: Vec<(ValueId, lang::LocalName, Ty)>,
    loop_scopes: Vec<LoopScope>,
    ret_local: lang::LocalName,
}

struct LoopScope {
    label: LabelId,
    loop_start: lang::BbName,
    exit_block: lang::BbName,
}

// ---------------------------------------------------------------------------
// CodegenGlobal helpers
// ---------------------------------------------------------------------------

impl<'a> CodegenGlobal<'a> {
    fn new(crates: &'a Crates) -> Self {
        CodegenGlobal {
            crates,
            local_counter: 0,
            bb_counter: 0,
            fn_counter: 0,
            locals: Vec::new(),
            fn_map: Vec::new(),
        }
    }

    fn fresh_name(&mut self) -> libspecr::Name {
        let n = self.local_counter;
        self.local_counter += 1;
        libspecr::Name::from_internal(n)
    }

    fn fresh_local(&mut self) -> lang::LocalName {
        lang::LocalName(self.fresh_name())
    }

    fn fresh_bb(&mut self) -> lang::BbName {
        let n = self.bb_counter;
        self.bb_counter += 1;
        lang::BbName(libspecr::Name::from_internal(n))
    }

    fn fresh_fn(&mut self) -> lang::FnName {
        let n = self.fn_counter;
        self.fn_counter += 1;
        lang::FnName(libspecr::Name::from_internal(n))
    }

    fn alloc_local(&mut self, ty: lang::Type) -> lang::LocalName {
        let name = self.fresh_local();
        self.locals.push((name, ty));
        name
    }

    /// Ensure a function is in fn_map. Returns its FnName.
    fn ensure_fn(&mut self, key: MonoKey) -> lang::FnName {
        for (k, name) in &self.fn_map {
            if *k == key {
                return *name;
            }
        }
        let name = self.fresh_fn();
        self.fn_map.push((key, name));
        name
    }

    /// Find the next MonoKey not yet in `done`.
    fn next_pending(
        &self,
        done: &Map<lang::FnName, lang::Function>,
    ) -> Option<(MonoKey, lang::FnName)> {
        self.fn_map
            .iter()
            .find(|(_, name)| !done.contains_key(*name))
            .cloned()
    }

    fn minirust_ty(&self, ty: &Ty) -> Fallible<lang::Type> {
        minirust_ty(self.crates, ty)
    }

    /// Reset per-function state (locals, counters) for a new function.
    fn reset_for_function(&mut self) {
        self.locals.clear();
        self.local_counter = 0;
        self.bb_counter = 0;
    }
}

// ---------------------------------------------------------------------------
// CodegenScope helpers
// ---------------------------------------------------------------------------

impl CodegenScope {
    fn new(ret_local: lang::LocalName) -> Self {
        CodegenScope {
            vars: Vec::new(),
            loop_scopes: Vec::new(),
            ret_local,
        }
    }

    fn lookup_var(&self, id: &ValueId) -> Fallible<(lang::LocalName, Ty)> {
        self.vars
            .iter()
            .rev()
            .find(|(name, _, _)| name == id)
            .map(|(_, local, ty)| (*local, ty.clone()))
            .ok_or_else(|| anyhow::anyhow!("unbound variable `{id:?}`"))
    }

    fn lookup_loop(&self, label: &LabelId) -> Fallible<(lang::BbName, lang::BbName)> {
        self.loop_scopes
            .iter()
            .rev()
            .find(|s| s.label == *label)
            .map(|s| (s.loop_start, s.exit_block))
            .ok_or_else(|| anyhow::anyhow!("no loop with label `{label:?}`"))
    }

    fn push_var(&mut self, id: ValueId, local: lang::LocalName, ty: Ty) {
        self.vars.push((id, local, ty));
    }

    fn push_loop(&mut self, label: LabelId, loop_start: lang::BbName, exit_block: lang::BbName) {
        self.loop_scopes.push(LoopScope {
            label,
            loop_start,
            exit_block,
        });
    }

    fn pop_loop(&mut self) {
        self.loop_scopes.pop();
    }
}

// ---------------------------------------------------------------------------
// Type helpers (reused from old codegen)
// ---------------------------------------------------------------------------

fn minirust_ty(crates: &Crates, ty: &Ty) -> Fallible<lang::Type> {
    match ty {
        Ty::RigidTy(rigid_ty) => match &rigid_ty.name {
            RigidName::ScalarId(scalar_id) => scalar_minirust_ty(scalar_id),
            RigidName::AdtId(adt_id) => struct_minirust_ty(crates, adt_id, &rigid_ty.parameters),
            RigidName::Never => Ok(unit_ty()),
            RigidName::Ref(ref_kind) => ref_minirust_ty(crates, ref_kind, &rigid_ty.parameters),
            RigidName::Tuple(arity) => tuple_minirust_ty(crates, *arity, &rigid_ty.parameters),
            RigidName::FnDef(_) => Ok(unit_ty()),
            RigidName::Raw(_) => unimplemented!("raw pointers"),
            RigidName::FnPtr(_) => unimplemented!("fnptrs"),
        },
        TyData::AliasTy(_) => unimplemented!("alias types"),
        TyData::PredicateTy(_) => unimplemented!("predicate types"),
        TyData::Variable(v) => anyhow::bail!("expected monomorphized input, found {v:?}"),
    }
}

fn scalar_minirust_ty(s: &ScalarId) -> Fallible<lang::Type> {
    let (signed, size) = match s {
        ScalarId::U8 => (Signedness::Unsigned, 1),
        ScalarId::U16 => (Signedness::Unsigned, 2),
        ScalarId::U32 => (Signedness::Unsigned, 4),
        ScalarId::U64 => (Signedness::Unsigned, 8),
        ScalarId::I8 => (Signedness::Signed, 1),
        ScalarId::I16 => (Signedness::Signed, 2),
        ScalarId::I32 => (Signedness::Signed, 4),
        ScalarId::I64 => (Signedness::Signed, 8),
        ScalarId::Bool => return Ok(lang::Type::Bool),
        ScalarId::Usize => (Signedness::Unsigned, 8),
        ScalarId::Isize => (Signedness::Signed, 8),
    };
    Ok(lang::Type::Int(lang::IntType {
        signed,
        size: libspecr::Size::from_bytes_const(size),
    }))
}

fn ref_minirust_ty(
    crates: &Crates,
    ref_kind: &crate::grammar::RefKind,
    parameters: &[Parameter],
) -> Fallible<lang::Type> {
    let pointee_ty = match parameters.get(1) {
        Some(Parameter::Ty(ty)) => ty.as_ref(),
        _ => anyhow::bail!("ref type missing pointee parameter"),
    };
    let pointee_mr_ty = minirust_ty(crates, pointee_ty)?;
    let (pointee_size, pointee_align) = type_size_align(&pointee_mr_ty);
    let mutbl = match ref_kind {
        crate::grammar::RefKind::Shared => Mutability::Immutable,
        crate::grammar::RefKind::Mut => Mutability::Mutable,
    };
    Ok(lang::Type::Ptr(PtrType::Ref {
        mutbl,
        pointee: minirust_rs::mem::PointeeInfo {
            layout: minirust_rs::mem::LayoutStrategy::Sized(pointee_size, pointee_align),
            inhabited: true,
            freeze: true,
            unpin: true,
            unsafe_cells: minirust_rs::mem::UnsafeCellStrategy::Sized { cells: list![] },
        },
    }))
}

fn tuple_minirust_ty(
    crates: &Crates,
    arity: usize,
    parameters: &[Parameter],
) -> Fallible<lang::Type> {
    if arity == 0 {
        return Ok(unit_ty());
    }
    layout_fields(
        parameters.iter().filter_map(|p| {
            if let Parameter::Ty(ty) = p {
                Some(ty.as_ref())
            } else {
                None
            }
        }),
        crates,
    )
}

fn struct_minirust_ty(
    crates: &Crates,
    adt_id: &crate::grammar::AdtId,
    parameters: &[Parameter],
) -> Fallible<lang::Type> {
    let s = crates.struct_named(adt_id)?;
    let bound_data = if parameters.is_empty() {
        let (_, data) = s.binder.open();
        data
    } else {
        s.binder.instantiate_with(parameters)?
    };
    layout_fields(bound_data.fields.iter().map(|f| &f.ty), crates)
}

/// Lay out a sequence of typed fields into a MiniRust Tuple type.
fn layout_fields<'a>(
    fields: impl Iterator<Item = &'a Ty>,
    crates: &Crates,
) -> Fallible<lang::Type> {
    let mut offset = Size::ZERO;
    let mut max_align = Align::ONE;
    let mut sized_fields: Vec<(Size, lang::Type)> = Vec::new();

    for ty in fields {
        let field_ty = minirust_ty(crates, ty)?;
        let (field_size, field_align) = type_size_align(&field_ty);
        let align_bytes = field_align.bytes();
        let offset_bytes = offset.bytes();
        let aligned = (offset_bytes + align_bytes - 1) / align_bytes * align_bytes;
        offset = Size::from_bytes(aligned).unwrap();
        sized_fields.push((offset, field_ty));
        offset = Size::from_bytes(aligned + field_size.bytes()).unwrap();
        if field_align.bytes() > max_align.bytes() {
            max_align = field_align;
        }
    }

    let total_bytes = offset.bytes();
    let align_bytes = max_align.bytes();
    let padded = (total_bytes + align_bytes - 1) / align_bytes * align_bytes;
    let total_size = Size::from_bytes(padded).unwrap();

    Ok(lang::Type::Tuple {
        sized_fields: sized_fields.into_iter().collect(),
        sized_head_layout: TupleHeadLayout {
            end: total_size,
            align: max_align,
            packed_align: None,
        },
        unsized_field: GcCow::new(None),
    })
}

fn type_size_align(ty: &lang::Type) -> (Size, Align) {
    match ty {
        lang::Type::Int(int_ty) => {
            let size = int_ty.size;
            let align = size.min(Size::from_bytes_const(8));
            (size, Align::from_bytes(align.bytes()).unwrap())
        }
        lang::Type::Bool => (Size::from_bytes_const(1), Align::ONE),
        lang::Type::Tuple {
            sized_head_layout, ..
        } => (sized_head_layout.end, sized_head_layout.align),
        lang::Type::Ptr(_) => (Size::from_bytes_const(8), Align::from_bytes(8).unwrap()),
        _ => unimplemented!("type_size_align for {:?}", ty),
    }
}

fn unit_ty() -> lang::Type {
    lang::Type::Tuple {
        sized_fields: list![],
        sized_head_layout: TupleHeadLayout {
            end: Size::ZERO,
            align: Align::ONE,
            packed_align: None,
        },
        unsized_field: GcCow::new(None),
    }
}

fn unit_value() -> lang::ValueExpr {
    lang::ValueExpr::Tuple(list![], unit_ty())
}

fn scalar_ty(s: ScalarId) -> Ty {
    RigidTy {
        name: RigidName::ScalarId(s),
        parameters: vec![],
    }
    .upcast()
}

fn struct_field_index(
    crates: &Crates,
    adt_id: &crate::grammar::AdtId,
    parameters: &[Parameter],
    field_name: &crate::grammar::FieldName,
) -> Fallible<(usize, Ty)> {
    let s = crates.struct_named(adt_id)?;
    let bound_data = if parameters.is_empty() {
        let (_, data) = s.binder.open();
        data
    } else {
        s.binder.instantiate_with(parameters)?
    };
    for (i, field) in bound_data.fields.iter().enumerate() {
        if field.name == *field_name {
            return Ok((i, field.ty.clone()));
        }
    }
    anyhow::bail!("no field {:?} in struct {:?}", field_name, adt_id)
}

// ---------------------------------------------------------------------------
// Expression codegen
// ---------------------------------------------------------------------------

fn codegen_expr_into(
    global: &mut CodegenGlobal,
    scope: &CodegenScope,
    target: lang::LocalName,
    expr: &Expr,
) -> Fallible<SemeRegion> {
    match expr.data() {
        ExprData::Literal { value, ty } => {
            let mr_ty = global.minirust_ty(&scalar_ty(ty.clone()))?;
            let source = lang::ValueExpr::Constant(lang::Constant::Int(Int::from(*value)), mr_ty);
            let mut region = SemeRegion::empty(global);
            region.push_stmt(lang::Statement::Assign {
                destination: lang::PlaceExpr::Local(target),
                source,
            });
            Ok(region)
        }
        ExprData::Place(place_expr) => {
            let source_place = codegen_place_expr(global, scope, place_expr)?;
            let mut region = SemeRegion::empty(global);
            region.push_stmt(lang::Statement::Assign {
                destination: lang::PlaceExpr::Local(target),
                source: lang::ValueExpr::Load {
                    source: GcCow::new(source_place),
                },
            });
            Ok(region)
        }
        ExprData::True | ExprData::False => {
            let val = matches!(expr.data(), ExprData::True);
            let mut region = SemeRegion::empty(global);
            region.push_stmt(lang::Statement::Assign {
                destination: lang::PlaceExpr::Local(target),
                source: lang::ValueExpr::Constant(lang::Constant::Bool(val), lang::Type::Bool),
            });
            Ok(region)
        }
        ExprData::Assign { place, expr } => {
            let dest_place = codegen_place_expr(global, scope, place)?;
            let rhs_ty = infer_expr_ty(global, scope, expr)?;
            let mr_ty = global.minirust_ty(&rhs_ty)?;
            let rhs_temp = global.alloc_local(mr_ty);
            let mut region = codegen_expr_into(global, scope, rhs_temp, expr)?;
            region.push_stmt(lang::Statement::Assign {
                destination: dest_place,
                source: lang::ValueExpr::Load {
                    source: GcCow::new(lang::PlaceExpr::Local(rhs_temp)),
                },
            });
            region.push_stmt(lang::Statement::Assign {
                destination: lang::PlaceExpr::Local(target),
                source: unit_value(),
            });
            Ok(region)
        }
        ExprData::Turbofish { id, args } => {
            let key = MonoKey {
                id: id.clone(),
                args: args.clone(),
            };
            let _fn_name = global.ensure_fn(key);
            let mut region = SemeRegion::empty(global);
            region.push_stmt(lang::Statement::Assign {
                destination: lang::PlaceExpr::Local(target),
                source: unit_value(),
            });
            Ok(region)
        }
        ExprData::Call { callee, args } => {
            let fn_name = resolve_call(global, scope, callee)?;
            let mut region = SemeRegion::empty(global);
            let mut arg_exprs = list![];
            for arg in args {
                let arg_ty = infer_expr_ty(global, scope, arg)?;
                let mr_ty = global.minirust_ty(&arg_ty)?;
                let arg_temp = global.alloc_local(mr_ty);
                let arg_region = codegen_expr_into(global, scope, arg_temp, arg)?;
                region = region.append(global, arg_region);
                arg_exprs.push(lang::ArgumentExpr::ByValue(lang::ValueExpr::Load {
                    source: GcCow::new(lang::PlaceExpr::Local(arg_temp)),
                }));
            }
            let next_bb = global.fresh_bb();
            region.terminate(lang::Terminator::Call {
                callee: lang::ValueExpr::Constant(
                    lang::Constant::FnPointer(fn_name),
                    lang::Type::Ptr(PtrType::FnPtr),
                ),
                calling_convention: lang::CallingConvention::Rust,
                arguments: arg_exprs,
                ret: lang::PlaceExpr::Local(target),
                next_block: Some(next_bb),
                unwind_block: None,
            });
            region.add_empty_block(next_bb);
            Ok(region)
        }
        ExprData::Ref { kind, lt: _, place } => {
            let place_expr = codegen_place_expr(global, scope, place)?;
            let pointee_ty = infer_place_ty(global, scope, place)?;
            let pointee_mr_ty = global.minirust_ty(&pointee_ty)?;
            let (pointee_size, pointee_align) = type_size_align(&pointee_mr_ty);
            let ref_kind = match kind {
                crate::grammar::RefKind::Shared => Mutability::Immutable,
                crate::grammar::RefKind::Mut => Mutability::Mutable,
            };
            let ptr_ty = PtrType::Ref {
                mutbl: ref_kind,
                pointee: minirust_rs::mem::PointeeInfo {
                    layout: minirust_rs::mem::LayoutStrategy::Sized(pointee_size, pointee_align),
                    inhabited: true,
                    freeze: true,
                    unpin: true,
                    unsafe_cells: minirust_rs::mem::UnsafeCellStrategy::Sized { cells: list![] },
                },
            };
            let mut region = SemeRegion::empty(global);
            region.push_stmt(lang::Statement::Assign {
                destination: lang::PlaceExpr::Local(target),
                source: lang::ValueExpr::AddrOf {
                    target: GcCow::new(place_expr),
                    ptr_ty,
                },
            });
            Ok(region)
        }
        ExprData::Struct {
            field_exprs,
            adt_id,
            turbofish,
        } => {
            let mut region = SemeRegion::empty(global);
            let s = global.crates.struct_named(adt_id)?;
            let bound_data = if turbofish.parameters.is_empty() {
                let (_, data) = s.binder.open();
                data
            } else {
                s.binder.instantiate_with(&turbofish.parameters)?
            };
            let mut field_values = Vec::new();
            for field_def in &bound_data.fields {
                let field_expr = field_exprs
                    .iter()
                    .find(|fe| fe.name == field_def.name)
                    .ok_or_else(|| anyhow::anyhow!("missing field {:?}", field_def.name))?;
                let mr_ty = minirust_ty(global.crates, &field_def.ty)?;
                let temp = global.alloc_local(mr_ty);
                let expr_region = codegen_expr_into(global, scope, temp, &field_expr.value)?;
                region = region.append(global, expr_region);
                field_values.push(lang::ValueExpr::Load {
                    source: GcCow::new(lang::PlaceExpr::Local(temp)),
                });
            }
            let struct_ty = minirust_ty(
                global.crates,
                &Ty::rigid(
                    RigidName::AdtId(adt_id.clone()),
                    turbofish.parameters.clone(),
                ),
            )?;
            region.push_stmt(lang::Statement::Assign {
                destination: lang::PlaceExpr::Local(target),
                source: lang::ValueExpr::Tuple(field_values.into_iter().collect(), struct_ty),
            });
            Ok(region)
        }
        _ => anyhow::bail!("codegen not yet implemented for this expression"),
    }
}

// ---------------------------------------------------------------------------
// Place expression codegen
// ---------------------------------------------------------------------------

fn codegen_place_expr(
    global: &CodegenGlobal,
    scope: &CodegenScope,
    place: &PlaceExpr,
) -> Fallible<lang::PlaceExpr> {
    match place.data() {
        PlaceExprData::Var(id) => {
            let (local, _ty) = scope.lookup_var(id)?;
            Ok(lang::PlaceExpr::Local(local))
        }
        PlaceExprData::Parens(inner) => codegen_place_expr(global, scope, inner),
        PlaceExprData::Deref { prefix } => {
            let prefix_place = codegen_place_expr(global, scope, prefix)?;
            let prefix_ty = infer_place_ty(global, scope, prefix)?;
            let pointee_ty = match &prefix_ty {
                Ty::RigidTy(r) => match &r.name {
                    RigidName::Ref(_) => match r.parameters.get(1) {
                        Some(Parameter::Ty(ty)) => ty.as_ref().clone(),
                        _ => anyhow::bail!("ref type missing pointee parameter"),
                    },
                    _ => anyhow::bail!("deref on non-reference type"),
                },
                _ => anyhow::bail!("deref on non-reference type"),
            };
            let pointee_mr_ty = minirust_ty(global.crates, &pointee_ty)?;
            Ok(lang::PlaceExpr::Deref {
                operand: GcCow::new(lang::ValueExpr::Load {
                    source: GcCow::new(prefix_place),
                }),
                ty: pointee_mr_ty,
            })
        }
        PlaceExprData::Field { prefix, field_name } => {
            let prefix_place = codegen_place_expr(global, scope, prefix)?;
            let prefix_ty = infer_place_ty(global, scope, prefix)?;
            match &prefix_ty {
                Ty::RigidTy(r) => match &r.name {
                    RigidName::AdtId(adt_id) => {
                        let (idx, _) =
                            struct_field_index(global.crates, adt_id, &r.parameters, field_name)?;
                        Ok(lang::PlaceExpr::Field {
                            root: GcCow::new(prefix_place),
                            field: Int::from(idx),
                        })
                    }
                    _ => anyhow::bail!("field access on non-struct type"),
                },
                _ => anyhow::bail!("field access on non-struct type"),
            }
        }
        _ => anyhow::bail!("codegen not yet implemented for this place expression"),
    }
}

// ---------------------------------------------------------------------------
// Call resolution
// ---------------------------------------------------------------------------

fn resolve_call(
    global: &mut CodegenGlobal,
    scope: &CodegenScope,
    callee: &Expr,
) -> Fallible<lang::FnName> {
    match callee.data() {
        ExprData::Turbofish { id, args } => {
            let key = MonoKey {
                id: id.clone(),
                args: args.clone(),
            };
            Ok(global.ensure_fn(key))
        }
        ExprData::Place(place) => {
            let ty = infer_place_ty(global, scope, place)?;
            match &ty {
                Ty::RigidTy(r) => match &r.name {
                    RigidName::FnDef(id) => {
                        let key = MonoKey {
                            id: id.clone(),
                            args: vec![],
                        };
                        Ok(global.ensure_fn(key))
                    }
                    _ => anyhow::bail!("cannot call non-function type: {ty:?}"),
                },
                _ => anyhow::bail!("cannot call non-function type: {ty:?}"),
            }
        }
        _ => anyhow::bail!("unsupported callee expression"),
    }
}

// ---------------------------------------------------------------------------
// Type inference (for codegen)
// ---------------------------------------------------------------------------

fn infer_expr_ty(global: &CodegenGlobal, scope: &CodegenScope, expr: &Expr) -> Fallible<Ty> {
    match expr.data() {
        ExprData::Literal { ty, .. } => Ok(scalar_ty(ty.clone())),
        ExprData::True | ExprData::False => Ok(Ty::bool()),
        ExprData::Place(place) => infer_place_ty(global, scope, place),
        ExprData::Ref { kind, lt, place } => {
            let pointee_ty = infer_place_ty(global, scope, place)?;
            Ok(pointee_ty.ref_ty_of_kind(kind, lt.clone()))
        }
        ExprData::Assign { .. } => Ok(Ty::unit()),
        ExprData::Struct {
            adt_id, turbofish, ..
        } => Ok(Ty::rigid(
            RigidName::AdtId(adt_id.clone()),
            turbofish.parameters.clone(),
        )),
        ExprData::Turbofish { id, .. } => Ok(Ty::rigid(RigidName::FnDef(id.clone()), ())),
        ExprData::Call { callee, .. } => {
            let callee_ty = infer_expr_ty(global, scope, callee)?;
            match &callee_ty {
                Ty::RigidTy(r) => match &r.name {
                    RigidName::FnDef(id) => {
                        let fn_def = global.crates.fn_named(id)?;
                        let (_, fn_data) = fn_def.binder.open();
                        Ok(fn_data.output_ty.clone())
                    }
                    _ => anyhow::bail!("cannot infer return type of non-function"),
                },
                _ => anyhow::bail!("cannot infer return type of non-function"),
            }
        }
        _ => anyhow::bail!("cannot infer type of expression"),
    }
}

fn infer_place_ty(global: &CodegenGlobal, scope: &CodegenScope, place: &PlaceExpr) -> Fallible<Ty> {
    match place.data() {
        PlaceExprData::Var(id) => match scope.lookup_var(id) {
            Ok((_, ty)) => Ok(ty),
            Err(_) if global.crates.fn_named(id).is_ok() => {
                Ok(Ty::rigid(RigidName::FnDef(id.clone()), ()))
            }
            Err(e) => Err(e),
        },
        PlaceExprData::Parens(inner) => infer_place_ty(global, scope, inner),
        PlaceExprData::Deref { prefix } => {
            let prefix_ty = infer_place_ty(global, scope, prefix)?;
            match &prefix_ty {
                Ty::RigidTy(r) => match &r.name {
                    RigidName::Ref(_) => match r.parameters.get(1) {
                        Some(Parameter::Ty(ty)) => Ok(ty.as_ref().clone()),
                        _ => anyhow::bail!("ref type missing pointee parameter"),
                    },
                    _ => anyhow::bail!("deref on non-reference type"),
                },
                _ => anyhow::bail!("deref on non-reference type"),
            }
        }
        PlaceExprData::Field { prefix, field_name } => {
            let prefix_ty = infer_place_ty(global, scope, prefix)?;
            match &prefix_ty {
                Ty::RigidTy(r) => match &r.name {
                    RigidName::AdtId(adt_id) => {
                        let (_, field_ty) =
                            struct_field_index(global.crates, adt_id, &r.parameters, field_name)?;
                        Ok(field_ty)
                    }
                    _ => anyhow::bail!("field access on non-struct type"),
                },
                _ => anyhow::bail!("field access on non-struct type"),
            }
        }
        _ => anyhow::bail!("cannot infer type of place expression"),
    }
}

// ---------------------------------------------------------------------------
// Statement and block codegen
// ---------------------------------------------------------------------------

fn codegen_block(
    global: &mut CodegenGlobal,
    scope: &mut CodegenScope,
    block: &Block,
) -> Fallible<SemeRegion> {
    let mut region = SemeRegion::empty(global);
    for stmt in &block.stmts {
        let stmt_region = codegen_stmt(global, scope, stmt)?;
        region = region.append(global, stmt_region);
    }
    Ok(region)
}

fn codegen_stmt(
    global: &mut CodegenGlobal,
    scope: &mut CodegenScope,
    stmt: &Stmt,
) -> Fallible<SemeRegion> {
    match stmt {
        Stmt::Let {
            label: _,
            id,
            ty,
            init,
        } => {
            let mr_ty = global.minirust_ty(ty)?;
            let local = global.alloc_local(mr_ty);
            scope.push_var(id.clone(), local, ty.clone());
            if let Some(init) = init {
                codegen_expr_into(global, scope, local, &init.expr)
            } else {
                Ok(SemeRegion::empty(global))
            }
        }
        Stmt::Return { expr } => {
            let ret_local = scope.ret_local;
            let mut region = codegen_expr_into(global, scope, ret_local, expr)?;
            region.terminate(lang::Terminator::Return);
            Ok(region)
        }
        Stmt::Print { expr } => {
            let expr_ty = infer_expr_ty(global, scope, expr)?;
            let mr_ty = global.minirust_ty(&expr_ty)?;
            let temp = global.alloc_local(mr_ty);
            let mut region = codegen_expr_into(global, scope, temp, expr)?;
            let next_bb = global.fresh_bb();
            let print_ret = global.alloc_local(unit_ty());
            region.terminate(lang::Terminator::Intrinsic {
                intrinsic: lang::IntrinsicOp::PrintStdout,
                arguments: list![lang::ValueExpr::Load {
                    source: GcCow::new(lang::PlaceExpr::Local(temp)),
                }],
                ret: lang::PlaceExpr::Local(print_ret),
                next_block: Some(next_bb),
            });
            region.add_empty_block(next_bb);
            Ok(region)
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            let cond_temp = global.alloc_local(lang::Type::Bool);
            let cond_region = codegen_expr_into(global, scope, cond_temp, condition)?;
            let then_region = codegen_block(global, scope, then_block)?;
            let else_region = codegen_block(global, scope, else_block)?;
            Ok(cond_region.branch_on_bool(global, cond_temp, then_region, else_region))
        }
        Stmt::Expr { expr } => {
            let expr_ty = infer_expr_ty(global, scope, expr)?;
            let mr_ty = global.minirust_ty(&expr_ty)?;
            let temp = global.alloc_local(mr_ty);
            codegen_expr_into(global, scope, temp, expr)
        }
        Stmt::Loop { label, body } => {
            let label = label
                .as_ref()
                .ok_or_else(|| anyhow::anyhow!("loop must have a label for codegen"))?;
            let loop_start = global.fresh_bb();
            let exit_block = global.fresh_bb();

            scope.push_loop(label.id.clone(), loop_start, exit_block);
            let body_region = codegen_block(global, scope, body)?;
            scope.pop_loop();

            let mut region = SemeRegion::empty(global);
            region.terminate(lang::Terminator::Goto(loop_start));
            region.add_empty_block(loop_start);
            region = region.append(global, body_region);
            if region.has_fallthrough() {
                region.terminate(lang::Terminator::Goto(loop_start));
            }
            region.add_empty_block(exit_block);
            Ok(region)
        }
        Stmt::Break { label } => {
            let (_, exit_block) = scope.lookup_loop(label)?;
            let mut region = SemeRegion::empty(global);
            region.terminate(lang::Terminator::Goto(exit_block));
            Ok(region)
        }
        Stmt::Continue { label } => {
            let (loop_start, _) = scope.lookup_loop(label)?;
            let mut region = SemeRegion::empty(global);
            region.terminate(lang::Terminator::Goto(loop_start));
            Ok(region)
        }
        Stmt::Block(block) => codegen_block(global, scope, block),
        Stmt::Exists { binder } => {
            let params: Vec<Parameter> = binder
                .kinds()
                .iter()
                .map(|kind| match kind {
                    ParameterKind::Lt => Ok(Lt::Erased.upcast()),
                    ParameterKind::Ty => anyhow::bail!("exists block with type parameter"),
                    ParameterKind::Const => anyhow::bail!("exists block with const parameter"),
                })
                .collect::<Fallible<_>>()?;
            let block = binder.instantiate_with(&params)?;
            codegen_block(global, scope, &block)
        }
        _ => anyhow::bail!("codegen not yet implemented for this statement"),
    }
}

// ---------------------------------------------------------------------------
// Function codegen
// ---------------------------------------------------------------------------

fn codegen_function(global: &mut CodegenGlobal, key: &MonoKey) -> Fallible<lang::Function> {
    global.reset_for_function();

    let fn_def = global.crates.fn_named(&key.id)?;
    let fn_data = if key.args.is_empty() {
        let (_, data) = fn_def.binder.open();
        data
    } else {
        fn_def.binder.instantiate_with(&key.args)?
    };

    let body_block = match &fn_data.body {
        crate::grammar::MaybeFnBody::FnBody(crate::grammar::FnBody::Expr(block)) => block,
        _ => anyhow::bail!("function {:?} must have an expression body", key.id),
    };

    let ret_ty = minirust_ty(global.crates, &fn_data.output_ty)?;
    let ret_local = global.alloc_local(ret_ty);

    let mut arg_locals = list![];
    let mut scope = CodegenScope::new(ret_local);
    for arg in &fn_data.input_args {
        let mr_ty = minirust_ty(global.crates, &arg.ty)?;
        let local = global.alloc_local(mr_ty);
        arg_locals.push(local);
        scope.push_var(arg.id.clone(), local, arg.ty.clone());
    }

    let mut region = codegen_block(global, &mut scope, body_block)?;

    // Finalize: if there's a fallthrough, assign unit to ret and return
    if region.has_fallthrough() {
        region.push_stmt(lang::Statement::Assign {
            destination: lang::PlaceExpr::Local(ret_local),
            source: unit_value(),
        });
        region.terminate(lang::Terminator::Return);
    }

    let entry = region.entry();
    let mut blocks = region.into_blocks();

    // Prepend StorageLive for non-arg, non-ret locals
    let mut skip_locals = vec![ret_local];
    for al in arg_locals {
        skip_locals.push(al);
    }
    let storage_lives: Vec<lang::Statement> = global
        .locals
        .iter()
        .filter(|(name, _)| !skip_locals.contains(name))
        .map(|(name, _)| lang::Statement::StorageLive(*name))
        .collect();
    if let Some((_, entry_bb)) = blocks.first_mut() {
        let mut new_stmts: List<lang::Statement> = storage_lives.into_iter().collect();
        for s in entry_bb.statements {
            new_stmts.push(s);
        }
        entry_bb.statements = new_stmts;
    }

    let locals_map: Map<lang::LocalName, lang::Type> =
        global.locals.iter().map(|(k, v)| (*k, *v)).collect();
    let blocks_map: Map<lang::BbName, lang::BasicBlock> = blocks.into_iter().collect();

    Ok(lang::Function {
        locals: locals_map,
        args: arg_locals,
        ret: ret_local,
        calling_convention: lang::CallingConvention::Rust,
        blocks: blocks_map,
        start: entry,
    })
}

// ---------------------------------------------------------------------------
// Top-level entry point
// ---------------------------------------------------------------------------

pub fn codegen_program(crates: &Crates) -> Fallible<lang::Program> {
    let mut global = CodegenGlobal::new(crates);

    let main_key = MonoKey {
        id: crate::rust::term("main"),
        args: vec![],
    };
    let main_fn_name = global.ensure_fn(main_key);

    // Process worklist until convergence
    let mut functions: Map<lang::FnName, lang::Function> = Map::new();
    while let Some((key, fn_name)) = global.next_pending(&functions) {
        let function = codegen_function(&mut global, &key)?;
        functions.insert(fn_name, function);
    }

    // Build synthetic _start
    let start_ret = lang::LocalName(global.fresh_fn().0);
    let start_main_ret = lang::LocalName(global.fresh_fn().0);
    let call_bb = lang::BbName(global.fresh_fn().0);
    let exit_bb = lang::BbName(global.fresh_fn().0);

    let main_ret_ty = functions
        .get(main_fn_name)
        .map(|f| f.locals.get(f.ret).unwrap())
        .unwrap();

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

    let start_fn_name = global.fresh_fn();
    let mut start_locals = Map::new();
    start_locals.insert(start_ret, unit_ty());
    start_locals.insert(start_main_ret, main_ret_ty);

    let start_function = lang::Function {
        locals: start_locals,
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

    functions.insert(start_fn_name, start_function);

    Ok(lang::Program {
        functions,
        start: start_fn_name,
        globals: Map::new(),
        traits: Map::new(),
        vtables: Map::new(),
    })
}
