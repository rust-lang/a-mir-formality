//! Judgment-based codegen: translates formality-rust programs to MiniRust.

use crate::check::borrow_check::env::TypeckEnv;
use crate::check::borrow_check::flow_state::FlowState;
use crate::check::borrow_check::nll::{borrow_check_place_expr, prove_ty_is_rigid};
use crate::check::borrow_check::typed_place_expression::{
    TypedPlaceExpr, TypedPlaceExpressionData,
};
use crate::grammar::{
    expr::{Block, Expr, ExprData, LabelId, PlaceExpr, Stmt},
    Crates, Fallible, Lt, Parameter, ParameterKind, RigidName, RigidTy, ScalarId, Ty, TyData,
    ValueId, Wcs,
};
use crate::prove::prove::{Env, Program};
use formality_core::{judgment_fn, Upcast};
use libspecr::hidden::GcCow;
use libspecr::list;
use libspecr::prelude::{Align, Int, List, Map, Mutability, Signedness, Size};
use minirust_rs::lang;
use minirust_rs::mem::{PtrType, TupleHeadLayout};
use std::sync::Arc;

mod ord_by_debug;
use ord_by_debug::OrdByDebug;

mod seme_region;
use seme_region::SemeRegion;

mod test;

// Wrapped specr types for use in judgment signatures
type MrLocal = OrdByDebug<lang::LocalName>;
type MrType = OrdByDebug<lang::Type>;
type MrFn = OrdByDebug<lang::FnName>;
type MrBb = OrdByDebug<lang::BbName>;

fn wrap_local(l: lang::LocalName) -> MrLocal {
    OrdByDebug(l)
}
fn wrap_ty(t: lang::Type) -> MrType {
    OrdByDebug(t)
}
fn wrap_fn(f: lang::FnName) -> MrFn {
    OrdByDebug(f)
}
fn wrap_bb(b: lang::BbName) -> MrBb {
    OrdByDebug(b)
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct MonoKey {
    id: ValueId,
    args: Vec<Parameter>,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct CodegenGlobal {
    crates: Crates,
    typeck_env: TypeckEnv,
    assumptions: Wcs,
    local_counter: u32,
    bb_counter: u32,
    fn_counter: u32,
    locals: Vec<(MrLocal, MrType)>,
    fn_map: Vec<(MonoKey, MrFn)>,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct CodegenScope {
    vars: Vec<(ValueId, MrLocal, Ty)>,
    loop_scopes: Vec<LoopScope>,
    ret_local: MrLocal,
    flow_state: FlowState,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct LoopScope {
    label: LabelId,
    loop_start: MrBb,
    exit_block: MrBb,
}

formality_core::cast_impl!(CodegenGlobal);
formality_core::cast_impl!(CodegenScope);
formality_core::cast_impl!(SemeRegion);

impl CodegenGlobal {
    fn new(crates: &Crates) -> Self {
        let program = Program {
            crates: Arc::new(crates.clone()),
            max_size: Program::DEFAULT_MAX_SIZE,
        };
        let typeck_env = TypeckEnv::for_const(Env::default(), &program);
        CodegenGlobal {
            crates: crates.clone(),
            typeck_env,
            assumptions: Wcs::t(),
            local_counter: 0,
            bb_counter: 0,
            fn_counter: 0,
            locals: Vec::new(),
            fn_map: Vec::new(),
        }
    }
    fn fresh_local(mut self) -> (lang::LocalName, Self) {
        let n = self.local_counter;
        self.local_counter += 1;
        (lang::LocalName(libspecr::Name::from_internal(n)), self)
    }
    fn fresh_bb(&self) -> (lang::BbName, Self) {
        let mut this = self.clone();
        let n = this.bb_counter;
        this.bb_counter += 1;
        (lang::BbName(libspecr::Name::from_internal(n)), this)
    }
    fn fresh_fn(mut self) -> (lang::FnName, Self) {
        let n = self.fn_counter;
        self.fn_counter += 1;
        (lang::FnName(libspecr::Name::from_internal(n)), self)
    }
    fn alloc_local(self, ty: lang::Type) -> (lang::LocalName, Self) {
        let (name, mut g) = self.fresh_local();
        g.locals.push((wrap_local(name), wrap_ty(ty)));
        (name, g)
    }
    fn ensure_fn(mut self, key: MonoKey) -> (lang::FnName, Self) {
        for (k, name) in &self.fn_map {
            if *k == key {
                return (name.0, self);
            }
        }
        let (name, g) = self.fresh_fn();
        self = g;
        self.fn_map.push((key, wrap_fn(name)));
        (name, self)
    }
    fn next_pending(
        &self,
        done: &Map<lang::FnName, lang::Function>,
    ) -> Option<(MonoKey, lang::FnName)> {
        self.fn_map
            .iter()
            .find(|(_, n)| !done.contains_key(n.0))
            .map(|(k, n)| (k.clone(), n.0))
    }
    fn minirust_ty(&self, ty: &Ty) -> Fallible<lang::Type> {
        minirust_ty(&self.crates, ty)
    }
    fn reset_for_function(mut self, output_ty: &Ty) -> Self {
        self.locals.clear();
        self.local_counter = 0;
        self.bb_counter = 0;
        self.typeck_env =
            TypeckEnv::for_fn_body(Env::default(), &self.typeck_env.program, output_ty);
        self.assumptions = Wcs::t();
        self
    }
}

impl CodegenScope {
    fn new(ret_local: lang::LocalName, flow_state: FlowState) -> Self {
        CodegenScope {
            vars: Vec::new(),
            loop_scopes: Vec::new(),
            ret_local: wrap_local(ret_local),
            flow_state,
        }
    }
    fn lookup_var(&self, id: &ValueId) -> Fallible<(lang::LocalName, Ty)> {
        self.vars
            .iter()
            .rev()
            .find(|(n, _, _)| n == id)
            .map(|(_, l, t)| (l.0, t.clone()))
            .ok_or_else(|| anyhow::anyhow!("unbound variable `{id:?}`"))
    }
    fn lookup_loop(&self, label: &LabelId) -> Fallible<(lang::BbName, lang::BbName)> {
        self.loop_scopes
            .iter()
            .rev()
            .find(|s| s.label == *label)
            .map(|s| (s.loop_start.0, s.exit_block.0))
            .ok_or_else(|| anyhow::anyhow!("no loop with label `{label:?}`"))
    }
    fn push_var(mut self, id: ValueId, local: lang::LocalName, ty: Ty) -> Fallible<Self> {
        self.flow_state = self
            .flow_state
            .with_local_in_scope(&Env::default(), &None, &id, &ty)?;
        self.vars.push((id, wrap_local(local), ty));
        Ok(self)
    }
    /// Add a variable mapping without updating FlowState.
    /// Used for input args that are already registered in FlowState via for_fn_body.
    fn push_var_no_flow(mut self, id: ValueId, local: lang::LocalName, ty: Ty) -> Self {
        self.vars.push((id, wrap_local(local), ty));
        self
    }
    fn push_loop(mut self, label: LabelId, start: lang::BbName, exit: lang::BbName) -> Self {
        self.loop_scopes.push(LoopScope {
            label,
            loop_start: wrap_bb(start),
            exit_block: wrap_bb(exit),
        });
        self
    }
    fn pop_loop(mut self) -> Self {
        self.loop_scopes.pop();
        self
    }
}

// ---------------------------------------------------------------------------
// Shared judgment wrappers
// ---------------------------------------------------------------------------

/// Verify that a FlowState returned from a shared judgment has no meaningful
/// side effects. With erased lifetimes, the shared judgments should never
/// produce outlives, loans, breaks, or continues.
fn assert_no_constraints(state: &FlowState) {
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

/// Call `borrow_check_place_expr` with erased lifetimes, assert no constraints,
/// return the `TypedPlaceExpr`.
fn resolve_place(
    g: &CodegenGlobal,
    s: &CodegenScope,
    place: &PlaceExpr,
) -> Fallible<TypedPlaceExpr> {
    let result = borrow_check_place_expr(
        g.typeck_env.clone(),
        g.assumptions.clone(),
        s.flow_state.clone(),
        place.clone(),
    );
    let (typed_place, returned_state) = unwrap_proven(result)?;
    assert_no_constraints(&returned_state);
    Ok(typed_place)
}

/// Call `prove_ty_is_rigid` with erased lifetimes, assert no constraints,
/// return the `RigidTy`.
fn resolve_rigid(g: &CodegenGlobal, s: &CodegenScope, ty: &Ty) -> Fallible<RigidTy> {
    let result = prove_ty_is_rigid(
        g.typeck_env.clone(),
        g.assumptions.clone(),
        s.flow_state.clone(),
        ty.clone(),
    );
    let (rigid_ty, returned_state) = unwrap_proven(result)?;
    assert_no_constraints(&returned_state);
    Ok(rigid_ty)
}

/// Translate a `TypedPlaceExpr` (from the borrow checker) into a MiniRust `lang::PlaceExpr`.
fn typed_place_to_minirust(
    g: &CodegenGlobal,
    s: &CodegenScope,
    typed: &TypedPlaceExpr,
) -> Fallible<lang::PlaceExpr> {
    match typed.data() {
        TypedPlaceExpressionData::Local(id) => {
            let (local, _) = s.lookup_var(id)?;
            Ok(lang::PlaceExpr::Local(local))
        }
        TypedPlaceExpressionData::Deref(prefix) => {
            let pp = typed_place_to_minirust(g, s, prefix)?;
            let pointee_ty = &typed.ty;
            Ok(lang::PlaceExpr::Deref {
                operand: GcCow::new(lang::ValueExpr::Load {
                    source: GcCow::new(pp),
                }),
                ty: minirust_ty(&g.crates, pointee_ty)?,
            })
        }
        TypedPlaceExpressionData::Field(prefix, field_name) => {
            let pp = typed_place_to_minirust(g, s, prefix)?;
            let prefix_rigid = match &prefix.ty {
                Ty::RigidTy(r) => r,
                _ => anyhow::bail!("field on non-rigid type"),
            };
            let idx = match &prefix_rigid.name {
                RigidName::AdtId(id) => {
                    struct_field_index(&g.crates, id, &prefix_rigid.parameters, field_name)?.0
                }
                RigidName::Tuple(_) => match field_name {
                    crate::grammar::FieldName::Index(i) => *i,
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

// ---------------------------------------------------------------------------
// Type helpers
// ---------------------------------------------------------------------------

fn minirust_ty(crates: &Crates, ty: &Ty) -> Fallible<lang::Type> {
    match ty {
        Ty::RigidTy(r) => match &r.name {
            RigidName::ScalarId(s) => scalar_minirust_ty(s),
            RigidName::AdtId(id) => struct_minirust_ty(crates, id, &r.parameters),
            RigidName::Never => Ok(unit_ty()),
            RigidName::Ref(k) => ref_minirust_ty(crates, k, &r.parameters),
            RigidName::Tuple(n) => tuple_minirust_ty(crates, *n, &r.parameters),
            RigidName::FnDef(_) => Ok(unit_ty()),
            RigidName::Raw(_) | RigidName::FnPtr(_) => unimplemented!(),
        },
        TyData::AliasTy(_) | TyData::PredicateTy(_) => unimplemented!(),
        TyData::Variable(v) => anyhow::bail!("unmonomorphized {v:?}"),
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
    kind: &crate::grammar::RefKind,
    params: &[Parameter],
) -> Fallible<lang::Type> {
    let pointee = match params.get(1) {
        Some(Parameter::Ty(t)) => t.as_ref(),
        _ => anyhow::bail!("bad ref"),
    };
    let mr = minirust_ty(crates, pointee)?;
    let (sz, al) = type_size_align(&mr);
    let mutbl = match kind {
        crate::grammar::RefKind::Shared => Mutability::Immutable,
        crate::grammar::RefKind::Mut => Mutability::Mutable,
    };
    Ok(lang::Type::Ptr(PtrType::Ref {
        mutbl,
        pointee: minirust_rs::mem::PointeeInfo {
            layout: minirust_rs::mem::LayoutStrategy::Sized(sz, al),
            inhabited: true,
            freeze: true,
            unpin: true,
            unsafe_cells: minirust_rs::mem::UnsafeCellStrategy::Sized { cells: list![] },
        },
    }))
}
fn tuple_minirust_ty(crates: &Crates, arity: usize, params: &[Parameter]) -> Fallible<lang::Type> {
    if arity == 0 {
        return Ok(unit_ty());
    }
    layout_fields(
        params.iter().filter_map(|p| {
            if let Parameter::Ty(t) = p {
                Some(t.as_ref())
            } else {
                None
            }
        }),
        crates,
    )
}
fn struct_minirust_ty(
    crates: &Crates,
    id: &crate::grammar::AdtId,
    params: &[Parameter],
) -> Fallible<lang::Type> {
    let s = crates.struct_named(id)?;
    let bd = if params.is_empty() {
        let (_, d) = s.binder.open();
        d
    } else {
        s.binder.instantiate_with(params)?
    };
    layout_fields(bd.fields.iter().map(|f| &f.ty), crates)
}
fn layout_fields<'a>(
    fields: impl Iterator<Item = &'a Ty>,
    crates: &Crates,
) -> Fallible<lang::Type> {
    let mut off = Size::ZERO;
    let mut ma = Align::ONE;
    let mut sf = Vec::new();
    for ty in fields {
        let ft = minirust_ty(crates, ty)?;
        let (fs, fa) = type_size_align(&ft);
        let ab = fa.bytes();
        let ob = off.bytes();
        let aligned = (ob + ab - 1) / ab * ab;
        off = Size::from_bytes(aligned).unwrap();
        sf.push((off, ft));
        off = Size::from_bytes(aligned + fs.bytes()).unwrap();
        if ab > ma.bytes() {
            ma = fa;
        }
    }
    let tb = off.bytes();
    let ab = ma.bytes();
    Ok(lang::Type::Tuple {
        sized_fields: sf.into_iter().collect(),
        sized_head_layout: TupleHeadLayout {
            end: Size::from_bytes((tb + ab - 1) / ab * ab).unwrap(),
            align: ma,
            packed_align: None,
        },
        unsized_field: GcCow::new(None),
    })
}
fn type_size_align(ty: &lang::Type) -> (Size, Align) {
    match ty {
        lang::Type::Int(i) => (
            i.size,
            Align::from_bytes(i.size.min(Size::from_bytes_const(8)).bytes()).unwrap(),
        ),
        lang::Type::Bool => (Size::from_bytes_const(1), Align::ONE),
        lang::Type::Tuple {
            sized_head_layout: h,
            ..
        } => (h.end, h.align),
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
    id: &crate::grammar::AdtId,
    params: &[Parameter],
    field: &crate::grammar::FieldName,
) -> Fallible<(usize, Ty)> {
    let s = crates.struct_named(id)?;
    let bd = if params.is_empty() {
        let (_, d) = s.binder.open();
        d
    } else {
        s.binder.instantiate_with(params)?
    };
    for (i, f) in bd.fields.iter().enumerate() {
        if f.name == *field {
            return Ok((i, f.ty.clone()));
        }
    }
    anyhow::bail!("no field {:?} in {:?}", field, id)
}

// ---------------------------------------------------------------------------
// Call resolution and expression type inference
// ---------------------------------------------------------------------------

fn resolve_call(
    g: CodegenGlobal,
    s: &CodegenScope,
    callee: &Expr,
) -> Fallible<(lang::FnName, CodegenGlobal)> {
    match callee.data() {
        ExprData::Turbofish { id, args } => {
            let (n, g) = g.ensure_fn(MonoKey {
                id: id.clone(),
                args: args.clone(),
            });
            Ok((n, g))
        }
        ExprData::Place(place) => {
            let ty = match place.data() {
                crate::grammar::expr::PlaceExprData::Var(id) => match s.lookup_var(id) {
                    Ok((_, ty)) => ty,
                    Err(_) if g.crates.fn_named(id).is_ok() => {
                        Ty::rigid(RigidName::FnDef(id.clone()), ())
                    }
                    Err(e) => return Err(e),
                },
                _ => resolve_place(&g, s, place)?.ty,
            };
            let rigid = resolve_rigid(&g, s, &ty)?;
            match &rigid.name {
                RigidName::FnDef(id) => {
                    let (n, g) = g.ensure_fn(MonoKey {
                        id: id.clone(),
                        args: vec![],
                    });
                    Ok((n, g))
                }
                _ => anyhow::bail!("call non-fn"),
            }
        }
        _ => anyhow::bail!("bad callee"),
    }
}

fn infer_expr_ty(g: &CodegenGlobal, s: &CodegenScope, e: &Expr) -> Fallible<Ty> {
    match e.data() {
        ExprData::Literal { ty, .. } => Ok(scalar_ty(ty.clone())),
        ExprData::True | ExprData::False => Ok(Ty::bool()),
        ExprData::Place(p) => Ok(match p.data() {
            crate::grammar::expr::PlaceExprData::Var(id) => match s.lookup_var(id) {
                Ok((_, ty)) => ty,
                Err(_) if g.crates.fn_named(id).is_ok() => {
                    Ty::rigid(RigidName::FnDef(id.clone()), ())
                }
                Err(e) => return Err(e),
            },
            _ => resolve_place(g, s, p)?.ty,
        }),
        ExprData::Ref { kind, lt, place } => Ok(resolve_place(g, s, place)?
            .ty
            .ref_ty_of_kind(kind, lt.clone())),
        ExprData::Assign { .. } => Ok(Ty::unit()),
        ExprData::Struct {
            adt_id, turbofish, ..
        } => Ok(Ty::rigid(
            RigidName::AdtId(adt_id.clone()),
            turbofish.parameters.clone(),
        )),
        ExprData::Turbofish { id, .. } => Ok(Ty::rigid(RigidName::FnDef(id.clone()), ())),
        ExprData::Call { callee, .. } => {
            let ct = infer_expr_ty(g, s, callee)?;
            let rigid = resolve_rigid(g, s, &ct)?;
            match &rigid.name {
                RigidName::FnDef(id) => {
                    let (_, d) = g.crates.fn_named(id)?.binder.open();
                    Ok(d.output_ty.clone())
                }
                _ => anyhow::bail!("ret ty of non-fn"),
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Judgments
// ---------------------------------------------------------------------------

/// Result type for codegen operations — wraps (SemeRegion, CodegenGlobal)
/// so it can be used in `(let r: CgResult = ...)` inside judgment_fn!
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct CgResult {
    region: SemeRegion,
    global: CodegenGlobal,
}

impl CgResult {
    fn new(region: impl Upcast<SemeRegion>, global: impl Upcast<CodegenGlobal>) -> Self {
        CgResult {
            region: region.upcast(),
            global: global.upcast(),
        }
    }
}

formality_core::cast_impl!(CgResult);

judgment_fn! {
    fn codegen_expr_into(
        global: CodegenGlobal,
        scope: CodegenScope,
        target: MrLocal,
        expr: Expr,
    ) => CgResult {
        debug(global, scope, target, expr)

        (
            (let mr_ty = minirust_ty(&global.crates, &scalar_ty(ty.clone()))?)
            (let (entry, g) = global.fresh_bb())
            (let region = SemeRegion::empty(entry)
                .push_stmt(lang::Statement::Assign {
                    destination: lang::PlaceExpr::Local(target.0),
                    source: lang::ValueExpr::Constant(
                        lang::Constant::Int(Int::from(*value)),
                        mr_ty.clone(),
                    ),
                }))
            ---- ("literal")
            (codegen_expr_into(global, scope, target, ExprData::Literal { value, ty }) => CgResult::new(region, g))
        )

        (
            (let typed = resolve_place(&global, &scope, place_expr)?)
            (let source = typed_place_to_minirust(&global, &scope, &typed)?)
            (let (entry, g) = global.fresh_bb())
            (let region = SemeRegion::empty(entry)
                .push_stmt(lang::Statement::Assign {
                    destination: lang::PlaceExpr::Local(target.0),
                    source: lang::ValueExpr::Load {
                        source: GcCow::new(source.clone()),
                    },
                }))
            ---- ("place")
            (codegen_expr_into(global, scope, target, ExprData::Place(place_expr)) => CgResult::new(region, g))
        )

        (
            (if matches!(expr.data(), ExprData::True | ExprData::False))
            (let r: CgResult = (|| -> Fallible<CgResult> {
                let val = matches!(expr.data(), ExprData::True);
                let (entry, g) = global.clone().fresh_bb();
                let region = SemeRegion::empty(&entry)
                    .push_stmt(lang::Statement::Assign {
                        destination: lang::PlaceExpr::Local(target.0),
                        source: lang::ValueExpr::Constant(lang::Constant::Bool(val), lang::Type::Bool),
                    });
                Ok(CgResult::new(region, g))
            })()?)
            ---- ("bool")
            (codegen_expr_into(global, scope, target, expr) => r.clone())
        )

        (
            (if let ExprData::Assign { place, expr: rhs } = expr.data())
            (let r: CgResult = (|| -> Fallible<CgResult> {
                let typed_dest = resolve_place(&global, &scope, place)?;
                let dest = typed_place_to_minirust(&global, &scope, &typed_dest)?;
                let rhs_ty = infer_expr_ty(&global, &scope, rhs)?;
                let mr_ty = global.minirust_ty(&rhs_ty)?;
                let (rhs_temp, mut g) = global.clone().alloc_local(mr_ty);
                let CgResult {
                    region,
                    global: g2,
                } = unwrap_proven(codegen_expr_into(
                    g,
                    scope.clone(),
                    wrap_local(rhs_temp),
                    rhs.clone(),
                ))?;
                g = g2;
                let region = region
                    .push_stmt(lang::Statement::Assign {
                        destination: dest,
                        source: lang::ValueExpr::Load {
                            source: GcCow::new(lang::PlaceExpr::Local(rhs_temp)),
                        },
                    })
                    .push_stmt(lang::Statement::Assign {
                        destination: lang::PlaceExpr::Local(target.0),
                        source: unit_value(),
                    });
                Ok(CgResult::new(region, g))
            })()?)
            ---- ("assign")
            (codegen_expr_into(global, scope, target, expr) => r.clone())
        )

        (
            (if let ExprData::Turbofish { id, args } = expr.data())
            (let r: CgResult = (|| -> Fallible<CgResult> {
                let (_fn, mut g) = global.clone().ensure_fn(MonoKey {
                    id: id.clone(),
                    args: args.to_vec(),
                });
                let (entry, g2) = g.fresh_bb();
                g = g2;
                let region = SemeRegion::empty(&entry)
                    .push_stmt(lang::Statement::Assign {
                        destination: lang::PlaceExpr::Local(target.0),
                        source: unit_value(),
                    });
                Ok(CgResult::new(region, g))
            })()?)
            ---- ("turbofish")
            (codegen_expr_into(global, scope, target, expr) => r.clone())
        )

        (
            (if let ExprData::Call { callee, args } = expr.data())
            (let r: CgResult = codegen_call(global.clone(), &scope, &target, callee, args)?)
            ---- ("call")
            (codegen_expr_into(global, scope, target, expr) => r.clone())
        )

        (
            (if let ExprData::Ref { kind, lt: _, place } = expr.data())
            (let r: CgResult = (|| -> Fallible<CgResult> {
                let typed = resolve_place(&global, &scope, place)?;
                let pe = typed_place_to_minirust(&global, &scope, &typed)?;
                let pt = &typed.ty;
                let mr = global.minirust_ty(pt)?;
                let (ps, pa) = type_size_align(&mr);
                let mutbl = match kind {
                    crate::grammar::RefKind::Shared => Mutability::Immutable,
                    crate::grammar::RefKind::Mut => Mutability::Mutable,
                };
                let ptr_ty = PtrType::Ref {
                    mutbl,
                    pointee: minirust_rs::mem::PointeeInfo {
                        layout: minirust_rs::mem::LayoutStrategy::Sized(ps, pa),
                        inhabited: true,
                        freeze: true,
                        unpin: true,
                        unsafe_cells: minirust_rs::mem::UnsafeCellStrategy::Sized { cells: list![] },
                    },
                };
                let (entry, g) = global.clone().fresh_bb();
                let region = SemeRegion::empty(&entry)
                    .push_stmt(lang::Statement::Assign {
                        destination: lang::PlaceExpr::Local(target.0),
                        source: lang::ValueExpr::AddrOf {
                            target: GcCow::new(pe),
                            ptr_ty,
                        },
                    });
                Ok(CgResult::new(region, g))
            })()?)
            ---- ("ref")
            (codegen_expr_into(global, scope, target, expr) => r.clone())
        )

        (
            (if let ExprData::Struct { field_exprs, adt_id, turbofish } = expr.data())
            (let r: CgResult = codegen_struct(global.clone(), &scope, &target, adt_id, turbofish, field_exprs)?)
            ---- ("struct")
            (codegen_expr_into(global, scope, target, expr) => r.clone())
        )
    }
}

judgment_fn! {
    fn codegen_stmt(
        global: CodegenGlobal,
        scope: CodegenScope,
        stmt: Stmt,
    ) => CgResult {
        debug(global, scope, stmt)

        (
            (if let Stmt::Let { label: _, id, ty, init } = &*stmt)
            (let r: CgResult = codegen_let(global.clone(), scope.clone(), id, ty, init.as_ref())?)
            ---- ("let")
            (codegen_stmt(global, scope, stmt) => r.clone())
        )

        (
            (if let Stmt::Return { expr } = &*stmt)
            (let r: CgResult = (|| -> Fallible<CgResult> {
                let ret = scope.ret_local.clone();
                let CgResult {
                    region,
                    global: g,
                } = unwrap_proven(codegen_expr_into(global.clone(), scope.clone(), ret, expr.clone()))?;
                let region = region.terminate(lang::Terminator::Return);
                Ok(CgResult::new(region, g))
            })()?)
            ---- ("return")
            (codegen_stmt(global, scope, stmt) => r.clone())
        )

        (
            (if let Stmt::Print { expr } = &*stmt)
            (let r: CgResult = codegen_print(global.clone(), &scope, expr)?)
            ---- ("print")
            (codegen_stmt(global, scope, stmt) => r.clone())
        )

        (
            (if let Stmt::If { condition, then_block, else_block } = &*stmt)
            (let r: CgResult = codegen_if(global.clone(), &scope, condition, then_block, else_block)?)
            ---- ("if")
            (codegen_stmt(global, scope, stmt) => r.clone())
        )

        (
            (if let Stmt::Expr { expr } = &*stmt)
            (let r: CgResult = (|| -> Fallible<CgResult> {
                let et = infer_expr_ty(&global, &scope, expr)?;
                let mr_ty = global.minirust_ty(&et)?;
                let (temp, g) = global.clone().alloc_local(mr_ty);
                unwrap_proven(codegen_expr_into(
                    g,
                    scope.clone(),
                    wrap_local(temp),
                    expr.clone(),
                ))
            })()?)
            ---- ("expr")
            (codegen_stmt(global, scope, stmt) => r.clone())
        )

        (
            (if let Stmt::Loop { label, body } = &*stmt)
            (let r: CgResult = codegen_loop(global.clone(), scope.clone(), label, body)?)
            ---- ("loop")
            (codegen_stmt(global, scope, stmt) => r.clone())
        )

        (
            (if let Stmt::Break { label } = &*stmt)
            (let r: CgResult = (|| -> Fallible<CgResult> {
                let (_, exit) = scope.lookup_loop(label)?;
                let (entry, g) = global.clone().fresh_bb();
                let region = SemeRegion::empty(&entry)
                    .terminate(lang::Terminator::Goto(exit));
                Ok(CgResult::new(region, g))
            })()?)
            ---- ("break")
            (codegen_stmt(global, scope, stmt) => r.clone())
        )

        (
            (if let Stmt::Continue { label } = &*stmt)
            (let r: CgResult = (|| -> Fallible<CgResult> {
                let (start, _) = scope.lookup_loop(label)?;
                let (entry, g) = global.clone().fresh_bb();
                let region = SemeRegion::empty(&entry)
                    .terminate(lang::Terminator::Goto(start));
                Ok(CgResult::new(region, g))
            })()?)
            ---- ("continue")
            (codegen_stmt(global, scope, stmt) => r.clone())
        )

        (
            (if let Stmt::Block(block) = &*stmt)
            (let r: CgResult = codegen_block_inner(global.clone(), scope.clone(), block)?)
            ---- ("block")
            (codegen_stmt(global, scope, stmt) => r.clone())
        )

        (
            (if let Stmt::Exists { binder } = &*stmt)
            (let r: CgResult = codegen_exists(global.clone(), scope.clone(), binder)?)
            ---- ("exists")
            (codegen_stmt(global, scope, stmt) => r.clone())
        )
    }
}

// ---------------------------------------------------------------------------
// Expression codegen helpers (regular functions returning CgResult)
// ---------------------------------------------------------------------------

fn codegen_call(
    mut g: CodegenGlobal,
    scope: &CodegenScope,
    target: &MrLocal,
    callee: &Expr,
    args: &[Expr],
) -> Fallible<CgResult> {
    let (fn_name, g2) = resolve_call(g, scope, callee)?;
    g = g2;
    let (entry, g2) = g.fresh_bb();
    g = g2;
    let mut region = SemeRegion::empty(&entry);
    let mut arg_exprs = list![];
    for arg in args {
        let arg_ty = infer_expr_ty(&g, scope, arg)?;
        let mr_ty = g.minirust_ty(&arg_ty)?;
        let (temp, g2) = g.alloc_local(mr_ty);
        g = g2;
        let CgResult {
            region: ar,
            global: g2,
        } = unwrap_proven(codegen_expr_into(
            g,
            scope.clone(),
            wrap_local(temp),
            arg.clone(),
        ))?;
        g = g2;
        region = region.append(ar, || {
            let (bb, _) = g.clone().fresh_bb();
            bb
        });
        arg_exprs.push(lang::ArgumentExpr::ByValue(lang::ValueExpr::Load {
            source: GcCow::new(lang::PlaceExpr::Local(temp)),
        }));
    }
    let (next_bb, g2) = g.fresh_bb();
    g = g2;
    region = region
        .terminate(lang::Terminator::Call {
            callee: lang::ValueExpr::Constant(
                lang::Constant::FnPointer(fn_name),
                lang::Type::Ptr(PtrType::FnPtr),
            ),
            calling_convention: lang::CallingConvention::Rust,
            arguments: arg_exprs,
            ret: lang::PlaceExpr::Local(target.0),
            next_block: Some(next_bb),
            unwind_block: None,
        })
        .add_empty_block(next_bb);
    Ok(CgResult::new(region, g))
}

fn codegen_struct(
    mut g: CodegenGlobal,
    scope: &CodegenScope,
    target: &MrLocal,
    adt_id: &crate::grammar::AdtId,
    turbofish: &crate::grammar::expr::Turbofish,
    field_exprs: &[crate::grammar::expr::FieldExpr],
) -> Fallible<CgResult> {
    let s = g.crates.struct_named(adt_id)?;
    let bd = if turbofish.parameters.is_empty() {
        let (_, d) = s.binder.open();
        d
    } else {
        s.binder.instantiate_with(&turbofish.parameters)?
    };
    let (entry, g2) = g.fresh_bb();
    g = g2;
    let mut region = SemeRegion::empty(&entry);
    let mut fv = Vec::new();
    for fd in &bd.fields {
        let fe = field_exprs
            .iter()
            .find(|fe| fe.name == fd.name)
            .ok_or_else(|| anyhow::anyhow!("missing field {:?}", fd.name))?;
        let mr_ty = minirust_ty(&g.crates, &fd.ty)?;
        let (temp, g2) = g.alloc_local(mr_ty);
        g = g2;
        let CgResult {
            region: er,
            global: g2,
        } = unwrap_proven(codegen_expr_into(
            g,
            scope.clone(),
            wrap_local(temp),
            fe.value.clone(),
        ))?;
        g = g2;
        region = region.append(er, || {
            let (bb, _) = g.clone().fresh_bb();
            bb
        });
        fv.push(lang::ValueExpr::Load {
            source: GcCow::new(lang::PlaceExpr::Local(temp)),
        });
    }
    let st = minirust_ty(
        &g.crates,
        &Ty::rigid(
            RigidName::AdtId(adt_id.clone()),
            turbofish.parameters.clone(),
        ),
    )?;
    let region = region.push_stmt(lang::Statement::Assign {
        destination: lang::PlaceExpr::Local(target.0),
        source: lang::ValueExpr::Tuple(fv.into_iter().collect(), st),
    });
    Ok(CgResult::new(region, g))
}

/// Extract the single result from a ProvenSet, or error.
fn unwrap_proven<T: std::fmt::Debug + Clone + Ord>(
    ps: formality_core::ProvenSet<T>,
) -> Fallible<T> {
    let proven = ps
        .into_singleton()
        .map_err(|e| anyhow::anyhow!("{}", e.format_leaves()))?;
    Ok(proven.0)
}

// ---------------------------------------------------------------------------
// Statement codegen helpers
// ---------------------------------------------------------------------------

fn codegen_let(
    mut g: CodegenGlobal,
    mut scope: CodegenScope,
    id: &ValueId,
    ty: &Ty,
    init: Option<&crate::grammar::expr::Init>,
) -> Fallible<CgResult> {
    let mr_ty = g.minirust_ty(ty)?;
    let (local, g2) = g.alloc_local(mr_ty);
    g = g2;
    scope = scope.push_var(id.clone(), local, ty.clone())?;
    if let Some(init) = init {
        let CgResult { region, global } = unwrap_proven(codegen_expr_into(
            g,
            scope,
            wrap_local(local),
            init.expr.clone(),
        ))?;
        Ok(CgResult::new(region, global))
    } else {
        let (entry, g2) = g.fresh_bb();
        g = g2;
        Ok(CgResult::new(SemeRegion::empty(&entry), g))
    }
}

fn codegen_print(mut g: CodegenGlobal, scope: &CodegenScope, expr: &Expr) -> Fallible<CgResult> {
    let et = infer_expr_ty(&g, scope, expr)?;
    let mr_ty = g.minirust_ty(&et)?;
    let (temp, g2) = g.alloc_local(mr_ty);
    g = g2;
    let CgResult { region, global: g2 } = unwrap_proven(codegen_expr_into(
        g,
        scope.clone(),
        wrap_local(temp),
        expr.clone(),
    ))?;
    g = g2;
    let (next_bb, g2) = g.fresh_bb();
    g = g2;
    let (print_ret, g2) = g.alloc_local(unit_ty());
    g = g2;
    let region = region
        .terminate(lang::Terminator::Intrinsic {
            intrinsic: lang::IntrinsicOp::PrintStdout,
            arguments: list![lang::ValueExpr::Load {
                source: GcCow::new(lang::PlaceExpr::Local(temp))
            }],
            ret: lang::PlaceExpr::Local(print_ret),
            next_block: Some(next_bb),
        })
        .add_empty_block(next_bb);
    Ok(CgResult::new(region, g))
}

fn codegen_if(
    mut g: CodegenGlobal,
    scope: &CodegenScope,
    cond: &Expr,
    then_block: &Block,
    else_block: &Block,
) -> Fallible<CgResult> {
    let (ct, g2) = g.alloc_local(lang::Type::Bool);
    g = g2;
    let CgResult {
        region: cr,
        global: g2,
    } = unwrap_proven(codegen_expr_into(
        g,
        scope.clone(),
        wrap_local(ct),
        cond.clone(),
    ))?;
    g = g2;
    let CgResult {
        region: tr,
        global: g2,
    } = codegen_block_inner(g, scope.clone(), then_block)?;
    g = g2;
    let CgResult {
        region: er,
        global: g2,
    } = codegen_block_inner(g, scope.clone(), else_block)?;
    g = g2;
    let (join, g2) = g.fresh_bb();
    g = g2;
    Ok(CgResult::new(cr.branch_on_bool(ct, tr, er, join), g))
}

fn codegen_loop(
    mut g: CodegenGlobal,
    mut scope: CodegenScope,
    label: &Option<crate::grammar::expr::Label>,
    body: &Block,
) -> Fallible<CgResult> {
    let label = label
        .as_ref()
        .ok_or_else(|| anyhow::anyhow!("loop must have a label"))?;
    let (loop_start, g2) = g.fresh_bb();
    g = g2;
    let (exit_block, g2) = g.fresh_bb();
    g = g2;
    scope = scope.push_loop(label.id.clone(), loop_start, exit_block);
    let CgResult {
        region: body_region,
        global: g2,
    } = codegen_block_inner(g, scope.clone(), body)?;
    g = g2;
    let _scope = scope.pop_loop();
    let (entry, g2) = g.fresh_bb();
    g = g2;
    let mut region = SemeRegion::empty(&entry)
        .terminate(lang::Terminator::Goto(loop_start))
        .add_empty_block(loop_start);
    region = region.append(body_region, || {
        let (bb, _) = g.clone().fresh_bb();
        bb
    });
    if region.has_fallthrough() {
        region = region.terminate(lang::Terminator::Goto(loop_start));
    }
    region = region.add_empty_block(exit_block);
    Ok(CgResult::new(region, g))
}

fn codegen_block_inner(
    mut g: CodegenGlobal,
    scope: CodegenScope,
    block: &Block,
) -> Fallible<CgResult> {
    let (entry, g2) = g.fresh_bb();
    g = g2;
    let mut region = SemeRegion::empty(&entry);
    let mut scope = scope;
    for stmt in &block.stmts {
        // Handle Let specially to thread scope
        if let Stmt::Let {
            label: _,
            id,
            ty,
            init,
        } = stmt
        {
            let mr_ty = g.minirust_ty(ty)?;
            let (local, g2) = g.alloc_local(mr_ty);
            g = g2;
            scope = scope.push_var(id.clone(), local, ty.clone())?;
            if let Some(init) = init {
                let CgResult {
                    region: ir,
                    global: g2,
                } = unwrap_proven(codegen_expr_into(
                    g,
                    scope.clone(),
                    wrap_local(local),
                    init.expr.clone(),
                ))?;
                g = g2;
                region = region.append(ir, || {
                    let (bb, _) = g.clone().fresh_bb();
                    bb
                });
            }
        } else {
            let CgResult {
                region: sr,
                global: g2,
            } = unwrap_proven(codegen_stmt(g, scope.clone(), stmt.clone()))?;
            g = g2;
            region = region.append(sr, || {
                let (bb, _) = g.clone().fresh_bb();
                bb
            });
        }
    }
    Ok(CgResult::new(region, g))
}

fn codegen_exists(
    g: CodegenGlobal,
    scope: CodegenScope,
    binder: &crate::grammar::Binder<Block>,
) -> Fallible<CgResult> {
    let params: Vec<Parameter> = binder
        .kinds()
        .iter()
        .map(|k| match k {
            ParameterKind::Lt => Ok(Lt::Erased.upcast()),
            ParameterKind::Ty => anyhow::bail!("exists with type param"),
            ParameterKind::Const => anyhow::bail!("exists with const param"),
        })
        .collect::<Fallible<_>>()?;
    let block = binder.instantiate_with(&params)?;
    codegen_block_inner(g, scope, &block)
}

// ---------------------------------------------------------------------------
// Function and program codegen
// ---------------------------------------------------------------------------

fn codegen_function(
    mut g: CodegenGlobal,
    key: &MonoKey,
) -> Fallible<(lang::Function, CodegenGlobal)> {
    let fn_def = g.crates.fn_named(&key.id)?;
    let fn_data = if key.args.is_empty() {
        let (_, d) = fn_def.binder.open();
        d
    } else {
        fn_def.binder.instantiate_with(&key.args)?
    };
    g = g.reset_for_function(&fn_data.output_ty);
    let body = match &fn_data.body {
        crate::grammar::MaybeFnBody::FnBody(crate::grammar::FnBody::Expr(b)) => b,
        _ => anyhow::bail!("function {:?} must have expression body", key.id),
    };
    let ret_ty = minirust_ty(&g.crates, &fn_data.output_ty)?;
    let (ret_local, g2) = g.alloc_local(ret_ty);
    g = g2;
    let mut arg_locals = list![];
    let flow_state = FlowState::for_fn_body(&Env::default(), &fn_data.input_args)?;
    let mut scope = CodegenScope::new(ret_local, flow_state);
    for arg in &fn_data.input_args {
        let mr_ty = minirust_ty(&g.crates, &arg.ty)?;
        let (local, g2) = g.alloc_local(mr_ty);
        g = g2;
        arg_locals.push(local);
        scope = scope.push_var_no_flow(arg.id.clone(), local, arg.ty.clone());
    }
    let CgResult {
        mut region,
        global: g2,
    } = codegen_block_inner(g, scope, body)?;
    g = g2;
    if region.has_fallthrough() {
        region = region
            .push_stmt(lang::Statement::Assign {
                destination: lang::PlaceExpr::Local(ret_local),
                source: unit_value(),
            })
            .terminate(lang::Terminator::Return);
    }
    let entry = region.entry();
    let mut blocks = region.into_blocks();
    // Prepend StorageLive
    let mut skip = vec![ret_local];
    for al in arg_locals {
        skip.push(al);
    }
    let sl: Vec<lang::Statement> = g
        .locals
        .iter()
        .filter(|(n, _)| !skip.contains(&n.0))
        .map(|(n, _)| lang::Statement::StorageLive(n.0))
        .collect();
    if let Some((_, bb)) = blocks.first_mut() {
        let mut ns: List<lang::Statement> = sl.into_iter().collect();
        for s in bb.statements {
            ns.push(s);
        }
        bb.statements = ns;
    }
    let locals_map: Map<lang::LocalName, lang::Type> =
        g.locals.iter().map(|(k, v)| (k.0, v.0)).collect();
    let blocks_map: Map<lang::BbName, lang::BasicBlock> = blocks.into_iter().collect();
    Ok((
        lang::Function {
            locals: locals_map,
            args: arg_locals,
            ret: ret_local,
            calling_convention: lang::CallingConvention::Rust,
            blocks: blocks_map,
            start: entry,
        },
        g,
    ))
}

pub fn codegen_program(crates: &Crates) -> Fallible<lang::Program> {
    let mut g = CodegenGlobal::new(crates);
    let main_key = MonoKey {
        id: crate::rust::term("main"),
        args: vec![],
    };
    let (main_fn_name, g2) = g.ensure_fn(main_key);
    g = g2;
    let mut functions: Map<lang::FnName, lang::Function> = Map::new();
    while let Some((key, fn_name)) = g.next_pending(&functions) {
        let (function, g2) = codegen_function(g, &key)?;
        g = g2;
        functions.insert(fn_name, function);
    }
    // Build _start
    let (sr, g2) = g.fresh_fn();
    g = g2;
    let start_ret = lang::LocalName(sr.0);
    let (smr, g2) = g.fresh_fn();
    g = g2;
    let start_main_ret = lang::LocalName(smr.0);
    let (cb, g2) = g.fresh_fn();
    g = g2;
    let call_bb = lang::BbName(cb.0);
    let (eb, g2) = g.fresh_fn();
    g = g2;
    let exit_bb = lang::BbName(eb.0);
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
    let (sfn, _g) = g.fresh_fn();
    let start_fn_name = sfn;
    let mut sl = Map::new();
    sl.insert(start_ret, unit_ty());
    sl.insert(start_main_ret, main_ret_ty);
    let start_fn = lang::Function {
        locals: sl,
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
    functions.insert(start_fn_name, start_fn);
    Ok(lang::Program {
        functions,
        start: start_fn_name,
        globals: Map::new(),
        traits: Map::new(),
        vtables: Map::new(),
    })
}
