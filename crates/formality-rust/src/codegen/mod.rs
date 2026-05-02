//! Judgment-based codegen: translates formality-rust programs to MiniRust.

use crate::check::borrow_check::env::TypeckEnv;
use crate::check::borrow_check::flow_state::FlowState;
use crate::check::borrow_check::nll::{borrow_check_place_expr, prove_ty_is_rigid};
use crate::check::borrow_check::typed_place_expression::{
    TypedPlaceExpr, TypedPlaceExpressionData,
};
use crate::grammar::{
    expr::{Block, Expr, ExprData, LabelId, PlaceExpr, PlaceExprData, Stmt},
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

/// Thin wrapper around `lang::PlaceExpr` for builder ergonomics.
#[derive(Clone, Debug)]
struct MrPlace(lang::PlaceExpr);

formality_core::cast_impl!(MrPlace);

impl formality_core::UpcastFrom<MrLocal> for MrPlace {
    fn upcast_from(l: MrLocal) -> Self {
        MrPlace(lang::PlaceExpr::Local(l.0))
    }
}

impl formality_core::UpcastFrom<lang::PlaceExpr> for MrPlace {
    fn upcast_from(p: lang::PlaceExpr) -> Self {
        MrPlace(p)
    }
}

/// Thin wrapper around `lang::ValueExpr` for builder ergonomics.
#[derive(Clone, Debug)]
struct MrValue(lang::ValueExpr);

formality_core::cast_impl!(MrValue);

impl formality_core::UpcastFrom<lang::ValueExpr> for MrValue {
    fn upcast_from(v: lang::ValueExpr) -> Self {
        MrValue(v)
    }
}

// Upcast from &lang::LocalName to MrLocal (for judgment macro bindings)
impl formality_core::UpcastFrom<lang::LocalName> for MrLocal {
    fn upcast_from(l: lang::LocalName) -> Self {
        OrdByDebug(l)
    }
}

// Upcast from &lang::BbName to MrBb
impl formality_core::UpcastFrom<lang::BbName> for MrBb {
    fn upcast_from(b: lang::BbName) -> Self {
        OrdByDebug(b)
    }
}

// Upcast from lang::FnName to MrFn
impl formality_core::UpcastFrom<lang::FnName> for MrFn {
    fn upcast_from(f: lang::FnName) -> Self {
        OrdByDebug(f)
    }
}

fn wrap_local(l: &lang::LocalName) -> MrLocal {
    OrdByDebug(*l)
}
fn wrap_ty(t: lang::Type) -> MrType {
    OrdByDebug(t)
}
fn wrap_fn(f: &lang::FnName) -> MrFn {
    OrdByDebug(*f)
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
    label_scopes: Vec<LabelScope>,
    ret_local: MrLocal,
    flow_state: FlowState,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct LabelScope {
    label: LabelId,
    continue_target: Option<MrBb>,
    break_target: MrBb,
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
    fn fresh_local(&self) -> (lang::LocalName, Self) {
        let mut this = self.clone();
        let n = this.local_counter;
        this.local_counter += 1;
        (lang::LocalName(libspecr::Name::from_internal(n)), this)
    }
    fn fresh_bb(&self) -> (lang::BbName, Self) {
        let mut this = self.clone();
        let n = this.bb_counter;
        this.bb_counter += 1;
        (lang::BbName(libspecr::Name::from_internal(n)), this)
    }
    fn fresh_fn(&self) -> (lang::FnName, Self) {
        let mut this = self.clone();
        let n = this.fn_counter;
        this.fn_counter += 1;
        (lang::FnName(libspecr::Name::from_internal(n)), this)
    }
    fn alloc_local(&self, ty: lang::Type) -> (lang::LocalName, Self) {
        let (name, mut g) = self.fresh_local();
        g.locals.push((wrap_local(&name), wrap_ty(ty)));
        (name, g)
    }
    fn ensure_fn(&self, key: MonoKey) -> (lang::FnName, Self) {
        for (k, name) in &self.fn_map {
            if *k == key {
                return (name.0, self.clone());
            }
        }
        let (name, mut g) = self.fresh_fn();
        g.fn_map.push((key, wrap_fn(&name)));
        (name, g)
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
    fn fresh_region(&self) -> (SemeRegion, Self) {
        let (bb, g) = self.fresh_bb();
        (SemeRegion::empty(&bb), g)
    }
    fn alloc_temp(&self, ty: &Ty) -> Fallible<(MrLocal, Self)> {
        let mr_ty = self.minirust_ty(ty)?;
        let (name, g) = self.alloc_local(mr_ty);
        Ok((wrap_local(&name), g))
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
            label_scopes: Vec::new(),
            ret_local: wrap_local(&ret_local),
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
    fn lookup_label(&self, label: &LabelId) -> Fallible<(Option<lang::BbName>, lang::BbName)> {
        self.label_scopes
            .iter()
            .rev()
            .find(|s| s.label == *label)
            .map(|s| (s.continue_target.as_ref().map(|b| b.0), s.break_target.0))
            .ok_or_else(|| anyhow::anyhow!("no label `{label:?}` in scope"))
    }
    fn push_var(&self, id: ValueId, local: lang::LocalName, ty: Ty) -> Fallible<Self> {
        let mut s = self.clone();
        s.flow_state = s
            .flow_state
            .with_local_in_scope(&Env::default(), &None, &id, &ty)?;
        s.vars.push((id, wrap_local(&local), ty));
        Ok(s)
    }
    /// Add a variable mapping without updating FlowState.
    /// Used for input args that are already registered in FlowState via for_fn_body.
    fn push_var_no_flow(mut self, id: ValueId, local: lang::LocalName, ty: Ty) -> Self {
        self.vars.push((id, wrap_local(&local), ty));
        self
    }
    fn push_label(
        &self,
        label: LabelId,
        continue_target: Option<impl Upcast<MrBb>>,
        break_target: impl Upcast<MrBb>,
    ) -> Self {
        let mut s = self.clone();
        s.label_scopes.push(LabelScope {
            label,
            continue_target: continue_target.map(|b| b.upcast()),
            break_target: break_target.upcast(),
        });
        s
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
// Builder API: SemeRegion methods
// ---------------------------------------------------------------------------

impl SemeRegion {
    /// Assign a value to a place. Wraps `push_stmt` with an `Assign` statement.
    pub fn assign(&self, dest: impl Upcast<MrPlace>, value: impl Upcast<MrValue>) -> Self {
        let dest: MrPlace = dest.upcast();
        let value: MrValue = value.upcast();
        self.clone().push_stmt(lang::Statement::Assign {
            destination: dest.0,
            source: value.0,
        })
    }

    /// Terminate the region (cloning variant for use in judgment conclusions).
    pub fn terminated(&self, terminator: lang::Terminator) -> Self {
        self.clone().terminate(terminator)
    }

    /// Append another region's blocks into this one, allocating a fresh bb from `global`.
    pub fn append_from(&self, global: &CodegenGlobal, other: impl Upcast<SemeRegion>) -> Self {
        self.clone().append(other.upcast(), || {
            let (bb, _) = global.fresh_bb();
            bb
        })
    }

    /// Build a Call terminator, allocate the next block, and return the updated region and global.
    pub fn call(
        &self,
        global: &CodegenGlobal,
        fn_name: impl Upcast<MrFn>,
        args: &[MrLocal],
        ret: impl Upcast<MrLocal>,
    ) -> Fallible<(Self, CodegenGlobal)> {
        let fn_name: MrFn = fn_name.upcast();
        let ret: MrLocal = ret.upcast();
        let arg_exprs: List<lang::ArgumentExpr> = args
            .iter()
            .map(|t| {
                lang::ArgumentExpr::ByValue(lang::ValueExpr::Load {
                    source: GcCow::new(lang::PlaceExpr::Local(t.0)),
                })
            })
            .collect();
        let (next_bb, g) = global.fresh_bb();
        let region = self
            .clone()
            .terminate(lang::Terminator::Call {
                callee: lang::ValueExpr::Constant(
                    lang::Constant::FnPointer(fn_name.0),
                    lang::Type::Ptr(PtrType::FnPtr),
                ),
                calling_convention: lang::CallingConvention::Rust,
                arguments: arg_exprs,
                ret: lang::PlaceExpr::Local(ret.0),
                next_block: Some(next_bb),
                unwind_block: None,
            })
            .add_empty_block(next_bb);
        Ok((region, g))
    }

    /// Build a branch-on-bool: switch on `cond`, then/else regions, join block.
    pub fn branch_on_bool_from(
        &self,
        global: &CodegenGlobal,
        cond: impl Upcast<MrLocal>,
        then_r: impl Upcast<SemeRegion>,
        else_r: impl Upcast<SemeRegion>,
    ) -> (Self, CodegenGlobal) {
        let cond: MrLocal = cond.upcast();
        let (join, g) = global.fresh_bb();
        (
            self.clone()
                .branch_on_bool(cond.0, then_r.upcast(), else_r.upcast(), join),
            g,
        )
    }

    /// Build a print intrinsic call.
    pub fn print_intrinsic(
        &self,
        global: &CodegenGlobal,
        value: impl Upcast<MrLocal>,
    ) -> Fallible<(Self, CodegenGlobal)> {
        let value: MrLocal = value.upcast();
        let (next_bb, mut g) = global.fresh_bb();
        let (print_ret, g2) = g.alloc_local(unit_ty());
        g = g2;
        let region = self
            .clone()
            .terminate(lang::Terminator::Intrinsic {
                intrinsic: lang::IntrinsicOp::PrintStdout,
                arguments: list![lang::ValueExpr::Load {
                    source: GcCow::new(lang::PlaceExpr::Local(value.0))
                }],
                ret: lang::PlaceExpr::Local(print_ret),
                next_block: Some(next_bb),
            })
            .add_empty_block(next_bb);
        Ok((region, g))
    }
}

// ---------------------------------------------------------------------------
// Builder API: free functions for MiniRust value/terminator construction
// ---------------------------------------------------------------------------

fn constant(value: &usize, ty: &ScalarId) -> MrValue {
    let mr_ty = scalar_minirust_ty(ty).expect("scalar type always valid");
    MrValue(lang::ValueExpr::Constant(
        lang::Constant::Int(Int::from(*value)),
        mr_ty,
    ))
}

fn bool_constant(val: bool) -> MrValue {
    MrValue(lang::ValueExpr::Constant(
        lang::Constant::Bool(val),
        lang::Type::Bool,
    ))
}

fn load(place: impl Upcast<MrPlace>) -> MrValue {
    let place: MrPlace = place.upcast();
    MrValue(lang::ValueExpr::Load {
        source: GcCow::new(place.0),
    })
}

fn addr_of(
    global: &CodegenGlobal,
    place: impl Upcast<MrPlace>,
    kind: &crate::grammar::RefKind,
    pointee_ty: &Ty,
) -> Fallible<MrValue> {
    let place: MrPlace = place.upcast();
    let mr = global.minirust_ty(pointee_ty)?;
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
    Ok(MrValue(lang::ValueExpr::AddrOf {
        target: GcCow::new(place.0),
        ptr_ty,
    }))
}

fn tuple_value(
    temps: &[MrLocal],
    adt_id: &crate::grammar::AdtId,
    turbofish: &crate::grammar::expr::Turbofish,
    crates: &Crates,
) -> Fallible<MrValue> {
    let fv: List<lang::ValueExpr> = temps
        .iter()
        .map(|t| lang::ValueExpr::Load {
            source: GcCow::new(lang::PlaceExpr::Local(t.0)),
        })
        .collect();
    let st = minirust_ty(
        crates,
        &Ty::rigid(
            RigidName::AdtId(adt_id.clone()),
            turbofish.parameters.clone(),
        ),
    )?;
    Ok(MrValue(lang::ValueExpr::Tuple(fv, st)))
}

fn terminator_return() -> lang::Terminator {
    lang::Terminator::Return
}

fn terminator_goto(bb: impl Upcast<MrBb>) -> lang::Terminator {
    let bb: MrBb = bb.upcast();
    lang::Terminator::Goto(bb.0)
}

fn bool_ty() -> lang::Type {
    lang::Type::Bool
}

// ---------------------------------------------------------------------------
// Builder API: helper functions for data construction
// ---------------------------------------------------------------------------

fn alloc_temps_for_args(
    global: &CodegenGlobal,
    scope: &CodegenScope,
    args: &[Expr],
) -> Fallible<(Vec<MrLocal>, CodegenGlobal)> {
    let mut g = global.clone();
    let mut temps = Vec::new();
    for arg in args {
        let arg_ty = infer_expr_ty(&g, scope, arg)?;
        let (temp, g2) = g.alloc_temp(&arg_ty)?;
        g = g2;
        temps.push(temp);
    }
    Ok((temps, g))
}

fn alloc_temps_for_fields(
    global: &CodegenGlobal,
    fields: &[crate::grammar::Field],
) -> Fallible<(Vec<MrLocal>, CodegenGlobal)> {
    let mut g = global.clone();
    let mut temps = Vec::new();
    for f in fields {
        let (temp, g2) = g.alloc_temp(&f.ty)?;
        g = g2;
        temps.push(temp);
    }
    Ok((temps, g))
}

fn resolve_struct_fields(
    global: &CodegenGlobal,
    adt_id: &crate::grammar::AdtId,
    turbofish: &crate::grammar::expr::Turbofish,
) -> Fallible<Vec<crate::grammar::Field>> {
    let s = global.crates.struct_named(adt_id)?;
    let bd = if turbofish.parameters.is_empty() {
        let (_, d) = s.binder.open();
        d
    } else {
        s.binder.instantiate_with(&turbofish.parameters)?
    };
    Ok(bd.fields.clone())
}

fn find_field_expr<'a>(
    field_exprs: &'a [crate::grammar::expr::FieldExpr],
    field: &crate::grammar::Field,
) -> Fallible<&'a Expr> {
    field_exprs
        .iter()
        .find(|fe| fe.name == field.name)
        .map(|fe| &fe.value)
        .ok_or_else(|| anyhow::anyhow!("missing field {:?}", field.name))
}

fn instantiate_erased(binder: &crate::grammar::Binder<Block>) -> Fallible<Block> {
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

fn require_label(
    label: &Option<crate::grammar::expr::Label>,
) -> Fallible<&crate::grammar::expr::Label> {
    label
        .as_ref()
        .ok_or_else(|| anyhow::anyhow!("loop must have a label"))
}

fn build_loop(
    global: &CodegenGlobal,
    loop_start: impl Upcast<MrBb>,
    exit: impl Upcast<MrBb>,
    body: impl Upcast<SemeRegion>,
) -> Fallible<(SemeRegion, CodegenGlobal)> {
    let loop_start: MrBb = loop_start.upcast();
    let exit: MrBb = exit.upcast();
    let body: SemeRegion = body.upcast();
    let (entry, g) = global.fresh_bb();
    let mut region = SemeRegion::empty(&entry)
        .terminate(lang::Terminator::Goto(loop_start.0))
        .add_empty_block(loop_start.0);
    region = region.append(body, || {
        let (bb, _) = g.fresh_bb();
        bb
    });
    if region.has_fallthrough() {
        region = region.terminate(lang::Terminator::Goto(loop_start.0));
    }
    region = region.add_empty_block(exit.0);
    Ok((region, g))
}

// ---------------------------------------------------------------------------
// Call resolution and expression type inference
// ---------------------------------------------------------------------------

judgment_fn! {
    fn resolve_call(
        global: CodegenGlobal,
        scope: CodegenScope,
        callee: Expr,
    ) => (MrFn, CodegenGlobal) {
        debug(global, scope, callee)

        (
            (let (n, global) = global.ensure_fn(MonoKey { id: id.clone(), args: args.to_vec() }))
            ---- ("turbofish")
            (resolve_call(global, scope, ExprData::Turbofish { id, args }) => (wrap_fn(n), global))
        )

        (
            (if scope.lookup_var(id).is_err())
            (if global.crates.fn_named(id).is_ok())
            (let (n, global) = global.ensure_fn(MonoKey { id: id.clone(), args: vec![] }))
            ---- ("var-fn")
            (resolve_call(global, scope, PlaceExprData::Var(id)) => (wrap_fn(n), global))
        )

        (
            (let typed = resolve_place(&global, &scope, place)?)
            (let rigid = resolve_rigid(&global, &scope, &typed.ty)?)
            (if let RigidName::FnDef(fn_id) = &rigid.name)
            (let (n, global) = global.ensure_fn(MonoKey { id: fn_id.clone(), args: vec![] }))
            ---- ("place")
            (resolve_call(global, scope, ExprData::Place(place)) => (wrap_fn(n), global))
        )
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

judgment_fn! {
    fn codegen_expr_into(
        global: CodegenGlobal,
        scope: CodegenScope,
        target: MrLocal,
        expr: Expr,
    ) => (SemeRegion, CodegenGlobal) {
        debug(global, scope, target, expr)

        (
            (let (region, global) = global.fresh_region())
            ---- ("literal")
            (codegen_expr_into(global, scope, target, ExprData::Literal { value, ty }) => (
                region.assign(target, constant(value, ty)),
                global,
            ))
        )

        (
            (let typed = resolve_place(&global, &scope, place_expr)?)
            (let source = typed_place_to_minirust(&global, &scope, &typed)?)
            (let (region, global) = global.fresh_region())
            ---- ("place")
            (codegen_expr_into(global, scope, target, ExprData::Place(place_expr)) => (
                region.assign(target, load(source)),
                global,
            ))
        )

        (
            (let (region, global) = global.fresh_region())
            ---- ("true")
            (codegen_expr_into(global, scope, target, ExprData::True) => (
                region.assign(target, bool_constant(true)),
                global,
            ))
        )

        (
            (let (region, global) = global.fresh_region())
            ---- ("false")
            (codegen_expr_into(global, scope, target, ExprData::False) => (
                region.assign(target, bool_constant(false)),
                global,
            ))
        )

        (
            (let typed_dest = resolve_place(&global, &scope, place)?)
            (let dest = typed_place_to_minirust(&global, &scope, &typed_dest)?)
            (let rhs_ty = infer_expr_ty(&global, &scope, rhs)?)
            (let (rhs_temp, global) = global.alloc_temp(&rhs_ty)?)
            (codegen_expr_into(global, scope, rhs_temp, rhs) => (region, global))
            ---- ("assign")
            (codegen_expr_into(global, scope, target, ExprData::Assign { place, expr: rhs }) => (
                region
                    .assign(dest, load(rhs_temp))
                    .assign(target, unit_value()),
                global,
            ))
        )

        (
            (let (_, global) = global.ensure_fn(MonoKey { id: id.clone(), args: args.to_vec() }))
            (let (region, global) = global.fresh_region())
            ---- ("turbofish")
            (codegen_expr_into(global, scope, target, ExprData::Turbofish { id, args }) => (
                region.assign(target, unit_value()),
                global,
            ))
        )

        (
            (resolve_call(global, scope, callee) => (fn_name, global))
            (let (temps, global) = alloc_temps_for_args(&global, &scope, args)?)
            (let (region, global) = global.fresh_region())
            (for_all(i in 0..args.len()) with(region, global)
                (codegen_expr_into(global, scope, temps[i].clone(), args[i].clone()) => (arg_region, global))
                (let region = region.append_from(&global, arg_region)))
            (let (region, global) = region.call(&global, fn_name, &temps, target.clone())?)
            ---- ("call")
            (codegen_expr_into(global, scope, target, ExprData::Call { callee, args }) => (region, global))
        )

        (
            (let typed = resolve_place(&global, &scope, place)?)
            (let mr_place = typed_place_to_minirust(&global, &scope, &typed)?)
            (let ref_value = addr_of(&global, mr_place, kind, &typed.ty)?)
            (let (region, global) = global.fresh_region())
            ---- ("ref")
            (codegen_expr_into(global, scope, target, ExprData::Ref { kind, lt: _, place }) => (
                region.assign(target, ref_value),
                global,
            ))
        )

        (
            (let fields = resolve_struct_fields(&global, adt_id, turbofish)?)
            (let (temps, global) = alloc_temps_for_fields(&global, &fields)?)
            (let (region, global) = global.fresh_region())
            (for_all(i in 0..fields.len()) with(region, global)
                (let field_expr = find_field_expr(field_exprs, &fields[i])?)
                (codegen_expr_into(global, scope, temps[i].clone(), field_expr) => (field_region, global))
                (let region = region.append_from(&global, field_region)))
            (let struct_value = tuple_value(&temps, adt_id, turbofish, &global.crates)?)
            ---- ("struct")
            (codegen_expr_into(global, scope, target, ExprData::Struct { field_exprs, adt_id, turbofish }) => (
                region.assign(target, struct_value),
                global,
            ))
        )
    }
}

judgment_fn! {
    fn codegen_stmt(
        global: CodegenGlobal,
        scope: CodegenScope,
        stmt: Stmt,
    ) => (SemeRegion, CodegenScope, CodegenGlobal) {
        debug(global, scope, stmt)

        (
            (let (local, global) = global.alloc_temp(ty)?)
            (let scope = scope.push_var(id.clone(), local.0, ty.clone())?)
            (codegen_expr_into(global, scope, local, init.expr.clone()) => (region, global))
            ---- ("let-init")
            (codegen_stmt(global, scope, Stmt::Let { label: _, id, ty, init: Some(init) }) => (region, scope, global))
        )

        (
            (let (local, global) = global.alloc_temp(ty)?)
            (let scope = scope.push_var(id.clone(), local.0, ty.clone())?)
            (let (region, global) = global.fresh_region())
            ---- ("let-no-init")
            (codegen_stmt(global, scope, Stmt::Let { label: _, id, ty, init: None }) => (region, scope, global))
        )

        (
            (codegen_expr_into(global, scope, scope.ret_local.clone(), expr) => (region, global))
            ---- ("return")
            (codegen_stmt(global, scope, Stmt::Return { expr }) => (
                region.terminated(terminator_return()),
                scope,
                global,
            ))
        )

        (
            (let expr_ty = infer_expr_ty(&global, &scope, expr)?)
            (let (temp, global) = global.alloc_temp(&expr_ty)?)
            (codegen_expr_into(global, scope, temp, expr) => (region, global))
            (let (region, global) = region.print_intrinsic(&global, temp)?)
            ---- ("print")
            (codegen_stmt(global, scope, Stmt::Print { expr }) => (region, scope, global))
        )

        (
            (let (ct, global) = global.alloc_local(bool_ty()))
            (codegen_expr_into(global, scope, ct, condition) => (cond_region, global))
            (codegen_block(global, scope, then_block) => (then_region, global))
            (codegen_block(global, scope, else_block) => (else_region, global))
            (let (region, global) = cond_region.branch_on_bool_from(&global, ct, then_region, else_region))
            ---- ("if")
            (codegen_stmt(global, scope, Stmt::If { condition, then_block, else_block }) => (region, scope, global))
        )

        (
            (let expr_ty = infer_expr_ty(&global, &scope, expr)?)
            (let (temp, global) = global.alloc_temp(&expr_ty)?)
            (codegen_expr_into(global, scope, temp, expr) => (region, global))
            ---- ("expr")
            (codegen_stmt(global, scope, Stmt::Expr { expr }) => (region, scope, global))
        )

        (
            (let label = require_label(label)?)
            (let (loop_start, global) = global.fresh_bb())
            (let (exit_block, global) = global.fresh_bb())
            (let scope = scope.push_label(label.id.clone(), Some(loop_start), exit_block))
            (codegen_block(global, scope, body) => (body_region, global))
            (let (region, global) = build_loop(&global, loop_start, exit_block, body_region)?)
            ---- ("loop")
            (codegen_stmt(global, scope, Stmt::Loop { label, body }) => (region, scope, global))
        )

        (
            (let (_, exit) = scope.lookup_label(label)?)
            (let (region, global) = global.fresh_region())
            ---- ("break")
            (codegen_stmt(global, scope, Stmt::Break { label }) => (
                region.terminated(terminator_goto(exit)),
                scope,
                global,
            ))
        )

        (
            (let (continue_target, _) = scope.lookup_label(label)?)
            (let start = continue_target.ok_or_else(|| anyhow::anyhow!("cannot continue on block label `{label:?}`"))?)
            (let (region, global) = global.fresh_region())
            ---- ("continue")
            (codegen_stmt(global, scope, Stmt::Continue { label }) => (
                region.terminated(terminator_goto(start)),
                scope,
                global,
            ))
        )

        (
            (codegen_block(global, scope, block) => (region, global))
            ---- ("block")
            (codegen_stmt(global, scope, Stmt::Block(block)) => (region, scope, global))
        )

        (
            (let block = instantiate_erased(binder)?)
            (codegen_block(global, scope, block) => (region, global))
            ---- ("exists")
            (codegen_stmt(global, scope, Stmt::Exists { binder }) => (region, scope, global))
        )
    }
}

judgment_fn! {
    fn codegen_block(
        global: CodegenGlobal,
        scope: CodegenScope,
        block: Block,
    ) => (SemeRegion, CodegenGlobal) {
        debug(global, scope, block)

        (
            (if block.label.is_none())
            (let (region, global) = global.fresh_region())
            (for_all(i in 0..block.stmts.len()) with(region, scope, global)
                (codegen_stmt(global, scope, block.stmts[i].clone()) => (stmt_region, scope, global))
                (let region = region.append_from(&global, stmt_region)))
            ---- ("block")
            (codegen_block(global, scope, block) => (region, global))
        )

        (
            (if let Some(label) = &block.label)
            (let (exit_block, global) = global.fresh_bb())
            (let scope = scope.push_label(label.id.clone(), None::<lang::BbName>, exit_block))
            (let (region, global) = global.fresh_region())
            (for_all(i in 0..block.stmts.len()) with(region, scope, global)
                (codegen_stmt(global, scope, block.stmts[i].clone()) => (stmt_region, scope, global))
                (let region = region.append_from(&global, stmt_region)))
            (let region = region.terminated(terminator_goto(exit_block)).add_empty_block(*exit_block))
            ---- ("labeled-block")
            (codegen_block(global, scope, block) => (region, global))
        )
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

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
    let (mut region, g2) = unwrap_proven(codegen_block(g, scope, body.clone()))?;
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
