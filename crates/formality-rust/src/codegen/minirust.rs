//! Newtypes wrapping MiniRust types, plus value/terminator constructors
//! and type translation from formality-rust types to MiniRust types.

use crate::grammar::{Crates, Fallible, Parameter, RigidName, ScalarId, Ty, TyData};
use formality_core::Upcast;
use libspecr::hidden::GcCow;
use libspecr::list;
use libspecr::prelude::{Align, Int, List, Mutability, Signedness, Size};
use minirust_rs::lang;
use minirust_rs::mem::{PtrType, TupleHeadLayout};

use super::scope::CodegenFn;

// ===========================================================================
// Newtypes
// ===========================================================================

macro_rules! minirust_newtype {
    (copy $name:ident wraps $inner:ty) => {
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
        pub(crate) struct $name(pub(super) $inner);
        minirust_newtype!(@impls $name, $inner);
    };
    ($name:ident wraps $inner:ty) => {
        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
        pub(crate) struct $name(pub(super) $inner);
        minirust_newtype!(@impls $name, $inner);
    };
    (@impls $name:ident, $inner:ty) => {
        formality_core::cast_impl!($name);

        impl formality_core::UpcastFrom<$inner> for $name {
            fn upcast_from(v: $inner) -> Self {
                $name(v)
            }
        }

        impl formality_core::UpcastFrom<$name> for $inner {
            fn upcast_from(v: $name) -> Self {
                v.0
            }
        }

        impl From<$name> for $inner {
            fn from(v: $name) -> Self {
                v.0
            }
        }

        impl std::ops::Deref for $name {
            type Target = $inner;
            fn deref(&self) -> &$inner {
                &self.0
            }
        }
    };
}

minirust_newtype!(copy MiniRustLocal wraps lang::LocalName);
minirust_newtype!(copy MiniRustType wraps lang::Type);
minirust_newtype!(copy MiniRustFn wraps lang::FnName);
minirust_newtype!(copy MiniRustBb wraps lang::BbName);
minirust_newtype!(MiniRustFunction wraps lang::Function);
minirust_newtype!(copy MiniRustTerminator wraps lang::Terminator);

minirust_newtype!(copy MiniRustPlace wraps lang::PlaceExpr);
minirust_newtype!(copy MiniRustValue wraps lang::ValueExpr);

impl formality_core::UpcastFrom<MiniRustLocal> for MiniRustPlace {
    fn upcast_from(l: MiniRustLocal) -> Self {
        MiniRustPlace(lang::PlaceExpr::Local(l.into()))
    }
}

// ===========================================================================
// Value and terminator constructors
// ===========================================================================

pub(super) fn constant(value: &usize, ty: &ScalarId) -> MiniRustValue {
    let mr_ty = scalar_minirust_ty(ty).expect("scalar type always valid");
    MiniRustValue(lang::ValueExpr::Constant(
        lang::Constant::Int(Int::from(*value)),
        mr_ty,
    ))
}

pub(super) fn bool_constant(val: bool) -> MiniRustValue {
    MiniRustValue(lang::ValueExpr::Constant(
        lang::Constant::Bool(val),
        lang::Type::Bool,
    ))
}

pub(super) fn load(place: impl Upcast<MiniRustPlace>) -> MiniRustValue {
    let place: MiniRustPlace = place.upcast();
    MiniRustValue(lang::ValueExpr::Load {
        source: GcCow::new(place.into()),
    })
}

pub(super) fn addr_of(
    cfn: &CodegenFn,
    place: impl Upcast<MiniRustPlace>,
    kind: &crate::grammar::RefKind,
    pointee_ty: &Ty,
) -> Fallible<MiniRustValue> {
    let place: MiniRustPlace = place.upcast();
    let mr = cfn.minirust_ty(pointee_ty)?;
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
    Ok(MiniRustValue(lang::ValueExpr::AddrOf {
        target: GcCow::new(place.into()),
        ptr_ty,
    }))
}

pub(super) fn tuple_value(
    temps: &[MiniRustLocal],
    adt_id: &crate::grammar::AdtId,
    turbofish: &crate::grammar::expr::Turbofish,
    crates: &Crates,
) -> Fallible<MiniRustValue> {
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
    Ok(MiniRustValue(lang::ValueExpr::Tuple(fv, st)))
}

pub(super) fn terminator_return() -> lang::Terminator {
    lang::Terminator::Return
}

pub(super) fn terminator_goto(bb: impl Upcast<MiniRustBb>) -> MiniRustTerminator {
    let bb: MiniRustBb = bb.upcast();
    lang::Terminator::Goto(bb.into()).upcast()
}

pub(super) fn bool_ty() -> lang::Type {
    lang::Type::Bool
}

pub(super) fn unit_ty() -> lang::Type {
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

pub(super) fn unit_value() -> lang::ValueExpr {
    lang::ValueExpr::Tuple(list![], unit_ty())
}

// ===========================================================================
// Type translation
// ===========================================================================

pub(super) fn minirust_ty(crates: &Crates, ty: &Ty) -> Fallible<lang::Type> {
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

pub(super) fn type_size_align(ty: &lang::Type) -> (Size, Align) {
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

pub(super) fn struct_field_index(
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
