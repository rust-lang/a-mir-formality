use std::ops::Deref;

use crate::grammar::{
    AliasTy, Const, ConstData, Fallible, Lt, LtData, Parameter, Parameters, PtrKind, RefKind,
    RigidName, RigidTy, ScalarId, ScalarValue, Ty, Variable,
};

use super::{context::Context, syntax};

pub fn lower_generic_arg(ctx: &mut Context, parameter: &Parameter) -> Fallible<syntax::GenericArg> {
    match parameter {
        Parameter::Ty(ty) => Ok(syntax::GenericArg::Type(lower_ty(ctx, ty)?)),
        Parameter::Lt(lt) => Ok(syntax::GenericArg::Lifetime(lower_lt(ctx, lt)?)),
        Parameter::Const(konst) => Ok(syntax::GenericArg::Const(lower_const(ctx, konst)?)),
    }
}

pub fn lower_const(ctx: &mut Context, konst: &Const) -> Fallible<syntax::ConstExpr> {
    match konst {
        ConstData::RigidValue(_) => {
            todo!("lowering rigid const values is not implemented yet")
        }
        ConstData::Scalar(value) => {
            let (value, suffix) = match value {
                ScalarValue::U8(v) => (v.to_string(), "u8"),
                ScalarValue::U16(v) => (v.to_string(), "u16"),
                ScalarValue::U32(v) => (v.to_string(), "u32"),
                ScalarValue::U64(v) => (v.to_string(), "u64"),
                ScalarValue::I8(v) => (v.to_string(), "i8"),
                ScalarValue::I16(v) => (v.to_string(), "i16"),
                ScalarValue::I32(v) => (v.to_string(), "i32"),
                ScalarValue::I64(v) => (v.to_string(), "i64"),
                ScalarValue::Usize(v) => (v.to_string(), "usize"),
                ScalarValue::Isize(v) => (v.to_string(), "isize"),
                ScalarValue::Bool(v) => {
                    return Ok(syntax::ConstExpr::Ident(v.to_string()));
                }
            };

            Ok(syntax::ConstExpr::Scalar {
                value,
                suffix: suffix.to_owned(),
            })
        }
        ConstData::Block(_) => {
            todo!("lowering const block expressions is not implemented yet")
        }
        ConstData::Variable(core_variable) => Ok(syntax::ConstExpr::Ident(
            ctx.core_variable_to_string(&core_variable)?,
        )),
    }
}

pub fn lower_ty(ctx: &mut Context, ty: &Ty) -> Fallible<syntax::Type> {
    match ty {
        Ty::RigidTy(rigid_ty) => lower_rigid_ty(ctx, rigid_ty),
        Ty::AliasTy(alias) => lower_alias_ty(ctx, alias),
        Ty::PredicateTy(_) => {
            todo!("lowering predicate types is not implemented yet")
        }
        Ty::Variable(core_variable) => Ok(syntax::Type::Path {
            name: ctx.core_variable_to_string(core_variable)?,
            args: Vec::new(),
        }),
    }
}

pub fn lower_rigid_ty(ctx: &mut Context, rigid_ty: &RigidTy) -> Fallible<syntax::Type> {
    match &rigid_ty.name {
        RigidName::AdtId(adt_id) => {
            let args = rigid_ty
                .parameters
                .iter()
                .map(|arg| lower_generic_arg(ctx, arg))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(syntax::Type::Path {
                name: adt_id.deref().clone(),
                args,
            })
        }
        RigidName::ScalarId(scalar_id) => Ok(syntax::Type::Path {
            name: scalar_to_string(scalar_id),
            args: Vec::new(),
        }),
        RigidName::Ref(ref_kind) => lower_ref(ctx, ref_kind, &rigid_ty.parameters),
        RigidName::Raw(ptr_kind) => lower_ptr(ctx, ptr_kind, &rigid_ty.parameters),
        RigidName::Tuple(size) => lower_tuple(ctx, *size, &rigid_ty.parameters),
        RigidName::FnPtr(size) => lower_fn_ptr(ctx, *size, &rigid_ty.parameters),
        RigidName::FnDef(fn_id) => {
            todo!("lowering fn def type `{:?}` is not implemented yet", fn_id)
        }
        RigidName::Never => Ok(syntax::Type::Never),
    }
}

pub fn lower_alias_ty(ctx: &mut Context, alias_ty: &AliasTy) -> Fallible<syntax::Type> {
    let ty = alias_ty
        .parameters
        .get(0)
        .and_then(|p| match p {
            Parameter::Ty(ty) => lower_ty(ctx, ty).ok(),
            Parameter::Lt(_lt) => todo!(),
            Parameter::Const(_) => todo!(),
        })
        .ok_or_else(|| anyhow::anyhow!("alias type is missing type argument"))?;

    Ok(match &alias_ty.name {
        crate::grammar::AliasName::AssociatedTyId(associated_ty_name) => syntax::Type::Alias {
            ty: Box::new(ty),
            trait_name: associated_ty_name.trait_id.deref().clone(),
            assoc_name: associated_ty_name.item_id.deref().clone(),
        },
    })
}

pub fn scalar_to_string(scalar_id: &ScalarId) -> String {
    match scalar_id {
        ScalarId::U8 => "u8",
        ScalarId::U16 => "u16",
        ScalarId::U32 => "u32",
        ScalarId::U64 => "u64",
        ScalarId::I8 => "i8",
        ScalarId::I16 => "i16",
        ScalarId::I32 => "i32",
        ScalarId::I64 => "i64",
        ScalarId::Bool => "bool",
        ScalarId::Usize => "usize",
        ScalarId::Isize => "isize",
    }
    .to_owned()
}

pub fn lower_ref(
    ctx: &mut Context,
    ref_kind: &RefKind,
    parameters: &Parameters,
) -> Fallible<syntax::Type> {
    let lifetime = parameters
        .first()
        .and_then(|p| match p {
            Parameter::Lt(lt) => Some(lt.deref().clone()),
            _ => None,
        })
        .ok_or_else(|| anyhow::anyhow!("reference type is missing lifetime argument"))?;

    let pointee = parameters
        .get(1)
        .and_then(|p| match p {
            Parameter::Ty(ty) => Some(ty),
            _ => None,
        })
        .ok_or_else(|| anyhow::anyhow!("reference type is missing pointee type argument"))?;

    let lifetime = match &lifetime {
        Lt::Variable(Variable::ExistentialVar(_)) => None,
        _ => Some(lower_lt(ctx, &lifetime)?),
    };
    let pointee = lower_ty(ctx, pointee)?;

    Ok(syntax::Type::Ref {
        lifetime,
        mutable: matches!(ref_kind, RefKind::Mut),
        ty: Box::new(pointee),
    })
}

pub fn lower_ptr(
    ctx: &mut Context,
    ptr_kind: &PtrKind,
    parameters: &Parameters,
) -> Fallible<syntax::Type> {
    let pointee = parameters
        .first()
        .and_then(|p| match p {
            Parameter::Ty(ty) => Some(ty),
            _ => None,
        })
        .ok_or_else(|| anyhow::anyhow!("raw pointer type is missing pointee type argument"))?;

    Ok(syntax::Type::RawPtr {
        mutable: matches!(ptr_kind, PtrKind::Mut),
        ty: Box::new(lower_ty(ctx, pointee)?),
    })
}

pub fn lower_lt(ctx: &mut Context, lt: &Lt) -> Fallible<String> {
    match lt {
        LtData::Static => Ok("'static".to_owned()),
        LtData::Variable(core_variable) => ctx.core_variable_to_string(&core_variable),
    }
}

pub fn lower_tuple(
    ctx: &mut Context,
    size: usize,
    parameters: &Parameters,
) -> Fallible<syntax::Type> {
    if size != parameters.len() {
        anyhow::bail!(
            "tuple arity mismatch: expected {size} arguments but found {}",
            parameters.len()
        );
    }

    let types = parameters
        .iter()
        .map(|p| match p {
            Parameter::Ty(ty) => lower_ty(ctx, ty),
            _ => anyhow::bail!("tuple parameters must all be types"),
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(syntax::Type::Tuple(types))
}

pub fn lower_fn_ptr(
    ctx: &mut Context,
    size: usize,
    parameters: &Parameters,
) -> Fallible<syntax::Type> {
    if size != parameters.len() {
        anyhow::bail!(
            "function pointer arity mismatch: expected {size} arguments but found {}",
            parameters.len()
        );
    }

    if parameters.is_empty() {
        anyhow::bail!("function pointer must include a return type")
    }

    let mut types = parameters
        .iter()
        .map(|p| match p {
            Parameter::Ty(ty) => lower_ty(ctx, ty),
            _ => anyhow::bail!("function pointer parameters must all be types"),
        })
        .collect::<Result<Vec<_>, _>>()?;

    let output = types
        .pop()
        .ok_or_else(|| anyhow::anyhow!("function pointer must include a return type"))?;

    Ok(syntax::Type::FnPtr {
        inputs: types,
        output: Box::new(output),
    })
}

#[cfg(test)]
mod test {
    use crate::grammar::{AdtId, BoundVar, ExistentialVar, VarIndex, Variable};

    use super::*;

    fn create_ty(index: usize) -> Variable {
        Variable::BoundVar(BoundVar {
            debruijn: None,
            var_index: VarIndex { index },
            kind: crate::grammar::ParameterKind::Ty,
        })
    }

    fn create_lt(index: usize) -> Variable {
        Variable::BoundVar(BoundVar {
            debruijn: None,
            var_index: VarIndex { index },
            kind: crate::grammar::ParameterKind::Lt,
        })
    }

    fn create_const(index: usize) -> Variable {
        Variable::BoundVar(BoundVar {
            debruijn: None,
            var_index: VarIndex { index },
            kind: crate::grammar::ParameterKind::Const,
        })
    }

    #[test]
    fn pretty_print_type_variables() {
        let b = Context::default();
        let ty1 = create_ty(1);
        assert_eq!("T1", b.core_variable_to_string(&ty1).unwrap());
    }

    #[test]
    fn pretty_print_life_time_variables() {
        let b = Context::default();
        let lt1 = create_lt(1);
        assert_eq!("'a1", b.core_variable_to_string(&lt1).unwrap());
    }

    #[test]
    fn pretty_print_const_variables() {
        let b = Context::default();
        let const1 = create_const(1);
        assert_eq!("N1", b.core_variable_to_string(&const1).unwrap());
    }

    #[test]
    fn pretty_print_adt() {
        let mut ctx = Context::default();
        let ty = Ty::RigidTy(RigidTy {
            name: RigidName::AdtId(AdtId::new("Foo")),
            parameters: Vec::new(),
        });
        let t = lower_ty(&mut ctx, &ty).unwrap().to_string();

        assert_eq!("Foo", t);
    }

    #[test]
    fn pretty_print_scalar() {
        let mut ctx = Context::default();
        let ty = Ty::RigidTy(RigidTy {
            name: RigidName::ScalarId(ScalarId::U8),
            parameters: Vec::new(),
        });
        let t = lower_ty(&mut ctx, &ty).unwrap().to_string();

        assert_eq!("u8", t);
    }

    #[test]
    fn pretty_print_shared_ref() {
        let mut ctx = Context::default();
        let ty = Ty::RigidTy(RigidTy {
            name: RigidName::Ref(RefKind::Shared),
            parameters: vec![
                Parameter::Lt(Lt::Variable(create_lt(1)).into()),
                Parameter::Ty(
                    Ty::RigidTy(RigidTy {
                        name: RigidName::ScalarId(ScalarId::U8),
                        parameters: Vec::new(),
                    })
                    .into(),
                ),
            ],
        });
        let t = lower_ty(&mut ctx, &ty).unwrap().to_string();
        assert_eq!("&'a1 u8", t);
    }

    #[test]
    fn pretty_print_mutable_ref() {
        let mut ctx = Context::default();
        let ty = Ty::RigidTy(RigidTy {
            name: RigidName::Ref(RefKind::Mut),
            parameters: vec![
                Parameter::Lt(Lt::Variable(create_lt(1)).into()),
                Parameter::Ty(
                    Ty::RigidTy(RigidTy {
                        name: RigidName::ScalarId(ScalarId::U8),
                        parameters: Vec::new(),
                    })
                    .into(),
                ),
            ],
        });
        let t = lower_ty(&mut ctx, &ty).unwrap().to_string();
        assert_eq!("&'a1 mut u8", t);
    }

    #[test]
    fn pretty_print_mutable_static_ref() {
        let mut ctx = Context::default();
        let ty = Ty::RigidTy(RigidTy {
            name: RigidName::Ref(RefKind::Mut),
            parameters: vec![
                Parameter::Lt(Lt::Static.into()),
                Parameter::Ty(
                    Ty::RigidTy(RigidTy {
                        name: RigidName::ScalarId(ScalarId::U8),
                        parameters: Vec::new(),
                    })
                    .into(),
                ),
            ],
        });
        let t = lower_ty(&mut ctx, &ty).unwrap().to_string();
        assert_eq!("&'static mut u8", t);
    }

    #[test]
    fn pretty_print_existential_ref() {
        let mut ctx = Context::default();
        let ty = Ty::RigidTy(RigidTy {
            name: RigidName::Ref(RefKind::Mut),
            parameters: vec![
                Parameter::Lt(
                    Lt::Variable(Variable::ExistentialVar(ExistentialVar {
                        kind: crate::grammar::ParameterKind::Lt,
                        var_index: VarIndex { index: 0 },
                    }))
                    .into(),
                ),
                Parameter::Ty(
                    Ty::RigidTy(RigidTy {
                        name: RigidName::ScalarId(ScalarId::U8),
                        parameters: Vec::new(),
                    })
                    .into(),
                ),
            ],
        });
        let t = lower_ty(&mut ctx, &ty).unwrap().to_string();
        assert_eq!("&mut u8", t);
    }

    #[test]
    fn pretty_print_raw_ptr() {
        let mut ctx = Context::default();
        let ty = Ty::RigidTy(RigidTy {
            name: RigidName::Raw(PtrKind::Const),
            parameters: vec![Parameter::Ty(
                Ty::RigidTy(RigidTy {
                    name: RigidName::ScalarId(ScalarId::U8),
                    parameters: Vec::new(),
                })
                .into(),
            )],
        });
        let t = lower_ty(&mut ctx, &ty).unwrap().to_string();
        assert_eq!("*const u8", t);

        let ty = Ty::RigidTy(RigidTy {
            name: RigidName::Raw(PtrKind::Mut),
            parameters: vec![Parameter::Ty(
                Ty::RigidTy(RigidTy {
                    name: RigidName::ScalarId(ScalarId::U8),
                    parameters: Vec::new(),
                })
                .into(),
            )],
        });
        let t = lower_ty(&mut ctx, &ty).unwrap().to_string();
        assert_eq!("*mut u8", t);
    }

    #[test]
    fn pretty_print_tuple() {
        let mut ctx = Context::default();
        let ty = Ty::RigidTy(RigidTy {
            name: RigidName::Tuple(2),
            parameters: vec![
                Parameter::Ty(
                    Ty::RigidTy(RigidTy {
                        name: RigidName::ScalarId(ScalarId::U8),
                        parameters: vec![],
                    })
                    .into(),
                ),
                Parameter::Ty(
                    Ty::RigidTy(RigidTy {
                        name: RigidName::ScalarId(ScalarId::I64),
                        parameters: vec![],
                    })
                    .into(),
                ),
            ],
        });
        let t = lower_ty(&mut ctx, &ty).unwrap().to_string();

        assert_eq!("(u8, i64)", t);
    }

    #[test]
    fn pretty_print_fn_ptr() {
        let mut ctx = Context::default();
        let ty = Ty::RigidTy(RigidTy {
            name: RigidName::FnPtr(3),
            parameters: vec![
                Parameter::Ty(
                    Ty::RigidTy(RigidTy {
                        name: RigidName::ScalarId(ScalarId::U8),
                        parameters: vec![],
                    })
                    .into(),
                ),
                Parameter::Ty(
                    Ty::RigidTy(RigidTy {
                        name: RigidName::ScalarId(ScalarId::I64),
                        parameters: vec![],
                    })
                    .into(),
                ),
                Parameter::Ty(
                    Ty::RigidTy(RigidTy {
                        name: RigidName::ScalarId(ScalarId::Isize),
                        parameters: vec![],
                    })
                    .into(),
                ),
            ],
        });
        let t = lower_ty(&mut ctx, &ty).unwrap().to_string();

        assert_eq!("fn(u8, i64) -> isize", t);
    }

    #[test]
    fn alias_ty() {
        crate::assert_rust!(
            [
                crate Blub {
                    trait Foo {
                        type Assoc: [];
                    }
                    trait Bar<U> {}
                    impl<T, U> Bar<U> for T
                    where
                        <T as Foo>::Assoc: Bar<U> {}
                }
            ],
            r#"
trait Foo {
    type Assoc;
}

trait Bar<T2> { }

impl<T3, T4> Bar<T4> for T3 where <T3 as Foo>::Assoc: Bar<T4> {}
"#
        );
    }
}
