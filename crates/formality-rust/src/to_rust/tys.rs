use std::ops::Deref;

use crate::grammar::{
    Const, ConstData, Fallible, Lt, LtData, Parameter, Parameters, PtrKind, RefKind, RigidName,
    RigidTy, ScalarId, ScalarValue, Ty, TyData,
};

use super::{syntax, RustBuilder};

impl RustBuilder {
    pub fn lower_generic_arg(&mut self, parameter: &Parameter) -> Fallible<syntax::GenericArg> {
        match parameter {
            Parameter::Ty(ty) => Ok(syntax::GenericArg::Type(self.lower_ty(ty)?)),
            Parameter::Lt(lt) => Ok(syntax::GenericArg::Lifetime(self.lower_lt(lt)?)),
            Parameter::Const(konst) => Ok(syntax::GenericArg::Const(self.lower_const(konst)?)),
        }
    }

    pub fn lower_const(&mut self, konst: &Const) -> Fallible<syntax::ConstExpr> {
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
                self.core_variable_to_string(&core_variable)?,
            )),
        }
    }

    pub fn lower_ty(&mut self, ty: &Ty) -> Fallible<syntax::Type> {
        match ty {
            TyData::RigidTy(rigid_ty) => self.lower_rigid_ty(&rigid_ty),
            TyData::AliasTy(_) => todo!("lowering alias types is not implemented yet"),
            TyData::PredicateTy(_) => {
                todo!("lowering predicate types is not implemented yet")
            }
            TyData::Variable(core_variable) => Ok(syntax::Type::Path {
                name: self.core_variable_to_string(&core_variable)?,
                args: Vec::new(),
            }),
        }
    }

    pub fn lower_rigid_ty(&mut self, rigid_ty: &RigidTy) -> Fallible<syntax::Type> {
        match &rigid_ty.name {
            RigidName::AdtId(adt_id) => {
                let args = rigid_ty
                    .parameters
                    .iter()
                    .map(|arg| self.lower_generic_arg(arg))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(syntax::Type::Path {
                    name: adt_id.deref().clone(),
                    args,
                })
            }
            RigidName::ScalarId(scalar_id) => Ok(syntax::Type::Path {
                name: self.scalar_to_string(scalar_id),
                args: Vec::new(),
            }),
            RigidName::Ref(ref_kind) => self.lower_ref(ref_kind, &rigid_ty.parameters),
            RigidName::Raw(ptr_kind) => self.lower_ptr(ptr_kind, &rigid_ty.parameters),
            RigidName::Tuple(size) => self.lower_tuple(*size, &rigid_ty.parameters),
            RigidName::FnPtr(size) => self.lower_fn_ptr(*size, &rigid_ty.parameters),
            RigidName::FnDef(fn_id) => {
                todo!("lowering fn def type `{:?}` is not implemented yet", fn_id)
            }
            RigidName::Never => Ok(syntax::Type::Never),
        }
    }

    pub fn scalar_to_string(&self, scalar_id: &ScalarId) -> String {
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

    fn lower_ref(&mut self, ref_kind: &RefKind, parameters: &Parameters) -> Fallible<syntax::Type> {
        let lifetime = parameters
            .first()
            .and_then(|p| match p {
                Parameter::Lt(lt) => Some(lt),
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

        let lifetime = self.lower_lt(lifetime)?;
        let pointee = self.lower_ty(pointee)?;

        Ok(syntax::Type::Ref {
            lifetime: Some(lifetime),
            mutable: matches!(ref_kind, RefKind::Mut),
            ty: Box::new(pointee),
        })
    }

    fn lower_ptr(&mut self, ptr_kind: &PtrKind, parameters: &Parameters) -> Fallible<syntax::Type> {
        let pointee = parameters
            .first()
            .and_then(|p| match p {
                Parameter::Ty(ty) => Some(ty),
                _ => None,
            })
            .ok_or_else(|| anyhow::anyhow!("raw pointer type is missing pointee type argument"))?;

        Ok(syntax::Type::RawPtr {
            mutable: matches!(ptr_kind, PtrKind::Mut),
            ty: Box::new(self.lower_ty(pointee)?),
        })
    }

    pub fn lower_lt(&mut self, lt: &Lt) -> Fallible<String> {
        match lt {
            LtData::Static => Ok("'static".to_owned()),
            LtData::Variable(core_variable) => self.core_variable_to_string(&core_variable),
        }
    }

    fn lower_tuple(&mut self, size: usize, parameters: &Parameters) -> Fallible<syntax::Type> {
        if size != parameters.len() {
            anyhow::bail!(
                "tuple arity mismatch: expected {size} arguments but found {}",
                parameters.len()
            );
        }

        let types = parameters
            .iter()
            .map(|p| match p {
                Parameter::Ty(ty) => self.lower_ty(ty),
                _ => anyhow::bail!("tuple parameters must all be types"),
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(syntax::Type::Tuple(types))
    }

    fn lower_fn_ptr(&mut self, size: usize, parameters: &Parameters) -> Fallible<syntax::Type> {
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
                Parameter::Ty(ty) => self.lower_ty(ty),
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
}

#[cfg(test)]
mod test {
    use formality_core::variable::{CoreBoundVar, CoreVariable, DebruijnIndex, VarIndex};

    use crate::{
        grammar::{AdtId, ParameterKind},
        to_rust::NameContext,
    };

    use super::*;

    fn create_ty() -> CoreVariable<crate::FormalityLang> {
        CoreVariable::BoundVar(CoreBoundVar {
            debruijn: Some(DebruijnIndex { index: 0 }),
            var_index: VarIndex { index: 0 },
            kind: crate::grammar::ParameterKind::Ty,
        })
    }

    fn create_lt() -> CoreVariable<crate::FormalityLang> {
        CoreVariable::BoundVar(CoreBoundVar {
            debruijn: Some(DebruijnIndex { index: 0 }),
            var_index: VarIndex { index: 0 },
            kind: crate::grammar::ParameterKind::Lt,
        })
    }

    fn create_const() -> CoreVariable<crate::FormalityLang> {
        CoreVariable::BoundVar(CoreBoundVar {
            debruijn: Some(DebruijnIndex { index: 0 }),
            var_index: VarIndex { index: 0 },
            kind: crate::grammar::ParameterKind::Const,
        })
    }

    #[test]
    fn pretty_print_type_variables() {
        let mut ctx = NameContext::default();

        ctx.push(&[ParameterKind::Ty]);
        let ty1 = create_ty();

        assert_eq!("T1", ctx.core_variable_to_string(&ty1).unwrap());

        {
            ctx.push(&[ParameterKind::Ty]);
            let ty1 = ty1.shift_in();
            let ty2 = create_ty();
            assert_eq!("T1", ctx.core_variable_to_string(&ty1).unwrap());
            assert_eq!("T2", ctx.core_variable_to_string(&ty2).unwrap());
            ctx.pop();
        }

        assert_eq!("T1", ctx.core_variable_to_string(&ty1).unwrap());
    }

    #[test]
    fn pretty_print_life_time_variables() {
        let mut ctx = NameContext::default();

        ctx.push(&[ParameterKind::Lt]);
        let lt1 = create_lt();

        assert_eq!("'a1", ctx.core_variable_to_string(&lt1).unwrap());

        {
            ctx.push(&[ParameterKind::Lt]);
            let lt1 = lt1.shift_in();
            let lt2 = create_lt();
            assert_eq!("'a1", ctx.core_variable_to_string(&lt1).unwrap());
            assert_eq!("'a2", ctx.core_variable_to_string(&lt2).unwrap());
            ctx.pop();
        }

        assert_eq!("'a1", ctx.core_variable_to_string(&lt1).unwrap());
    }

    #[test]
    fn pretty_print_const_variables() {
        let mut ctx = NameContext::default();

        ctx.push(&[ParameterKind::Const]);
        let const1 = create_const();

        assert_eq!("N1", ctx.core_variable_to_string(&const1).unwrap());

        {
            ctx.push(&[ParameterKind::Const]);
            let const1 = const1.shift_in();
            let const2 = create_const();
            assert_eq!("N1", ctx.core_variable_to_string(&const1).unwrap());
            assert_eq!("N2", ctx.core_variable_to_string(&const2).unwrap());
            ctx.pop();
        }

        assert_eq!("N1", ctx.core_variable_to_string(&const1).unwrap());
    }

    #[test]
    fn pretty_print_adt() {
        let mut pp = RustBuilder::default();
        let ty = Ty::RigidTy(RigidTy {
            name: RigidName::AdtId(AdtId::new("Foo")),
            parameters: Vec::new(),
        });
        let t = pp.lower_ty(&ty).unwrap().to_string();

        assert_eq!("Foo", t);
    }

    #[test]
    fn pretty_print_scalar() {
        let mut pp = RustBuilder::default();
        let ty = Ty::RigidTy(RigidTy {
            name: RigidName::ScalarId(ScalarId::U8),
            parameters: Vec::new(),
        });
        let t = pp.lower_ty(&ty).unwrap().to_string();

        assert_eq!("u8", t);
    }

    #[test]
    fn pretty_print_ref() {
        let mut pp = RustBuilder::default();
        pp.ctx.push(&[ParameterKind::Lt]);

        let ty = Ty::RigidTy(RigidTy {
            name: RigidName::Ref(RefKind::Shared),
            parameters: vec![
                Parameter::Lt(Lt::Variable(create_lt()).into()),
                Parameter::Ty(
                    Ty::RigidTy(RigidTy {
                        name: RigidName::ScalarId(ScalarId::U8),
                        parameters: Vec::new(),
                    })
                    .into(),
                ),
            ],
        });
        let t = pp.lower_ty(&ty).unwrap().to_string();
        assert_eq!("&'a1 u8", t);

        let ty = Ty::RigidTy(RigidTy {
            name: RigidName::Ref(RefKind::Mut),
            parameters: vec![
                Parameter::Lt(Lt::Variable(create_lt()).into()),
                Parameter::Ty(
                    Ty::RigidTy(RigidTy {
                        name: RigidName::ScalarId(ScalarId::U8),
                        parameters: Vec::new(),
                    })
                    .into(),
                ),
            ],
        });
        let t = pp.lower_ty(&ty).unwrap().to_string();
        assert_eq!("&'a1 mut u8", t);

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
        let t = pp.lower_ty(&ty).unwrap().to_string();
        assert_eq!("&'static mut u8", t);
    }

    #[test]
    fn pretty_print_raw_ptr() {
        let mut pp = RustBuilder::default();
        pp.ctx.push(&[ParameterKind::Lt]);

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
        let t = pp.lower_ty(&ty).unwrap().to_string();
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
        let t = pp.lower_ty(&ty).unwrap().to_string();
        assert_eq!("*mut u8", t);
    }

    #[test]
    fn pretty_print_tuple() {
        let mut pp = RustBuilder::default();
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
        let t = pp.lower_ty(&ty).unwrap().to_string();

        assert_eq!("(u8, i64)", t);
    }

    #[test]
    fn pretty_print_fn_ptr() {
        let mut pp = RustBuilder::default();
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
        let t = pp.lower_ty(&ty).unwrap().to_string();

        assert_eq!("fn(u8, i64) -> isize", t);
    }
}
