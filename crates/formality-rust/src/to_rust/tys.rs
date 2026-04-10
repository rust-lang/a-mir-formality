use std::ops::Deref;

use crate::grammar::{
    Const, ConstData, Fallible, Lt, LtData, Parameter, Parameters, RefKind, RigidName, RigidTy,
    ScalarId, Ty, TyData,
};
use crate::to_rust::RustBuilder;

impl RustBuilder {
    pub fn const_to_string(&mut self, konst: &Const) -> Fallible<String> {
        match konst.data() {
            ConstData::RigidValue(_rigid_const_data) => todo!(),
            ConstData::Scalar(_scalar_value) => todo!(),
            ConstData::Block(_block) => todo!(),
            ConstData::Variable(core_variable) => self.variable_name(core_variable),
        }
    }

    pub fn ty_to_string(&mut self, ty: &Ty) -> Fallible<String> {
        match ty.data() {
            TyData::RigidTy(rigid_ty) => self.pretty_print_rigid_ty(rigid_ty),
            TyData::AliasTy(_alias_ty) => todo!(),
            TyData::PredicateTy(_predicate_ty) => todo!(),
            TyData::Variable(core_variable) => self.variable_name(core_variable),
        }
    }

    fn pretty_print_rigid_ty(&mut self, rigid_ty: &RigidTy) -> Fallible<String> {
        match &rigid_ty.name {
            RigidName::AdtId(adt_id) => Ok(adt_id.deref().into()),
            RigidName::ScalarId(scalar_id) => Ok(self.pretty_print_scalar(scalar_id)),
            RigidName::Ref(ref_kind) => self.pretty_print_ref(ref_kind, &rigid_ty.parameters),
            RigidName::Tuple(size) => self.pretty_print_tuple(*size, &rigid_ty.parameters),
            RigidName::FnPtr(size) => self.pretty_print_fn_ptr(*size, &rigid_ty.parameters),
            RigidName::FnDef(fn_id) => todo!("Implement pretty printing FnDef: {fn_id:?}"),
            RigidName::Never => Ok("!".into()),
        }
    }

    fn pretty_print_scalar(&self, scalar_id: &ScalarId) -> String {
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
        .into()
    }

    fn pretty_print_ref(
        &mut self,
        ref_kind: &RefKind,
        parameters: &Parameters,
    ) -> Fallible<String> {
        let kind = match ref_kind {
            RefKind::Shared => "",
            RefKind::Mut => "mut ",
        };

        let lt = parameters
            .get(0)
            .and_then(|p| match p {
                Parameter::Lt(lt) => Some(lt),
                _ => None,
            })
            .ok_or_else(|| {
                anyhow::anyhow!("the first parameter of a reference must be a life time")
            })?;
        let ty = parameters
            .get(1)
            .and_then(|p| match p {
                Parameter::Ty(ty) => Some(ty),
                _ => None,
            })
            .ok_or_else(|| anyhow::anyhow!("The second parameter of a reference muse be a type"))?;

        let lt = self.pretty_print_lt(lt)?;
        let ty = self.ty_to_string(ty)?;

        Ok(format!("&{lt} {kind}{ty}"))
    }

    fn pretty_print_lt(&mut self, lt: &Lt) -> Fallible<String> {
        match lt.data() {
            LtData::Static => Ok("'static".into()),
            LtData::Variable(core_variable) => self.variable_name(core_variable),
        }
    }

    fn pretty_print_tuple(&mut self, size: usize, parameters: &Parameters) -> Fallible<String> {
        assert_eq!(size, parameters.len());

        let types = parameters
            .iter()
            .filter_map(|p| match p {
                Parameter::Ty(ty) => Some(self.ty_to_string(ty)),
                _ => None,
            })
            .collect::<Result<Vec<_>, _>>()?
            .join(", ");

        Ok(format!("({types})"))
    }

    fn pretty_print_fn_ptr(&mut self, size: usize, parameters: &Parameters) -> Fallible<String> {
        assert_eq!(size, parameters.len());

        let input_args = parameters
            .iter()
            .take(size - 1)
            .filter_map(|p| match p {
                Parameter::Ty(ty) => Some(self.ty_to_string(ty)),
                _ => None,
            })
            .collect::<Result<Vec<_>, _>>()?
            .join(", ");

        let output_arg = parameters
            .iter()
            .rev()
            .next()
            .map(|p| match p {
                Parameter::Ty(ty) => self.ty_to_string(ty),
                _ => unimplemented!(),
            })
            .ok_or_else(|| anyhow::anyhow!("Return type is missing"))??;

        Ok(format!("fn({input_args}) -> {output_arg}"))
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

        assert_eq!("T1", ctx.variable_name(&ty1).unwrap());

        {
            ctx.push(&[ParameterKind::Ty]);
            let ty1 = ty1.shift_in();
            let ty2 = create_ty();
            assert_eq!("T1", ctx.variable_name(&ty1).unwrap());
            assert_eq!("T2", ctx.variable_name(&ty2).unwrap());
            ctx.pop();
        }

        assert_eq!("T1", ctx.variable_name(&ty1).unwrap());
    }

    #[test]
    fn pretty_print_life_time_variables() {
        let mut ctx = NameContext::default();

        ctx.push(&[ParameterKind::Lt]);
        let lt1 = create_lt();

        assert_eq!("'a1", ctx.variable_name(&lt1).unwrap());

        {
            ctx.push(&[ParameterKind::Lt]);
            let lt1 = lt1.shift_in();
            let lt2 = create_lt();
            assert_eq!("'a1", ctx.variable_name(&lt1).unwrap());
            assert_eq!("'a2", ctx.variable_name(&lt2).unwrap());
            ctx.pop();
        }

        assert_eq!("'a1", ctx.variable_name(&lt1).unwrap());
    }

    #[test]
    fn pretty_print_const_variables() {
        let mut ctx = NameContext::default();

        ctx.push(&[ParameterKind::Const]);
        let const1 = create_const();

        assert_eq!("N1", ctx.variable_name(&const1).unwrap());

        {
            ctx.push(&[ParameterKind::Const]);
            let const1 = const1.shift_in();
            let const2 = create_const();
            assert_eq!("N1", ctx.variable_name(&const1).unwrap());
            assert_eq!("N2", ctx.variable_name(&const2).unwrap());
            ctx.pop();
        }

        assert_eq!("N1", ctx.variable_name(&const1).unwrap());
    }

    #[test]
    fn pretty_print_adt() {
        let mut pp = RustBuilder::default();
        let ty = Ty::new(TyData::RigidTy(RigidTy {
            name: RigidName::AdtId(AdtId::new("Foo")),
            parameters: Vec::new(),
        }));
        let t = pp.ty_to_string(&ty);

        assert_eq!("Foo", t.unwrap());
    }

    #[test]
    fn pretty_print_scalar() {
        let mut pp = RustBuilder::default();
        let ty = Ty::new(TyData::RigidTy(RigidTy {
            name: RigidName::ScalarId(ScalarId::U8),
            parameters: Vec::new(),
        }));
        let t = pp.ty_to_string(&ty);

        assert_eq!("u8", t.unwrap());
    }

    #[test]
    fn pretty_print_ref() {
        let mut pp = RustBuilder::default();
        pp.ctx.push(&[ParameterKind::Lt]);

        // &'a u8
        let ty = Ty::new(TyData::RigidTy(RigidTy {
            name: RigidName::Ref(RefKind::Shared),
            parameters: vec![
                Parameter::Lt(Lt::new(create_lt())),
                Parameter::Ty(Ty::new(TyData::RigidTy(RigidTy {
                    name: RigidName::ScalarId(ScalarId::U8),
                    parameters: Vec::new(),
                }))),
            ],
        }));
        let t = pp.ty_to_string(&ty).unwrap();
        assert_eq!("&'a1 u8", t);

        let ty = Ty::new(TyData::RigidTy(RigidTy {
            name: RigidName::Ref(RefKind::Mut),
            parameters: vec![
                Parameter::Lt(Lt::new(create_lt())),
                Parameter::Ty(Ty::new(TyData::RigidTy(RigidTy {
                    name: RigidName::ScalarId(ScalarId::U8),
                    parameters: Vec::new(),
                }))),
            ],
        }));
        let t = pp.ty_to_string(&ty).unwrap();
        assert_eq!("&'a1 mut u8", t);

        let ty = Ty::new(TyData::RigidTy(RigidTy {
            name: RigidName::Ref(RefKind::Mut),
            parameters: vec![
                Parameter::Lt(Lt::new(LtData::Static)),
                Parameter::Ty(Ty::new(TyData::RigidTy(RigidTy {
                    name: RigidName::ScalarId(ScalarId::U8),
                    parameters: Vec::new(),
                }))),
            ],
        }));
        let t = pp.ty_to_string(&ty).unwrap();
        assert_eq!("&'static mut u8", t);
    }

    #[test]
    fn pretty_print_tuple() {
        let mut pp = RustBuilder::default();
        let ty = Ty::new(TyData::RigidTy(RigidTy {
            name: RigidName::Tuple(2),
            parameters: vec![
                Parameter::Ty(Ty::new(TyData::RigidTy(RigidTy {
                    name: RigidName::ScalarId(ScalarId::U8),
                    parameters: vec![],
                }))),
                Parameter::Ty(Ty::new(TyData::RigidTy(RigidTy {
                    name: RigidName::ScalarId(ScalarId::I64),
                    parameters: vec![],
                }))),
            ],
        }));
        let t = pp.ty_to_string(&ty).unwrap();

        assert_eq!("(u8, i64)", t);
    }

    #[test]
    fn pretty_print_fn_ptr() {
        let mut pp = RustBuilder::default();
        let ty = Ty::new(TyData::RigidTy(RigidTy {
            name: RigidName::FnPtr(3),
            parameters: vec![
                Parameter::Ty(Ty::new(TyData::RigidTy(RigidTy {
                    name: RigidName::ScalarId(ScalarId::U8),
                    parameters: vec![],
                }))),
                Parameter::Ty(Ty::new(TyData::RigidTy(RigidTy {
                    name: RigidName::ScalarId(ScalarId::I64),
                    parameters: vec![],
                }))),
                Parameter::Ty(Ty::new(TyData::RigidTy(RigidTy {
                    name: RigidName::ScalarId(ScalarId::Isize),
                    parameters: vec![],
                }))),
            ],
        }));
        let t = pp.ty_to_string(&ty).unwrap();

        assert_eq!("fn(u8, i64) -> isize", t);
    }
}
