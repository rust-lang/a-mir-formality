use std::{collections::HashSet, ops::Deref};

use crate::grammar::{
    Lt, LtData, Parameter, ParameterKind, Parameters, RefKind, RigidName, RigidTy, ScalarId, Ty,
    TyData,
};
use formality_core::variable::CoreVariable;
use itertools::Itertools;

type Stack<T> = Vec<T>;

/// Keeps track
#[derive(Debug, Default)]
pub struct NameContext {
    ty_names: Stack<String>,
    lt_names: Stack<String>,
    const_names: Stack<String>,
    used: HashSet<String>,
}

impl NameContext {
    /// Pushes `n` fresh type names to the context.
    pub fn push_tys(&mut self, n: usize) {
        push(self, n, ParameterKind::Ty);
    }

    /// Removes the `n` latest type names from the context.
    /// Panics if `n` is bigger than the numbers of names in the context.
    pub fn pop_tys(&mut self, n: usize) {
        pop(self, n, ParameterKind::Ty);
    }

    /// Pushes `n` fresh life time names to the context.
    pub fn push_lts(&mut self, n: usize) {
        push(self, n, ParameterKind::Lt);
    }

    /// Removes the `n` latest life time names from the context.
    /// Panics if `n` is bigger than the numbers of names in the context.
    pub fn pop_lts(&mut self, n: usize) {
        pop(self, n, ParameterKind::Lt);
    }

    /// Pushes `n` fresh const names to the context.
    pub fn push_const(&mut self, n: usize) {
        push(self, n, ParameterKind::Const);
    }

    /// Removes the `n` latest const names from the context.
    /// Panics if `n` is bigger than the numbers of names in the context.
    pub fn pop_const(&mut self, n: usize) {
        pop(self, n, ParameterKind::Const);
    }

    pub fn pretty_print_type(&mut self, ty: &Ty) -> String {
        match ty.data() {
            TyData::RigidTy(rigid_ty) => self.pretty_print_rigid_ty(rigid_ty),
            TyData::AliasTy(_alias_ty) => todo!(),
            TyData::PredicateTy(_predicate_ty) => todo!(),
            TyData::Variable(core_variable) => self.variable_name(core_variable),
        }
    }

    /// Returns a pretty printable name for the `given CoreVaribale`.
    pub fn variable_name(&self, variable: &CoreVariable<crate::FormalityLang>) -> String {
        let CoreVariable::BoundVar(core_bound_var) = variable else {
            unimplemented!()
        };

        let index = core_bound_var.debruijn.unwrap().index;
        match core_bound_var.kind {
            ParameterKind::Ty => self.ty_names.get(index).unwrap().clone(),
            ParameterKind::Lt => self.lt_names.get(index).unwrap().clone(),
            ParameterKind::Const => self.const_names.get(index).unwrap().clone(),
        }
    }

    fn fresh_name(&mut self, kind: ParameterKind) -> String {
        let prefix = match kind {
            ParameterKind::Ty => "T",
            ParameterKind::Lt => "'a",
            ParameterKind::Const => "N",
        };

        let mut name = String::new();
        for i in 1.. {
            name = format!("{prefix}{i}");
            if self.used.insert(name.clone()) {
                break;
            }
        }
        name
    }

    fn pretty_print_rigid_ty(&mut self, rigid_ty: &RigidTy) -> String {
        match &rigid_ty.name {
            RigidName::AdtId(adt_id) => adt_id.deref().into(),
            RigidName::ScalarId(scalar_id) => self.pretty_print_scalar(scalar_id),
            RigidName::Ref(ref_kind) => self.pretty_print_ref(ref_kind, &rigid_ty.parameters),
            RigidName::Tuple(size) => self.pretty_print_tuple(*size, &rigid_ty.parameters),
            RigidName::FnPtr(size) => self.pretty_print_fn_ptr(*size, &rigid_ty.parameters),
            RigidName::FnDef(fn_id) => todo!("Implement pretty printing FnDef: {fn_id:?}"),
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

    fn pretty_print_ref(&mut self, ref_kind: &RefKind, parameters: &Parameters) -> String {
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
            .expect("the first parameter of a reference must be a life time");
        let ty = parameters
            .get(1)
            .and_then(|p| match p {
                Parameter::Ty(ty) => Some(ty),
                _ => None,
            })
            .expect("The second parameter of a reference muse be a type");

        let lt = self.pretty_print_lt(lt);
        let ty = self.pretty_print_type(ty);

        format!("&{lt} {kind}{ty}")
    }

    fn pretty_print_lt(&mut self, lt: &Lt) -> String {
        match lt.data() {
            LtData::Static => "'static".into(),
            LtData::Variable(core_variable) => self.variable_name(core_variable),
        }
    }

    fn pretty_print_tuple(&mut self, size: usize, parameters: &Parameters) -> String {
        assert_eq!(size, parameters.len());

        let types = parameters
            .iter()
            .filter_map(|p| match p {
                Parameter::Ty(ty) => Some(self.pretty_print_type(ty)),
                _ => None,
            })
            .join(", ");

        format!("({types})")
    }

    fn pretty_print_fn_ptr(&mut self, size: usize, parameters: &Parameters) -> String {
        assert_eq!(size, parameters.len());

        let input_args = parameters
            .iter()
            .take(size - 1)
            .filter_map(|p| match p {
                Parameter::Ty(ty) => Some(self.pretty_print_type(ty)),
                _ => None,
            })
            .join(", ");

        let output_arg = parameters
            .iter()
            .rev()
            .next()
            .map(|p| match p {
                Parameter::Ty(ty) => Some(self.pretty_print_type(ty)),
                _ => None,
            })
            .flatten()
            .expect("Must be a type");

        format!("fn({input_args}) -> {output_arg}")
    }
}

fn push(ctx: &mut NameContext, n: usize, kind: ParameterKind) {
    let insert = match kind {
        ParameterKind::Ty => |ctx: &mut NameContext, name: String| ctx.ty_names.insert(0, name),
        ParameterKind::Lt => |ctx: &mut NameContext, name: String| ctx.lt_names.insert(0, name),
        ParameterKind::Const => {
            |ctx: &mut NameContext, name: String| ctx.const_names.insert(0, name)
        }
    };

    for _ in 0..n {
        let name = ctx.fresh_name(kind);
        insert(ctx, name);
    }
}

fn pop(ctx: &mut NameContext, n: usize, kind: ParameterKind) {
    let remove = match kind {
        ParameterKind::Ty => |ctx: &mut NameContext| ctx.ty_names.remove(0),
        ParameterKind::Lt => |ctx: &mut NameContext| ctx.lt_names.remove(0),
        ParameterKind::Const => |ctx: &mut NameContext| ctx.const_names.remove(0),
    };
    for _ in 0..n {
        let name = remove(ctx);
        ctx.used.remove(&name);
    }
}

#[cfg(test)]
mod test {
    use formality_core::variable::{CoreBoundVar, DebruijnIndex, VarIndex};

    use crate::grammar::AdtId;

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

        ctx.push_tys(1);
        let ty1 = create_ty();

        assert_eq!("T1", ctx.variable_name(&ty1));

        {
            ctx.push_tys(1);
            let ty1 = ty1.shift_in();
            let ty2 = create_ty();
            assert_eq!("T1", ctx.variable_name(&ty1));
            assert_eq!("T2", ctx.variable_name(&ty2));
            ctx.pop_tys(1);
        }

        assert_eq!("T1", ctx.variable_name(&ty1));
    }

    #[test]
    fn pretty_print_life_time_variables() {
        let mut ctx = NameContext::default();

        ctx.push_lts(1);
        let lt1 = create_lt();

        assert_eq!("'a1", ctx.variable_name(&lt1));

        {
            ctx.push_lts(1);
            let lt1 = lt1.shift_in();
            let lt2 = create_lt();
            assert_eq!("'a1", ctx.variable_name(&lt1));
            assert_eq!("'a2", ctx.variable_name(&lt2));
            ctx.pop_lts(1);
        }

        assert_eq!("'a1", ctx.variable_name(&lt1));
    }

    #[test]
    fn pretty_print_const_variables() {
        let mut ctx = NameContext::default();

        ctx.push_const(1);
        let const1 = create_const();

        assert_eq!("N1", ctx.variable_name(&const1));

        {
            ctx.push_const(1);
            let const1 = const1.shift_in();
            let const2 = create_const();
            assert_eq!("N1", ctx.variable_name(&const1));
            assert_eq!("N2", ctx.variable_name(&const2));
            ctx.pop_const(1);
        }

        assert_eq!("N1", ctx.variable_name(&const1));
    }

    #[test]
    fn pretty_print_adt() {
        let mut ctx = NameContext::default();
        let ty = Ty::new(TyData::RigidTy(RigidTy {
            name: RigidName::AdtId(AdtId::new("Foo")),
            parameters: Vec::new(),
        }));
        let t = ctx.pretty_print_type(&ty);

        assert_eq!("Foo", t);
    }

    #[test]
    fn pretty_print_scalar() {
        let mut ctx = NameContext::default();
        let ty = Ty::new(TyData::RigidTy(RigidTy {
            name: RigidName::ScalarId(ScalarId::U8),
            parameters: Vec::new(),
        }));
        let t = ctx.pretty_print_type(&ty);

        assert_eq!("u8", t);
    }

    #[test]
    fn pretty_print_ref() {
        let mut ctx = NameContext::default();
        ctx.push_lts(1);

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
        let t = ctx.pretty_print_type(&ty);
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
        let t = ctx.pretty_print_type(&ty);
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
        let t = ctx.pretty_print_type(&ty);
        assert_eq!("&'static mut u8", t);
    }

    #[test]
    fn pretty_print_tuple() {
        let mut ctx = NameContext::default();
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
        let t = ctx.pretty_print_type(&ty);

        assert_eq!("(u8, i64)", t);
    }

    #[test]
    fn pretty_print_fn_ptr() {
        let mut ctx = NameContext::default();
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
        let t = ctx.pretty_print_type(&ty);

        assert_eq!("fn(u8, i64) -> isize", t);
    }
}
