use std::collections::HashSet;

use crate::grammar::ParameterKind;
use formality_core::variable::CoreVariable;

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
    fn pretty_print_type() {
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
    fn pretty_print_life_times() {
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
    fn pretty_print_consts() {
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
}
