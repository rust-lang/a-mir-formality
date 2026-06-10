use std::collections::HashMap;

use crate::{
    grammar::{
        Binder, DebruijnIndex, ExistentialVar, Fallible, ParameterKind, TraitBinder, UniversalVar,
        VarIndex, Variable,
    },
    rust::{Fold, Term},
};

#[derive(Debug, Clone)]
pub struct Context {
    counter: VarIndex,
    bounded: Vec<Vec<String>>,
    free: HashMap<VarIndex, String>,
    nesting_level: usize,
}

impl Context {
    pub fn empty() -> Context {
        Context::default()
    }
}

impl Default for Context {
    fn default() -> Self {
        Self {
            counter: VarIndex { index: 0 },
            bounded: Vec::new(),
            free: HashMap::new(),
            nesting_level: 0,
        }
    }
}

impl Context {
    pub fn first(&self) -> Option<&Vec<String>> {
        self.bounded.first()
    }

    /// Introduces a fresh set of bound variable names for the binder `b` and
    /// returns the contained term wrapped in a [`Wrapped`] guard.
    ///
    /// For each binder parameter, a fresh name is generated and pushed onto the
    /// context. The returned [`Wrapped`] value provides access to the term while
    /// these names are in scope. When the `Wrapped` value is dropped, the names
    /// are removed from the contex.
    pub fn open_bounded<T: Term>(&mut self, b: Binder<T>) -> Wrapped<'_, T> {
        let term = b.peek();
        let names = b
            .kinds()
            .into_iter()
            .enumerate()
            .map(|(index, kind)| {
                format!(
                    "{}{}{}",
                    self.kind_to_string(kind),
                    self.nesting_level,
                    index
                )
            })
            .collect::<Vec<_>>();
        Wrapped::bound(self, term.clone(), names)
    }

    /// Introduces a fresh set of bound variable names for the binder `b` and
    /// returns the contained term wrapped in a [`Wrapped`] guard.
    ///
    /// Fresh names are generated as in [`open_bounded`], but the first bound
    /// variable is always renamed to `Self` to reflect Rust’s implicit `Self`
    /// parameter in trait declarations.
    pub fn open_trait<T: Term>(&mut self, b: TraitBinder<T>) -> Wrapped<'_, T> {
        let wrapped = self.open_bounded(b.explicit_binder);
        wrapped.ctx.bounded[0][0] = "Self".to_string();
        wrapped
    }

    /// Opens the binder `b` and instatiates ith with a fresh set of
    /// existential variables and returns the term.
    pub fn open_exists<T: Term>(&mut self, b: Binder<T>) -> Wrapped<'_, T> {
        // TODO: Replace Existential Lifetimes with an Erased after codegen (#369) is merged
        let subst = self.existential_substitution(&b);
        let term = b.instantiate_with(&subst).expect("suitable substitution");
        let names: Vec<_> = subst
            .into_iter()
            .map(|var| {
                (
                    var.var_index,
                    format!("{}E{}", self.kind_to_string(&var.kind), var.var_index.index),
                )
            })
            .collect();
        Wrapped::free(self, term.clone(), names)
    }

    /// Allocates a new set of fresh names and returns a wrapped `T`.
    pub fn open_universal<T: Term>(&mut self, b: Binder<T>) -> Wrapped<'_, T> {
        self.open_bounded(b)
    }

    pub fn core_variable_to_string(&self, variable: &Variable) -> Fallible<String> {
        let kind = self.kind_to_string(&variable.kind());
        match variable {
            Variable::BoundVar(bound_var) => {
                if let Some(index) = bound_var.debruijn {
                    self.lookup_bounded_var(&index, &bound_var.var_index)
                } else {
                    anyhow::bail!("binder was opened")
                }
            }
            Variable::UniversalVar(universal_var) => {
                Ok(format!("{kind}{}", universal_var.var_index.index))
            }
            Variable::ExistentialVar(existential_var) => {
                Ok(format!("{kind}{}", existential_var.var_index.index))
            }
        }
    }

    fn lookup_bounded_var(
        &self,
        debruijn: &DebruijnIndex,
        var_index: &VarIndex,
    ) -> Fallible<String> {
        Ok(self
            .bounded
            .get(debruijn.index)
            .and_then(|v| v.get(var_index.index))
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "variable with {}.{} is not bound",
                    debruijn.index,
                    var_index.index
                )
            })?
            .clone())
    }

    fn kind_to_string(&self, kind: &ParameterKind) -> &'static str {
        match kind {
            ParameterKind::Ty => "T",
            ParameterKind::Lt => "'a",
            ParameterKind::Const => "N",
        }
    }

    /// Creates a substitution that replaces the variables introduced by the
    /// binder `b` with fresh existential variables.
    ///
    /// Each binder parameter is mapped to a new [`ExistentialVar`] with a fresh
    /// variable index. The resulting substitution can be used to instantiate
    /// the binder without capturing existing variables.
    pub fn existential_substitution<T>(&mut self, b: &Binder<T>) -> Vec<ExistentialVar>
    where
        T: Fold,
    {
        self.substitution(b, |kind, var_index| ExistentialVar { kind, var_index })
    }

    /// Creates a substitution that replaces the variables introduced by the
    /// binder `b` with fresh universal variables.
    ///
    /// Each binder parameter is mapped to a new [`UniversalVar`] with a fresh
    /// variable index. The resulting substitution can be used to instantiate
    /// the binder without capturing existing variables.
    pub fn universal_substitution<T>(&mut self, b: &Binder<T>) -> Vec<UniversalVar>
    where
        T: Fold,
    {
        self.substitution(b, |kind, var_index| UniversalVar { kind, var_index })
    }

    /// Creates a substitution for the binder `b` using freshly generated
    /// variables.
    ///
    /// For each parameter introduced by the binder, a fresh variable is created
    /// by invoking the generator function `v`. The resulting substitution can be
    /// used to instantiate the binder.
    fn substitution<T, V>(
        &mut self,
        b: &Binder<T>,
        v: impl Fn(ParameterKind, VarIndex) -> V,
    ) -> Vec<V>
    where
        T: Fold,
    {
        let subst = self.fresh_substitution(b.kinds(), v);
        subst
    }

    /// Creates a fresh substitution for the given parameter kinds.
    ///
    /// For each entry in `kinds`, this function generates a fresh variable index
    /// and invokes the callback `v` with the corresponding `ParameterKind` and
    /// index. The collected results form the substitution in binder order.
    fn fresh_substitution<V>(
        &mut self,
        kinds: &[ParameterKind],
        v: impl Fn(ParameterKind, VarIndex) -> V,
    ) -> Vec<V> {
        let fresh_index = self.counter;
        self.counter = self.counter + kinds.len();
        kinds
            .iter()
            .zip(0..)
            .map(|(&kind, offset)| v(kind, fresh_index + offset))
            .collect()
    }
}

/// A RAII guard that provides access to a term `T` while a set of bound
/// variable names is active in the associated [`Context`].
///
/// When this guard is dropped, the bound variable names introduced for the
/// scope are removed from the context.
#[derive(Debug)]
pub struct Wrapped<'ctx, T> {
    pub ctx: &'ctx mut Context,
    pub term: T,
}

impl<'ctx, T> Wrapped<'ctx, T> {
    /// Adds `names` as bounded variables in `ctx` and returns [Wrapped].
    pub fn bound(ctx: &'ctx mut Context, term: T, names: Vec<String>) -> Wrapped<'ctx, T> {
        ctx.nesting_level += 1;
        ctx.bounded.insert(0, names);
        Wrapped { ctx, term }
    }

    /// Adds `names` as free variables in `ctx` and returns [Wrapped].
    pub fn free(
        ctx: &'ctx mut Context,
        term: T,
        names: Vec<(VarIndex, String)>,
    ) -> Wrapped<'ctx, T> {
        ctx.free.extend(names.clone().into_iter());
        Wrapped::bound(ctx, term, names.into_iter().map(|(_, name)| name).collect())
    }
}

impl<'ctx, T> Drop for Wrapped<'ctx, T> {
    fn drop(&mut self) {
        self.ctx.nesting_level -= 1;
        self.ctx.bounded.remove(0);
    }
}

#[macro_export]
macro_rules! open_bounded {
    ($ctx:expr, $binder:expr) => {{
        let mut wrapper = $ctx.open_bounded($binder);
        let ctx = &mut wrapper.ctx;
        let term = &wrapper.term;
        let generics = $crate::to_rust::lower_generics_for_binder(
            ctx,
            $binder.kinds(),
            &term.where_clauses,
            false,
        )?;
        (wrapper, generics)
    }};
}
#[macro_export]
macro_rules! open_trait {
    ($ctx:expr, $binder:expr) => {{
        let mut wrapper = $ctx.open_trait($binder);
        let ctx = &mut wrapper.ctx;
        let term = &wrapper.term;
        let generics = $crate::to_rust::lower_generics_for_binder(
            ctx,
            $binder.explicit_binder.kinds(),
            &term.where_clauses,
            true,
        )?;
        (wrapper, generics)
    }};
}
pub(crate) use open_bounded;
pub(crate) use open_trait;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::{BoundVar, DebruijnIndex, VarIndex, Variable};
    impl Context {
        pub fn with(bounded: Vec<Vec<String>>) -> Context {
            Context {
                counter: VarIndex { index: 0 },
                bounded,
                free: HashMap::new(),
                nesting_level: 0,
            }
        }
    }

    fn create_ty(index: usize) -> Variable {
        Variable::BoundVar(BoundVar {
            debruijn: Some(DebruijnIndex { index: 0 }),
            var_index: VarIndex { index },
            kind: crate::grammar::ParameterKind::Ty,
        })
    }

    fn create_lt(index: usize) -> Variable {
        Variable::BoundVar(BoundVar {
            debruijn: Some(DebruijnIndex { index: 0 }),
            var_index: VarIndex { index },
            kind: crate::grammar::ParameterKind::Lt,
        })
    }

    fn create_const(index: usize) -> Variable {
        Variable::BoundVar(BoundVar {
            debruijn: Some(DebruijnIndex { index: 0 }),
            var_index: VarIndex { index },
            kind: crate::grammar::ParameterKind::Const,
        })
    }

    #[test]
    fn pretty_print_type_variables() {
        let b = Context::with(vec![vec!["T0".into()]]);
        let ty1 = create_ty(0);
        assert_eq!("T0", b.core_variable_to_string(&ty1).unwrap());
    }

    #[test]
    fn pretty_print_life_time_variables() {
        let b = Context::with(vec![vec!["'a0".into()]]);
        let lt1 = create_lt(0);
        assert_eq!("'a0", b.core_variable_to_string(&lt1).unwrap());
    }

    #[test]
    fn pretty_print_const_variables() {
        let b = Context::with(vec![vec!["N0".into()]]);
        let const1 = create_const(0);
        assert_eq!("N0", b.core_variable_to_string(&const1).unwrap());
    }

    #[test]
    fn nesting_causes_no_shadowing() {
        crate::assert_rust!(
            [
                crate Blub {
                    trait Foo<T> {
                        fn blub<K, V>(k: K, v:V) -> T;
                        fn bar<K, V>(k: K, v:V) -> T;
                    }
                }
            ],
            expect_test::expect![[r#"
                pub trait Foo<T01> {
                    fn blub<T10, T11>(mut k: T10, mut v: T11) -> T01;
                    fn bar<T10, T11>(mut k: T10, mut v: T11) -> T01;
                }"#]]
        );
    }
}
