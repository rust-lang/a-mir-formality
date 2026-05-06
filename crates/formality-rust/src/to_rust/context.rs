use crate::{
    grammar::{
        Binder, BoundVar, ExistentialVar, Fallible, ParameterKind, UniversalVar, VarIndex,
        Variable, WhereClause,
    },
    rust::Fold,
    to_rust::{lower_generics_for_binder, syntax},
};

#[derive(Debug)]
pub struct Context {
    counter: VarIndex,
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
        }
    }
}

impl Context {
    /// Applies the operation `op` to the term contained within the binder `b`.
    ///
    /// The binder is instantiated with a fresh set of bound variables. Since a
    /// binder introduces new generic variables, those variables are lowered at
    /// this point and provided to the caller via the `syntax::Generics` argument
    /// passed to `op`.
    ///
    /// The function `g` is used to extract the relevant `&[WhereClause]` clauses from the
    /// instantiated term.
    ///
    /// When lowering generics for a trait declaration, `is_trait` must be set to
    /// `true`. In this case, the implicit first generic parameter represents
    /// `Self` and is therefore not treated as a regular generic parameter.
    ///
    /// # Example
    /// ```rust,no_run
    /// # use formality_rust::grammar::{AdtId, Binder, Struct, StructBoundData, Fallible};
    /// # use formality_rust::to_rust::{context::Context, syntax::StructItem};
    /// fn lower_struct(term: Struct) -> Fallible<StructItem> {
    ///     let mut ctx = Context::empty();
    ///     ctx.with_binder(
    ///         &term.binder,
    ///         true,
    ///         |term| &term.where_clauses,
    ///         |term, generics, ctx| todo!(),
    ///     )
    /// }
    /// ```
    pub fn with_binder<T: Fold, R>(
        &mut self,
        b: &Binder<T>,
        is_trait: bool,
        g: impl Fn(&T) -> &[WhereClause],
        mut op: impl FnMut(T, syntax::Generics, &mut Context) -> Fallible<R>,
    ) -> Fallible<R> {
        let subst = self.bounded_substitution(b);
        let term = b.instantiate_with(&subst).expect("suitable substitution");

        let names = subst
            .into_iter()
            .map(|var| format!("{}{}", self.kind_to_string(&var.kind), var.var_index.index))
            .collect::<Vec<_>>();
        let generics =
            lower_generics_for_binder(self, b.kinds(), g(&term), names.as_slice(), is_trait)?;

        let result = op(term, generics, self);
        result
    }

    pub fn core_variable_to_string(&self, variable: &Variable) -> Fallible<String> {
        let index = match variable {
            Variable::UniversalVar(universal_var) => universal_var.var_index,
            Variable::ExistentialVar(existential_var) => existential_var.var_index,
            Variable::BoundVar(bound_var) => {
                if bound_var.debruijn.is_some() {
                    anyhow::bail!("binder is not open")
                }
                bound_var.var_index
            }
        }
        .index;
        let kind = self.kind_to_string(&variable.kind());
        Ok(format!("{kind}{index}"))
    }

    fn kind_to_string(&self, kind: &ParameterKind) -> &'static str {
        match kind {
            ParameterKind::Ty => "T",
            ParameterKind::Lt => "'a",
            ParameterKind::Const => "N",
        }
    }

    /// Creates a substitution that replaces the variables introduced by the
    /// binder `b` with fresh bound variables.
    ///
    /// Each binder parameter is mapped to a new [`BoundVar`] with a fresh
    /// variable index. The resulting substitution can be used to instantiate
    /// the binder without capturing existing variables.
    pub fn bounded_substitution<T>(&mut self, b: &Binder<T>) -> Vec<BoundVar>
    where
        T: Fold,
    {
        self.substitution(b, |kind, var_index| BoundVar {
            debruijn: None,
            kind,
            var_index,
        })
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
