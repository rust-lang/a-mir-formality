use crate::{
    grammar::{
        AdtItem, Binder, BoundVar, Crate, CrateItem, Crates, ExistentialVar, Fallible, FeatureGate,
        FeatureGateName, ParameterKind, Ty, UniversalVar, VarIndex, Variable, WhereClause,
        WhereClauseData,
    },
    rust::Fold,
};
use std::{collections::HashMap, ops::Deref};

mod expr;
mod fns;
mod structs_enums_and_adts;
pub mod syntax;
pub mod test_util;
mod traits_and_impls;
mod tys;

/// Produces a Cargo workspaces on the file system in the
/// `root_direcotry`. After sucesfully creating all the files,
/// `cargo check` is run.
pub fn check_workspace(
    crates: &Crates,
    root_direcotry: &std::path::Path,
) -> Fallible<std::process::Output> {
    let root_toml = create_workspace(crates, root_direcotry)?;

    let output = std::process::Command::new("cargo")
        .env("RUSTFLAGS", "-A warnings")
        .args(["check", "--workspace", "--manifest-path", &root_toml])
        .output()?;
    Ok(output)
}

/// Produces a Cargo workspaces on the file system in the
/// `root_direcotry`. After sucesfully creating all the files,
/// `cargo fmt` is run.
pub fn create_workspace(crates: &Crates, root_directory: &std::path::Path) -> Fallible<String> {
    use std::io::Write;

    let crates = RustBuilder::default().build_crates(crates)?;
    let crates_path = root_directory.join("crates");
    let root_toml_path = root_directory.join("Cargo.toml");

    std::fs::create_dir_all(&crates_path)?;

    let root_toml_file = std::fs::File::create(&root_toml_path)?;
    writeln!(&root_toml_file, "[workspace]")?;
    writeln!(&root_toml_file, "resolver = \"2\"")?;
    writeln!(&root_toml_file, "members = [")?;

    for (name, source) in crates {
        let crate_path = crates_path.join(&name);
        let crate_toml_path = crate_path.join("Cargo.toml");
        let src_path = crate_path.join("src");
        let lib_path = src_path.join("lib.rs");
        std::fs::create_dir_all(&src_path)?;

        let mut file = std::fs::File::create(&lib_path)?;
        file.write_all(source.as_bytes())?;

        let file = std::fs::File::create(&crate_toml_path)?;
        writeln!(&file, "[package]")?;
        writeln!(&file, "name = \"{name}\"")?;
        writeln!(&file, "version = \"0.1.0\"")?;
        writeln!(&file, "edition = \"2021\"")?;

        writeln!(&root_toml_file, "    \"crates/{name}\",")?;
    }

    writeln!(&root_toml_file, "]")?;
    let location = root_toml_path
        .to_str()
        .ok_or_else(|| anyhow::anyhow!("Could not convert location to a &str"))?;

    std::process::Command::new("cargo")
        .args(["fmt", "--all", "--manifest-path", location])
        .spawn()?;

    Ok(location.to_string())
}

#[derive(Debug)]
pub struct RustBuilder {
    counter: VarIndex,
}

impl Default for RustBuilder {
    fn default() -> Self {
        Self {
            counter: VarIndex { index: 0 },
        }
    }
}

impl RustBuilder {
    pub fn with_binder<T: Fold, R>(
        &mut self,
        b: &Binder<T>,
        is_trait: bool,
        g: impl Fn(&T) -> &Vec<WhereClause>,
        mut op: impl FnMut(T, syntax::Generics, &mut RustBuilder) -> Fallible<R>,
    ) -> Fallible<R> {
        let subst = self.bounded_substitution(b);
        let term = b.instantiate_with(&subst).expect("suitable substitution");

        let names = subst
            .into_iter()
            .map(|var| format!("{}{}", self.kind_to_string(&var.kind), var.var_index.index))
            .collect::<Vec<_>>();
        let generics =
            self.lower_generics_for_binder(b.kinds(), g(&term), names.as_slice(), is_trait)?;

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

    pub fn substitution<T, V>(
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

    pub fn existential_substitution<T>(&mut self, b: &Binder<T>) -> Vec<ExistentialVar>
    where
        T: Fold,
    {
        self.substitution(b, |kind, var_index| ExistentialVar { kind, var_index })
    }

    pub fn universal_substitution<T>(&mut self, b: &Binder<T>) -> Vec<UniversalVar>
    where
        T: Fold,
    {
        self.substitution(b, |kind, var_index| UniversalVar { kind, var_index })
    }

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

    pub fn build_crates(&mut self, crates: &Crates) -> Fallible<HashMap<String, String>> {
        crates
            .crates
            .iter()
            .map(|krate| {
                let lowered = self.lower_crate(krate)?;
                Ok((krate.id.deref().clone(), lowered.to_string()))
            })
            .collect()
    }

    fn lower_crate(&mut self, krate: &Crate) -> Fallible<syntax::RustCrate> {
        let mut attrs = Vec::new();
        let mut items = Vec::new();

        for item in &krate.items {
            match item {
                CrateItem::FeatureGate(gate) => attrs.push(self.lower_feature_gate(gate)?),
                CrateItem::AdtItem(AdtItem::Struct(strukt)) => {
                    items.push(syntax::Item::Struct(self.lower_struct(strukt)?));
                }
                CrateItem::AdtItem(AdtItem::Enum(e)) => {
                    items.push(syntax::Item::Enum(self.lower_enum(e)?));
                }
                CrateItem::Trait(t) => {
                    items.push(syntax::Item::Trait(self.lower_trait(t)?));
                }
                CrateItem::TraitImpl(trait_impl) => {
                    items.push(syntax::Item::Impl(self.lower_trait_impl(trait_impl)?));
                }
                CrateItem::NegTraitImpl(neg_trait_impl) => {
                    items.push(syntax::Item::NegImpl(
                        self.lower_neg_trait_impl(neg_trait_impl)?,
                    ));
                }
                CrateItem::Fn(function) => {
                    items.push(syntax::Item::Function(self.lower_fn(function)?));
                }
                CrateItem::Test(_) => {
                    todo!("lowering `test` crate items is not implemented yet")
                }
            }
        }

        Ok(syntax::RustCrate { attrs, items })
    }

    fn lower_feature_gate(&mut self, gate: &FeatureGate) -> Fallible<syntax::Attr> {
        let name = match gate.name {
            FeatureGateName::PoloniusAlpha => "polonius_alpha",
        };
        Ok(syntax::Attr::Feature(name.to_owned()))
    }

    /// Lowers generic parameters and `where` clauses for a binder.
    ///
    /// Set `skip_first` when lowering generics for a trait
    /// declaration.  In trait declarations, the first parameter
    /// represents `Self` and must not be treated as a regular generic
    /// parameter.
    pub fn lower_generics_for_binder(
        &mut self,
        binder_kinds: &[ParameterKind],
        where_clauses: &[WhereClause],
        names: &[String],
        skip_first: bool,
    ) -> Fallible<syntax::Generics> {
        if names.len() != binder_kinds.len() {
            anyhow::bail!(
                "binder metadata mismatch: {} names but {} kinds",
                names.len(),
                binder_kinds.len()
            );
        }

        let start = usize::from(skip_first);
        let mut params: Vec<syntax::GenericParam> = names
            .iter()
            .skip(start)
            .cloned()
            .zip(binder_kinds.iter().copied().skip(start))
            .map(|(name, kind)| Self::generic_param_from_kind(name, kind))
            .collect();

        for wc in where_clauses {
            match wc.data() {
                WhereClauseData::IsImplemented(ty, _, _) => {
                    if let Ty::Variable(var) = ty {
                        let name = self.core_variable_to_string(&var)?;
                        // If the `where` clause referes to a type
                        // variable from an outer binder, that
                        // variable is not yet included in `params`.
                        Self::push_type_param_if_missing(&mut params, name);
                    }
                }
                WhereClauseData::TypeOfConst(konst, ty) => {
                    let name = match self.lower_const(konst)? {
                        syntax::ConstExpr::Ident(id) => id,
                        other => anyhow::bail!(
                            "const generic parameter must be an identifier, found `{other}`"
                        ),
                    };
                    let ty = self.lower_ty(ty)?;

                    if let Some(existing_ty) = params.iter_mut().find_map(|param| match param {
                        syntax::GenericParam::Const { name: existing, ty } if existing == &name => {
                            Some(ty)
                        }
                        _ => None,
                    }) {
                        *existing_ty = ty;
                    } else {
                        // If the `where` clause referes to a type
                        // variable from an outer binder, that
                        // variable is not yet included in `params`.
                        params.push(syntax::GenericParam::Const { name, ty });
                    }
                }
                WhereClauseData::AliasEq(_, _) => {
                    todo!("lowering `AliasEq` where-clauses is not implemented yet")
                }
                WhereClauseData::Outlives(_, _) => {
                    todo!("lowering `Outlives` where-clauses is not implemented yet")
                }
                WhereClauseData::ForAll(_) => {
                    todo!("lowering `ForAll` where-clauses is not implemented yet")
                }
            }
        }

        let where_clauses = self.lower_where_clauses(where_clauses)?;

        Ok(syntax::Generics {
            params,
            where_clauses,
        })
    }

    pub fn lower_where_clauses(
        &mut self,
        where_clauses: &[WhereClause],
    ) -> Fallible<Vec<syntax::WhereClause>> {
        let mut preds = Vec::new();

        for wc in where_clauses {
            match wc.data() {
                WhereClauseData::IsImplemented(ty, trait_id, params) => {
                    preds.push(syntax::WhereClause::Trait {
                        ty: self.lower_ty(ty)?,
                        trait_name: trait_id.deref().clone(),
                        args: params
                            .iter()
                            .map(|param| self.lower_generic_arg(param))
                            .collect::<Result<Vec<_>, _>>()?,
                    });
                }
                WhereClauseData::TypeOfConst(_, _) => {}
                WhereClauseData::AliasEq(_, _) => {
                    anyhow::bail!("lowering `AliasEq` where-clauses is not implemented yet")
                }
                WhereClauseData::Outlives(_, _) => {
                    anyhow::bail!("lowering `Outlives` where-clauses is not implemented yet")
                }
                WhereClauseData::ForAll(_) => {
                    anyhow::bail!("lowering `ForAll` where-clauses is not implemented yet")
                }
            }
        }

        Ok(preds)
    }

    fn push_type_param_if_missing(params: &mut Vec<syntax::GenericParam>, name: String) {
        if name == "Self" {
            return;
        }

        if !params
            .iter()
            .any(|param| matches!(param, syntax::GenericParam::Type(existing) if existing == &name))
        {
            params.push(syntax::GenericParam::Type(name));
        }
    }

    /// Creates a generic parameter nameed `name` with the given
    /// `kind`.
    ///
    /// For const generics, a default type of `usize` is used.  This
    /// default should not be relied upon, as it is an implementation
    /// detail and my change in the future.
    fn generic_param_from_kind(name: String, kind: ParameterKind) -> syntax::GenericParam {
        match kind {
            ParameterKind::Ty => syntax::GenericParam::Type(name),
            ParameterKind::Lt => syntax::GenericParam::Lifetime(name),
            ParameterKind::Const => syntax::GenericParam::Const {
                name,
                ty: syntax::Type::Path {
                    name: "usize".to_owned(),
                    args: Vec::new(),
                },
            },
        }
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn empty_crate() {
        crate::assert_rust!(
            [
                crate Foo {}
            ],
            ""
        );
    }

    #[test]
    fn simple_feature_gate() {
        crate::assert_rust!(
            [
                crate Foo {
                    #![feature(polonius_alpha)]
                }
            ],
            "#![feature(polonius_alpha)]"
        );
    }
}
