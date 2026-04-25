use crate::{
    grammar::{
        AdtItem, Binder, Crate, CrateItem, Crates, Fallible, FeatureGate, FeatureGateName,
        ParameterKind, TyData, Variable, WhereClause, WhereClauseData,
    },
    rust::Fold,
};
use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

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

type Stack<T> = Vec<T>;

#[derive(Debug, Default)]
pub struct NameContext {
    variable_names: Stack<Vec<String>>,
    used: HashSet<String>,
}

impl NameContext {
    pub fn core_variable_to_string(&self, variable: &Variable) -> Fallible<String> {
        match variable {
            Variable::BoundVar(core_bound_var) => {
                let var_index = core_bound_var.var_index.index;
                let binder_index = core_bound_var
                    .debruijn
                    .ok_or_else(|| anyhow::anyhow!("binder was opened"))?
                    .index;

                self.variable_names
                    .get(binder_index)
                    .and_then(|vars| vars.get(var_index))
                    .cloned()
                    .ok_or_else(|| anyhow::anyhow!("unbound variable {core_bound_var:?}"))
            }
            Variable::UniversalVar(v) => {
                Ok(self.free_variable_name(true, v.kind, v.var_index.index))
            }
            Variable::ExistentialVar(v) => {
                Ok(self.free_variable_name(false, v.kind, v.var_index.index))
            }
        }
    }

    pub fn current_names(&self) -> Fallible<&[String]> {
        self.variable_names
            .first()
            .map(|v| v.as_slice())
            .ok_or_else(|| anyhow::anyhow!("no active binder frame"))
    }

    pub fn pop(&mut self) {
        if let Some(names) = self.variable_names.first().cloned() {
            self.variable_names.remove(0);
            for name in names {
                self.used.remove(&name);
            }
        }
    }

    pub fn push(&mut self, kinds: &[ParameterKind]) {
        self.variable_names.insert(0, Vec::new());
        for kind in kinds {
            let name = self.fresh_name(kind);
            self.variable_names[0].push(name);
        }
    }

    pub fn push_anonymous(&mut self, kinds: &[ParameterKind]) {
        self.variable_names.insert(0, Vec::new());
        for kind in kinds {
            let name = if matches!(kind, ParameterKind::Lt) {
                "'_".to_owned()
            } else {
                self.fresh_name(kind)
            };
            self.used.insert(name.clone());
            self.variable_names[0].push(name);
        }
    }

    fn fresh_name(&mut self, kind: &ParameterKind) -> String {
        let prefix = match kind {
            ParameterKind::Ty => "T",
            ParameterKind::Lt => "'a",
            ParameterKind::Const => "N",
        };

        for i in 1.. {
            let name = format!("{prefix}{i}");
            if self.used.insert(name.clone()) {
                return name;
            }
        }

        unreachable!("fresh name generation should always terminate")
    }

    fn free_variable_name(&self, universal: bool, kind: ParameterKind, var_index: usize) -> String {
        let suffix = var_index + 1;
        let prefix = match (universal, kind) {
            (true, ParameterKind::Ty) => "U",
            (true, ParameterKind::Lt) => "'u",
            (true, ParameterKind::Const) => "CU",
            (false, ParameterKind::Ty) => "E",
            (false, ParameterKind::Lt) => "'e",
            (false, ParameterKind::Const) => "CE",
        };
        format!("{prefix}{suffix}")
    }
}

#[derive(Debug, Default)]
pub struct RustBuilder {
    ctx: NameContext,
}

impl RustBuilder {
    pub fn with_binder<T: Fold, R>(
        &mut self,
        binder: &Binder<T>,
        mut op: impl FnMut(&T, &mut RustBuilder) -> Fallible<R>,
    ) -> Fallible<R> {
        let term = binder.peek();
        self.ctx.push(binder.kinds());

        let result = op(term, self);

        self.ctx.pop();
        result
    }

    pub fn core_variable_to_string(&self, core_variable: &Variable) -> Fallible<String> {
        self.ctx.core_variable_to_string(core_variable)
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
        skip_first: bool,
    ) -> Fallible<syntax::Generics> {
        let names = self.ctx.current_names()?;

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
                    if let TyData::Variable(var) = ty {
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
