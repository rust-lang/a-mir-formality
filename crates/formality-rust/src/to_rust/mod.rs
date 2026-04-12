use crate::grammar::{
    AdtItem, Binder, Crate, CrateItem, Crates, Fallible, FeatureGate, ParameterKind,
};
use formality_core::{fold::CoreFold, variable::CoreVariable};

use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
    ops::Deref,
};

mod expr;
mod fns;
mod structs_enums_and_adts;
pub mod test_utils;
mod traits_and_impls;
mod tys;

pub fn build_workspace(
    crates: &Crates,
    root_direcotry: &std::path::Path,
) -> Fallible<std::process::Output> {
    let root_toml = create_workspace(crates, root_direcotry)?;

    let output = std::process::Command::new("cargo")
        .env("RUSTFLAGS", "-A warnings")
        .args(["build", "--manifest-path", &root_toml])
        .output()?;
    Ok(output)
}

/// Produces a Cargo workspaces on the file system in the `root_direcotry`. After sucesfully creating all the files, `cargo fmt` is run.
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
        file.write_all(&source.as_bytes())?;

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
        .args(["fmt", "--manifest-path", location])
        .spawn()?;

    Ok(location.to_string())
}

type Stack<T> = Vec<T>;

/// Keeps track
#[derive(Debug, Default)]
pub struct NameContext {
    variable_names: Stack<Vec<String>>,
    used: HashSet<String>,
}

impl NameContext {
    /// Returns a pretty printable name for the `given CoreVaribale`.
    pub fn variable_name(&self, variable: &CoreVariable<crate::FormalityLang>) -> Fallible<String> {
        let CoreVariable::BoundVar(core_bound_var) = variable else {
            unimplemented!()
        };

        let var_index = core_bound_var.var_index.index;
        let index = core_bound_var
            .debruijn
            .ok_or_else(|| anyhow::anyhow!("binder was opened"))?
            .index;
        self.variable_names
            .get(index)
            .and_then(|vars| vars.get(var_index))
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("unbound variable {core_bound_var:?}"))
    }

    pub fn pop(&mut self) {
        let name = self.variable_names.remove(0);
        for n in name {
            self.used.remove(&n);
        }
    }

    pub fn push(&mut self, kinds: &[ParameterKind]) {
        self.variable_names.insert(0, Vec::new());
        for kind in kinds {
            let name = self.fresh_name(kind);
            self.variable_names[0].push(name);
        }
    }

    fn fresh_name(&mut self, kind: &ParameterKind) -> String {
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

#[derive(Debug, Default)]
pub struct CodeWriter {
    indention_level: i32,
    buffer: String,
}

const INDENT_SIZE: i32 = 4;

impl Write for CodeWriter {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        let n_spaces = self.indention_level * INDENT_SIZE;
        let spaces = " ".repeat(n_spaces as usize);

        // TODO: Update indention level

        write!(self.buffer, "{spaces}{s}")
    }
}

#[derive(Debug, Default)]
pub struct RustBuilder {
    ctx: NameContext,
}

impl RustBuilder {
    pub fn with_binder<T: CoreFold<crate::FormalityLang, Output = T>>(
        &mut self,
        binder: &Binder<T>,
        mut op: impl FnMut(&T, &mut RustBuilder) -> Fallible<()>,
    ) -> Fallible<()> {
        let term = binder.peek();
        self.ctx.push(binder.kinds());

        let result = op(term, self);

        self.ctx.pop();
        result
    }

    pub fn variable_name(
        &self,
        core_variable: &CoreVariable<crate::FormalityLang>,
    ) -> Fallible<String> {
        self.ctx.variable_name(core_variable)
    }

    pub fn build_crates(&mut self, crates: &Crates) -> Fallible<HashMap<String, String>> {
        crates
            .crates
            .iter()
            .map(|krate| {
                let mut out = CodeWriter::default();
                let result = self.write_crate(&mut out, krate);
                if result.is_ok() {
                    return Ok((krate.id.deref().clone(), out.buffer));
                }
                Err(result.unwrap_err())
            })
            .collect::<Result<HashMap<_, _>, _>>()
    }

    fn write_crate(&mut self, out: &mut CodeWriter, krate: &Crate) -> Fallible<()> {
        for item in &krate.items {
            self.write_crate_item(out, item)?;
        }
        Ok(())
    }

    fn write_crate_item(&mut self, out: &mut CodeWriter, crate_item: &CrateItem) -> Fallible<()> {
        match crate_item {
            CrateItem::FeatureGate(gate) => self.write_feature_gate(out, gate),
            CrateItem::AdtItem(AdtItem::Struct(strukt)) => self.write_struct(out, strukt),
            CrateItem::AdtItem(AdtItem::Enum(e)) => self.write_enum(out, e),
            CrateItem::Trait(t) => self.write_trait(out, t),
            CrateItem::TraitImpl(trait_impl) => self.write_trait_impl(out, trait_impl),
            CrateItem::NegTraitImpl(neg_trait_impl) => {
                self.write_neg_trait_impl(out, neg_trait_impl)
            }
            CrateItem::Fn(f) => self.write_fn(out, f),
            CrateItem::Test(_) => unimplemented!(),
        }
    }

    fn write_feature_gate(&mut self, out: &mut CodeWriter, gate: &FeatureGate) -> Fallible<()> {
        writeln!(out, "{:?}", gate)?;
        Ok(())
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
            #![feature(polonius_alpha)]
        );
    }
}
