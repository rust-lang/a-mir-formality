use crate::grammar::{
    AdtItem, Binder, Crate, CrateItem, Crates, Fallible, FeatureGate, ParameterKind,
};
use formality_core::{fold::CoreFold, variable::CoreVariable};

use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
    ops::Deref,
};

mod fns;

mod structs_enums_and_adts;

mod traits_and_impls;

mod tys;

pub fn build_rust(crates: &Crates) -> Fallible<HashMap<String, String>> {
    RustBuilder::default().build_crates(crates)
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

/// Asserts that the given Formality input is translated into the expected
/// Rust code. Only a single crate is supported.
///
/// The Formality `input` must be provided as a token tree. The `expected` Rust
/// output may be given either as a string literal or as another token tree.
#[macro_export]
macro_rules! assert_rust {
    ($input:tt, $expected:literal) => {{
        $crate::to_rust::assert_rust(stringify!($input), $expected);
    }};
    ($input:tt, $($expected:tt)*) => {{
        $crate::to_rust::assert_rust(stringify!($input), stringify!($($expected)*));
    }};
}

#[track_caller]
pub fn assert_rust(input: &str, expected: &str) {
    let program = crate::rust::try_term(input).unwrap();
    let rust = crate::to_rust::RustBuilder::default()
        .build_crates(&program)
        .unwrap()
        .into_iter()
        .next()
        .unwrap()
        .1
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ");
    let expected = expected.split_whitespace().collect::<Vec<_>>().join(" ");
    assert_eq!(rust, expected);
}

#[cfg(test)]
mod test {
    #[test]
    fn empty_crate() {
        assert_rust!(
            [
                crate Foo {}
            ],
        );
    }

    #[test]
    fn simple_feature_gate() {
        assert_rust!(
            [
                crate Foo {
                    #![feature(polonius_alpha)]
                }
            ],
            #![feature(polonius_alpha)]
        );
    }
}
