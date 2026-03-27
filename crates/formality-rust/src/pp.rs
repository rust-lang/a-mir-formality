use crate::grammar::{
    AdtItem, Binder, Crate, CrateItem, Fallible, FeatureGate, ParameterKind, Program,
};
use formality_core::{fold::CoreFold, variable::CoreVariable};

use std::collections::HashSet;

mod fns;

mod structs_enums_and_adts;

mod traits_and_impls;

mod tys;

pub fn pretty_print(program: &Program) -> Fallible<Vec<String>> {
    PrettyPrinter::default().print_program(program)
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
pub struct PrettyPrinter {
    ctx: NameContext,
}

impl PrettyPrinter {
    pub fn with_binder<T: CoreFold<crate::FormalityLang>>(
        &mut self,
        binder: &Binder<T>,
        mut op: impl FnMut(&T, &mut PrettyPrinter) -> Fallible<String>,
    ) -> Fallible<String> {
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

    pub fn print_program(&mut self, program: &Program) -> Fallible<Vec<String>> {
        program
            .crates
            .iter()
            .map(|krate| self.print_crate(krate))
            .collect::<Result<Vec<_>, _>>()
    }

    fn print_crate(&mut self, krate: &Crate) -> Fallible<String> {
        Ok(krate
            .items
            .iter()
            .map(|item| self.print_crate_item(item))
            .collect::<Result<Vec<_>, _>>()?
            .join("\n"))
    }

    fn print_crate_item(&mut self, crate_item: &CrateItem) -> Fallible<String> {
        match crate_item {
            CrateItem::FeatureGate(gate) => self.print_feature_gate(gate),
            CrateItem::AdtItem(AdtItem::Struct(strukt)) => self.print_struct(strukt),
            CrateItem::AdtItem(AdtItem::Enum(e)) => self.print_enum(e),
            CrateItem::Trait(t) => self.print_trait(t),
            CrateItem::TraitImpl(trait_impl) => self.print_trait_impl(trait_impl),
            CrateItem::NegTraitImpl(neg_trait_impl) => self.print_neg_trait_impl(neg_trait_impl),
            CrateItem::Fn(f) => self.print_fn(f),
            CrateItem::Test(_) => unimplemented!(),
        }
    }

    fn print_feature_gate(&mut self, gate: &FeatureGate) -> Fallible<String> {
        Ok(format!("{:?}", gate))
    }
}

#[macro_export]
macro_rules! assert_rust {
    ($input:tt, $($expected:tt)*) => {{
        $crate::pp::assert_rust(stringify!($input), stringify!($($expected)*));
    }};
}

pub fn assert_rust(input: &str, expected: &str) {
    let program = crate::rust::try_term(input).unwrap();
    let rust = crate::pp::PrettyPrinter::default()
        .print_program(&program)
        .unwrap()[0]
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ");
    let expected = expected.split_whitespace().collect::<Vec<_>>().join(" ");
    assert_eq!(rust, expected);
}

#[macro_export]
macro_rules! assert_rust2 {
    ([$($input:tt)*], $fn:ident, $expected:expr) => {{
        let term = $crate::rust::try_term(stringify!($($input)*)).unwrap();
        let r = $fn(term);
        let rust = r
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ");
        assert_eq!(rust, $expected);
    }};
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
