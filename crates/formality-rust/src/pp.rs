use crate::grammar::{AdtItem, Crate, CrateItem, FeatureGate, ParameterKind, Program};
use formality_core::variable::CoreVariable;
use itertools::Itertools;

use std::collections::HashSet;

mod fns;

mod structs_enums_and_adts;

mod traits_and_impls;

mod tys;

pub fn pretty_print(program: &Program) -> Vec<String> {
    PrettyPrinter::default().print_program(program)
}

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
        self.push(n, ParameterKind::Ty);
    }

    /// Removes the `n` latest type names from the context.
    /// Panics if `n` is bigger than the numbers of names in the context.
    pub fn pop_tys(&mut self, n: usize) {
        self.pop(n, ParameterKind::Ty);
    }

    /// Pushes `n` fresh life time names to the context.
    pub fn push_lts(&mut self, n: usize) {
        self.push(n, ParameterKind::Lt);
    }

    /// Removes the `n` latest life time names from the context.
    /// Panics if `n` is bigger than the numbers of names in the context.
    pub fn pop_lts(&mut self, n: usize) {
        self.pop(n, ParameterKind::Lt);
    }

    /// Pushes `n` fresh const names to the context.
    pub fn push_const(&mut self, n: usize) {
        self.push(n, ParameterKind::Const);
    }

    /// Removes the `n` latest const names from the context.
    /// Panics if `n` is bigger than the numbers of names in the context.
    pub fn pop_const(&mut self, n: usize) {
        self.pop(n, ParameterKind::Const);
    }

    /// Returns a pretty printable name for the `given CoreVaribale`.
    pub fn variable_name(&self, variable: &CoreVariable<crate::FormalityLang>) -> String {
        let CoreVariable::BoundVar(core_bound_var) = variable else {
            unimplemented!()
        };

        // TODO: I must take var_index into account.
        // The Debruijn index indicates the nesting level, while var_index indicates the index inside the binder.
        // Idea: to not store Vec<String> but Vec<Vec<String>>, where the db index is used to index Vec<Vec<String>> and var_index Vec<String>.
        // let type_name = self.ty_names[index][var_index];
        let _var_index = core_bound_var.var_index;
        let index = core_bound_var.debruijn.unwrap().index;
        match core_bound_var.kind {
            ParameterKind::Ty => self.ty_names[index].clone(),
            ParameterKind::Lt => self.lt_names[index].clone(),
            ParameterKind::Const => self.const_names[index].clone(),
        }
    }

    fn push(&mut self, n: usize, kind: ParameterKind) {
        let insert = match kind {
            ParameterKind::Ty => |ctx: &mut NameContext, name: String| ctx.ty_names.insert(0, name),
            ParameterKind::Lt => |ctx: &mut NameContext, name: String| ctx.lt_names.insert(0, name),
            ParameterKind::Const => {
                |ctx: &mut NameContext, name: String| ctx.const_names.insert(0, name)
            }
        };

        for _ in 0..n {
            let name = self.fresh_name(kind);
            insert(self, name);
        }
    }

    fn pop(&mut self, n: usize, kind: ParameterKind) {
        let remove = match kind {
            ParameterKind::Ty => |ctx: &mut NameContext| ctx.ty_names.remove(0),
            ParameterKind::Lt => |ctx: &mut NameContext| ctx.lt_names.remove(0),
            ParameterKind::Const => |ctx: &mut NameContext| ctx.const_names.remove(0),
        };
        for _ in 0..n {
            let name = remove(self);
            self.used.remove(&name);
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

#[derive(Debug, Default)]
pub struct PrettyPrinter {
    ctx: NameContext,
}

impl PrettyPrinter {
    pub fn ty_name(&self, core_variable: &CoreVariable<crate::FormalityLang>) -> String {
        dbg!(&core_variable);
        self.ctx.variable_name(core_variable)
    }

    pub fn print_program(&mut self, program: &Program) -> Vec<String> {
        program
            .crates
            .iter()
            .map(|krate| self.print_crate(krate))
            .collect()
    }

    fn print_crate(&mut self, krate: &Crate) -> String {
        krate
            .items
            .iter()
            .map(|item| self.print_crate_item(item))
            .join("\n")
    }

    fn print_crate_item(&mut self, crate_item: &CrateItem) -> String {
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

    fn print_feature_gate(&mut self, gate: &FeatureGate) -> String {
        format!("{:?}", gate)
    }
}

#[macro_export]
macro_rules! assert_rust {
    ($input:tt, $($expected:tt)*) => {{
        let program = $crate::rust::try_term(stringify!($input)).unwrap();
        let rust = $crate::pp::PrettyPrinter::default().print_program(&program)[0]
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ");
        assert_eq!(rust, stringify!($($expected)*));
    }};
}

pub fn assert_rust(program: &Program, expected: &str) {
    let rust = crate::pp::PrettyPrinter::default().print_program(&program)[0]
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ");
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
    use crate::grammar::TraitItem;

    use super::*;

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

    #[test]
    fn blub() {
        use std::io::Write;
        let source = stringify!(
            [
                crate Foo {
                    trait Write {
                        fn test() -> i32;
                    }

                    enum Kind {
                        A { },
                        B { }
                    }

                    struct Map
                    where
                        T: Write,
                        K: Write,
                    {
                        f0: T,
                        f1: K,
                        f2: Kind,
                    }

                    fn run() -> i32 trusted;
                }
            ]
        );

        let program: Program = crate::rust::try_term(source).unwrap();
        let rust = pretty_print(&program);
        let rust = &rust[0];
        let mut file = std::fs::File::create("/tmp/test.rs").unwrap();
        file.write_all(rust.as_bytes()).unwrap();
    }

    #[test]
    #[ignore]
    fn blub2() {
        let source = stringify!(
            [
                crate Foo {
                    trait Bar {
                        fn foo() -> i32;
                    }
                }
            ]
        );

        let program: Program = crate::rust::try_term(source).unwrap();
        let CrateItem::Trait(item) = program.crates.get(0).unwrap().items.get(0).unwrap() else {
            panic!();
        };
        let data = item.binder.open();
        let TraitItem::Fn(f) = data.1.trait_items.get(0).unwrap() else {
            panic!();
        };
        dbg!(f.binder.open().1.output_ty);

        assert!(false);
    }
}
