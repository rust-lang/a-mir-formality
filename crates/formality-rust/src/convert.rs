use crate::grammar::{Crate, CrateItem, FeatureGate, Program};
use std::fmt::{self, Write};

mod fns;
pub use fns::*;

mod structs_enums_and_adts;
pub use structs_enums_and_adts::*;

mod traits_and_impls;
pub use traits_and_impls::*;

mod tys;
pub use tys::*;

// mod ty;

pub fn convert(program: &Program) -> String {
    let mut buffer = String::new();
    emit_crate(&mut buffer, &program.crates[0]).unwrap();
    buffer
}

fn emit_crate<W: Write>(w: &mut W, krate: &Crate) -> fmt::Result {
    for item in &krate.items {
        emit_crate_item(w, &item)?;
        writeln!(w, "\n")?;
    }
    Ok(())
}

fn emit_crate_item<W: Write>(w: &mut W, crate_item: &CrateItem) -> fmt::Result {
    match crate_item {
        CrateItem::FeatureGate(gate) => emit_feature_gate(w, gate),
        CrateItem::Struct(strukt) => emit_struct(w, strukt),
        CrateItem::Enum(e) => emit_enum(w, e),
        CrateItem::Trait(t) => emit_trait(w, t),
        CrateItem::TraitImpl(trait_impl) => emit_trait_impl(w, trait_impl),
        CrateItem::NegTraitImpl(neg_trait_impl) => emit_neg_trait_impl(w, neg_trait_impl),
        CrateItem::Fn(f) => emit_fn(w, f),
        CrateItem::Test(_) => unimplemented!(),
    }
}

fn emit_feature_gate<W: Write>(w: &mut W, gate: &FeatureGate) -> fmt::Result {
    write!(w, "{:?}", gate)
}

#[macro_export]
macro_rules! assert_rust {
    ($input:tt, $($expected:tt)*) => {{
        let program = $crate::rust::try_term(stringify!($input)).unwrap();
        let rust = $crate::convert::convert(&program)
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ");
        assert_eq!(rust, stringify!($($expected)*));
    }};
}

pub fn assert_rust(program: &Program, expected: &str) {
    let rust = crate::convert::convert(&program)
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ");
    assert_eq!(rust, expected);
}

#[macro_export]
macro_rules! assert_rust2 {
    ([$($input:tt)*], $fn:ident, $expected:expr) => {{
        let term = $crate::rust::try_term(stringify!($($input)*)).unwrap();
        let mut buffer = String::new();
        $fn(&mut buffer, term);
        let rust = buffer
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ");
        assert_eq!(rust, $expected);
    }};
}

#[cfg(test)]
mod test {
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

                    fn run() -> i32 0 _ i32
                }
            ]
        );

        let program: Program = crate::rust::try_term(source).unwrap();
        let rust = convert(&program);
        let mut file = std::fs::File::create("/tmp/test.rs").unwrap();
        file.write_all(rust.as_bytes()).unwrap();
    }
}
