//! Test for how we handle ambiguity between a variable and an identifier in the parser.
//! This can't be part of parser-torture-tests because it requires settting up a custom variable.

use std::sync::Arc;

// Default language for our crate
use formality_core::{language::HasKind, term};
use ptt::grammar::{Binder, BoundVar, ExistentialVar, UniversalVar, Variable};
use ptt::FormalityLang;

formality_core::declare_language! {
    mod ptt {
        const NAME = "PTT";
        type Kind = crate::Kind;
        type Parameter = crate::Parameter;
        const BINDING_OPEN = '<';
        const BINDING_CLOSE = '>';
        const KEYWORDS = [
        ];
    }
}

#[term]
#[derive(Copy)]
pub enum Kind {
    Ty,
}

#[term]
pub enum Parameter {
    #[cast]
    Ty(Ty),
}

#[term]
pub enum Ty {
    #[variable(Kind::Ty)]
    Variable(Variable),

    #[cast]
    Id(Id),

    #[grammar($v0 :: $v1)]
    Assoc(Arc<Ty>, Id),
}

formality_core::id!(Id);

formality_core::cast_impl!((BoundVar) <: (Variable) <: (Ty));
formality_core::cast_impl!((ExistentialVar) <: (Variable) <: (Ty));
formality_core::cast_impl!((UniversalVar) <: (Variable) <: (Ty));
formality_core::cast_impl!((Variable) <: (Ty) <: (Parameter));
formality_core::cast_impl!((BoundVar) <: (Ty) <: (Parameter));
formality_core::cast_impl!((ExistentialVar) <: (Ty) <: (Parameter));
formality_core::cast_impl!((UniversalVar) <: (Ty) <: (Parameter));

impl HasKind<FormalityLang> for Parameter {
    fn kind(&self) -> formality_core::language::CoreKind<FormalityLang> {
        match self {
            Parameter::Ty(_) => Kind::Ty,
        }
    }
}

#[test]
fn parse_var() {
    let value: Binder<Ty> = ptt::term("<ty T> T");
    expect_test::expect![[r#"
        Binder {
            kinds: [
                Ty,
            ],
            term: Variable(
                ^ty0_0,
            ),
        }
    "#]]
    .assert_debug_eq(&value);
}

#[test]
fn parse_id() {
    let value: Binder<Ty> = ptt::term("<ty T> i32");
    expect_test::expect![[r#"
        Binder {
            kinds: [
                Ty,
            ],
            term: Id(
                i32,
            ),
        }
    "#]]
    .assert_debug_eq(&value);
}

#[test]
fn parse_assoc() {
    let value: Binder<Ty> = ptt::term("<ty T> i32::T");
    expect_test::expect![[r#"
        Binder {
            kinds: [
                Ty,
            ],
            term: Assoc(
                Id(
                    i32,
                ),
                T,
            ),
        }
    "#]]
    .assert_debug_eq(&value);
}
