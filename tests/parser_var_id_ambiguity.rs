//! Test for how we handle ambiguity between a variable and an identifier in the parser.
//! This can't be part of parser-torture-tests because it requires settting up a custom variable.

use std::sync::Arc;

use formality_core::{Downcast, DowncastTo, Upcast, UpcastFrom};
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
    Perm,
}

#[term]
pub enum Parameter {
    #[cast]
    Ty(Ty),

    #[cast]
    Perm(Perm),
}

#[term]
pub enum Ty {
    #[variable(Kind::Ty)]
    Variable(Variable),

    #[cast]
    Id(Id),

    #[grammar($v0 $v1)]
    Apply(Perm, Arc<Ty>),

    #[grammar($v0 :: $v1)]
    Assoc(Arc<Ty>, Id),
}

#[term]
pub enum Perm {
    #[variable(Kind::Perm)]
    Variable(Variable),
}

formality_core::id!(Id);

formality_core::cast_impl!((BoundVar) <: (Variable) <: (Parameter));
formality_core::cast_impl!((ExistentialVar) <: (Variable) <: (Parameter));
formality_core::cast_impl!((UniversalVar) <: (Variable) <: (Parameter));

impl UpcastFrom<Variable> for Parameter {
    fn upcast_from(term: Variable) -> Self {
        match term.kind() {
            Kind::Ty => Ty::Variable(term).upcast(),
            Kind::Perm => Perm::Variable(term).upcast(),
        }
    }
}

impl DowncastTo<Variable> for Parameter {
    fn downcast_to(&self) -> Option<Variable> {
        match self {
            Parameter::Ty(ty) => ty.downcast(),
            Parameter::Perm(perm) => perm.downcast(),
        }
    }
}

impl HasKind<FormalityLang> for Parameter {
    fn kind(&self) -> formality_core::language::CoreKind<FormalityLang> {
        match self {
            Parameter::Ty(_) => Kind::Ty,
            Parameter::Perm(_) => Kind::Perm,
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

/// Test for the case where the ambiguity occurs after some
/// reductions. In this case, we parse P (the variable) as a Perm
/// and then integrate that into the type, but we could also parse
/// P as a type itself.
#[test]
#[should_panic] // FIXME
fn parse_apply() {
    let value: Binder<Ty> = ptt::term("<perm P> P i32");
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

/// Test for the case where the ambiguity occurs after some
/// reductions. In this case, we parse P (the variable) as a Perm
/// and then integrate that into the type, but we could also parse
/// P as a type itself.
#[test]
#[should_panic] // FIXME
fn parse_parameter() {
    let value: Binder<Parameter> = ptt::term("<perm P> P");
    expect_test::expect![[r#"
        Binder {
            kinds: [
                Perm,
            ],
            term: Perm(
                Variable(
                    ^perm0_0,
                ),
            ),
        }
    "#]]
    .assert_debug_eq(&value);
}
