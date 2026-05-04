use std::ops::Deref;

use crate::grammar::{Fallible, Fn, FnBody, InputArg, MaybeFnBody};
use crate::prove::prove::Safety;

use super::{syntax, RustBuilder};

impl RustBuilder {
    pub fn lower_fn(&mut self, function: &Fn) -> Fallible<syntax::FunctionItem> {
        self.with_binder(
            &function.binder,
            false,
            |term| &term.where_clauses,
            |term, generics, pp| {
                let params = term
                    .input_args
                    .iter()
                    .map(|arg| pp.lower_fn_param(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                let return_ty = pp.lower_ty(&term.output_ty)?;
                let body = pp.lower_fn_body(&term.body)?;

                Ok(syntax::FunctionItem {
                    is_unsafe: matches!(function.safety, Safety::Unsafe),
                    name: function.id.deref().clone(),
                    generics,
                    params,
                    return_ty,
                    body,
                })
            },
        )
    }

    fn lower_fn_param(&mut self, arg: &InputArg) -> Fallible<syntax::FnParam> {
        Ok(syntax::FnParam {
            // TODO: Is there a way to know if a variable must be mutable?
            mutable: true,
            name: arg.id.deref().clone(),
            ty: self.lower_ty(&arg.ty)?,
        })
    }

    fn lower_fn_body(&mut self, body: &MaybeFnBody) -> Fallible<syntax::FunctionBody> {
        match body {
            MaybeFnBody::NoFnBody => Ok(syntax::FunctionBody::Declaration),
            MaybeFnBody::FnBody(fn_body) => match fn_body {
                FnBody::TrustedFnBody => Ok(syntax::FunctionBody::Trusted),
                FnBody::Expr(block) => Ok(syntax::FunctionBody::Block(self.lower_block(block)?)),
            },
        }
    }
}

#[cfg(test)]
mod test {

    #[test]
    fn simple_fn() {
        crate::assert_rust!(
            [
                crate Foo {
                    fn run() -> i32 {trusted}
                }
            ],
            r#"
fn run() -> i32 {
    panic!("Trusted Fn Body")
}
"#
        );
    }

    #[test]
    fn simple_fn_with_args() {
        crate::assert_rust!(
            [
                crate Foo {
                    fn run(p1: i32, p2: i32) -> i32 {trusted}
                }
            ],
            r#"
fn run(mut p1: i32, mut p2: i32) -> i32 {
    panic!("Trusted Fn Body")
}
"#
        );
    }

    #[test]
    fn simple_fn_with_generics() {
        crate::assert_rust!(
            [
                crate Foo {
                    fn run<T>(p1: T) -> T {trusted}
                }
            ],
            r#"
fn run<T0>(mut p1: T0) -> T0 {
    panic!("Trusted Fn Body")
}
"#
        );
    }

    #[test]
    fn simple_fn_with_bounds() {
        crate::assert_rust!(
            [
                crate Foo {
                    trait Bar { }
                    fn run<T>(p1: T) -> T where T: Bar {trusted}
                }
            ],
            r#"
trait Bar { }

fn run<T1>(mut p1: T1) -> T1 where T1: Bar {
    panic!("Trusted Fn Body")
}
"#
        );
    }

    #[test]
    fn simple_fn_with_const_bounds() {
        crate::assert_rust!(
            [
                crate Foo {
                    fn run<const N>() -> i32 where type_of_const N is i32 { trusted }

                }
            ],
            r#"
fn run<const N0: i32>() -> i32 {
    panic!("Trusted Fn Body")
}
"#
        );
    }
}
