use std::ops::Deref;

use crate::grammar::{
    Fallible, Fn, FnBody, InputArg, MaybeFnBody, ParameterKind, TyData, WhereClause,
};
use crate::prove::prove::Safety;

use super::{syntax, RustBuilder};

impl RustBuilder {
    pub fn lower_fn(&mut self, function: &Fn) -> Fallible<syntax::FunctionItem> {
        self.with_binder(&function.binder, |term, pp| {
            let generics = pp.lower_generics_for_fn(
                function.binder.kinds(),
                &term.where_clauses,
                &term.input_args,
            )?;
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
        })
    }

    fn lower_generics_for_fn(
        &mut self,
        binder_kinds: &[ParameterKind],
        where_clauses: &[WhereClause],
        input_args: &[InputArg],
    ) -> Fallible<syntax::Generics> {
        let mut generics = self.lower_generics_for_binder(binder_kinds, where_clauses, false)?;

        for arg in input_args {
            if let TyData::Variable(var) = arg.ty.data() {
                let name = self.core_variable_to_string(var)?;
                Self::push_type_param_if_missing(&mut generics.params, name);
            }
        }

        Ok(generics)
    }

    fn lower_fn_param(&mut self, arg: &InputArg) -> Fallible<syntax::FnParam> {
        Ok(syntax::FnParam {
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
fn run(p1: i32, p2: i32) -> i32 {
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
fn run<T1>(p1: T1) -> T1 {
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

fn run<T1>(p1: T1) -> T1 where T1: Bar {
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
fn run<const N1: i32>() -> i32 {
    panic!("Trusted Fn Body")
}
"#
        );
    }
}
