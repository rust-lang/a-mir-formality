use std::ops::Deref;

use crate::grammar::{Fn, FnBody, MaybeFnBody};
use crate::pp::PrettyPrinter;
use itertools::Itertools;

impl PrettyPrinter {
    pub fn print_fn(&mut self, function: &Fn) -> String {
        let id = function.id.deref();
        let data = function.binder.peek();
        let input_args = data
            .input_args
            .iter()
            .map(|a| self.pretty_print_type(&a.ty))
            .join(", ");
        let output_arg = self.pretty_print_type(&data.output_ty);
        // TODO: Where clauses
        // self.print_where(&data.where_clauses)?;

        let body = match &data.body {
            MaybeFnBody::NoFnBody => ";".into(),
            MaybeFnBody::FnBody(fn_body) => self.print_fn_body(fn_body),
        };

        format!("fn {id}({input_args} -> {output_arg}{body})")
    }

    pub fn print_fn_body(&mut self, fn_body: &FnBody) -> String {
        match fn_body {
            FnBody::TrustedFnBody => unimplemented!(),
            FnBody::Literal(value, _) => format!("{{ {value} }}"),
            FnBody::MiniRust(_) => unimplemented!(),
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
                    fn run () -> i32 0 _ i32
                }
            ],
            fn run() -> i32 {
                0
            }
        );
    }
}
