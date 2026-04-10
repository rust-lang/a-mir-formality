use std::ops::Deref;

use crate::grammar::{Fallible, Fn, FnBody, MaybeFnBody};
use crate::to_rust::RustBuilder;

impl RustBuilder {
    pub fn build_fn(&mut self, function: &Fn) -> Fallible<String> {
        let id = function.id.deref();
        let data = function.binder.peek();
        let input_args = data
            .input_args
            .iter()
            .map(|a| self.pretty_print_type(&a.ty))
            .collect::<Result<Vec<_>, _>>()?
            .join(", ");
        let output_arg = self.pretty_print_type(&data.output_ty)?;
        // TODO: Where clauses
        // self.print_where(&data.where_clauses)?;

        let body = match &data.body {
            MaybeFnBody::NoFnBody => ";".into(),
            MaybeFnBody::FnBody(fn_body) => format!(" {}", self.build_fn_body(fn_body)),
        };

        Ok(format!("fn {id}({input_args}) -> {output_arg}{body}"))
    }

    pub fn build_fn_body(&mut self, fn_body: &FnBody) -> String {
        match fn_body {
            FnBody::TrustedFnBody => format!("{{ panic!(\"Trusted Fn Body\") }}"),
            FnBody::Expr(_) => unimplemented!("expr fn body"),
        }
    }
}

#[cfg(test)]
mod test {

    #[test]
    #[ignore = "ignore until clear how trusted should be parsed"]
    fn simple_fn() {
        crate::assert_rust!(
            [
                crate Foo {
                    fn run () -> i32 trusted;
                }
            ],
            fn run() -> i32 { panic!("Trusted Fn Body") }
        );
    }
}
