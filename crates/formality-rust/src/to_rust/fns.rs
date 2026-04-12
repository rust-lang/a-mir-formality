use std::{fmt::Write, ops::Deref};

use crate::grammar::{Fallible, Fn, FnBody, MaybeFnBody};
use crate::to_rust::{CodeWriter, RustBuilder};

impl RustBuilder {
    pub fn write_fn(&mut self, out: &mut CodeWriter, function: &Fn) -> Fallible<()> {
        write!(out, "fn {}(", function.id.deref())?;

        let data = function.binder.peek();

        let mut sep = "";
        for arg in &data.input_args {
            let ty = self.ty_to_string(&arg.ty)?;
            let name = "todo";
            write!(out, "{sep}{name}: {ty}")?;
            sep = ", ";
        }

        let output_arg = self.ty_to_string(&data.output_ty)?;
        write!(out, ") -> {output_arg}")?;

        // TODO: Where clauses
        // self.write_where(&data.where_clauses)?;

        match &data.body {
            MaybeFnBody::NoFnBody => writeln!(out, ";")?,
            MaybeFnBody::FnBody(fn_body) => {
                writeln!(out, "")?;
                self.write_fn_body(out, fn_body)?;
            }
        };

        Ok(())
    }

    pub fn write_fn_body(&mut self, out: &mut CodeWriter, fn_body: &FnBody) -> Fallible<()> {
        match fn_body {
            FnBody::TrustedFnBody => writeln!(out, "{{ panic!(\"Trusted Fn Body\") }}")?,
            FnBody::Expr(block) => self.write_block(out, block)?,
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {

    #[test]
    fn simple_fn() {
        crate::assert_rust!(
            [
                crate Foo {
                    fn run () -> i32 {trusted}
                }
            ],
            fn run() -> i32 { panic!("Trusted Fn Body") }
        );
    }
}
