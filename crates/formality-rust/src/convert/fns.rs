use std::fmt::{self, Write};

use crate::convert::emit_where;
use crate::grammar::{Fn, FnBody, MaybeFnBody};

pub fn emit_fn<W: Write>(w: &mut W, function: &Fn) -> fmt::Result {
    write!(w, "fn {}", *function.id)?;
    let data = function.binder.peek();
    write!(w, "(")?;
    let mut sep = "";
    for arg in &data.input_args {
        write!(w, "{}{:?}", sep, arg)?;
        sep = ", ";
    }
    write!(w, ") -> {:?}", data.output_ty)?;
    emit_where(w, &data.where_clauses)?;

    match &data.body {
        MaybeFnBody::NoFnBody => writeln!(w, ";"),
        MaybeFnBody::FnBody(fn_body) => emit_fn_body(w, fn_body),
    }
}

pub fn emit_fn_body<W: Write>(w: &mut W, fn_body: &FnBody) -> fmt::Result {
    match fn_body {
        FnBody::TrustedFnBody => todo!(),
        FnBody::Literal(value, _) => writeln!(w, "\n{{ {} }}", value),
        FnBody::MiniRust(_) => unimplemented!(),
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
