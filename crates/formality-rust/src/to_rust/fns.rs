use std::{fmt::Write, ops::Deref};

use crate::grammar::{
    Fallible, Fn, FnBody, InputArg, MaybeFnBody, TyData, WhereClause, WhereClauseData,
};
use crate::to_rust::{CodeWriter, RustBuilder};

impl RustBuilder {
    pub fn write_fn(&mut self, out: &mut CodeWriter, function: &Fn) -> Fallible<()> {
        // "fn {id}{params}({input_args}) -> {output_arg}{bounds}{body}"
        self.with_binder(&function.binder, |term, pp| {
            write!(out, "fn {}", function.id.deref())?;

            pp.write_params(out, &term.where_clauses, &term.input_args)?;

            write!(out, "(")?;
            let mut sep = "";
            for arg in &term.input_args {
                let ty = pp.ty_to_string(&arg.ty)?;
                let name = arg.id.deref();
                write!(out, "{sep}{name}: {ty}")?;
                sep = ", ";
            }

            let output_arg = pp.ty_to_string(&term.output_ty)?;
            write!(out, ") -> {output_arg}")?;

            // pp.write_bounds(out, &term.where_clauses)?;
            pp.write_where_bounds(out, &term.where_clauses)?;

            match &term.body {
                MaybeFnBody::NoFnBody => writeln!(out, ";")?,
                MaybeFnBody::FnBody(fn_body) => {
                    writeln!(out, "")?;
                    pp.write_fn_body(out, fn_body)?;
                }
            };

            Ok(())
        })
    }

    fn write_params(
        &mut self,
        out: &mut CodeWriter,
        where_clauses: &Vec<WhereClause>,
        input_args: &Vec<InputArg>,
    ) -> Fallible<()> {
        // <T1, T2, const N1: bool, const N2: u8>
        if where_clauses.is_empty() && input_args.is_empty() {
            return Ok(());
        }

        let mut buffer = String::new();
        let mut sep = "";
        for var in input_args.iter().filter_map(|arg| {
            if let TyData::Variable(var) = arg.ty.data() {
                Some(var)
            } else {
                None
            }
        }) {
            let var = self.core_variable_to_string(var)?;
            write!(&mut buffer, "{sep}{var}")?;
            sep = ", ";
        }

        for wc in where_clauses {
            match wc.data() {
                WhereClauseData::IsImplemented(_, _, _) => continue,
                WhereClauseData::AliasEq(_, _) => todo!(),
                WhereClauseData::Outlives(_, _) => todo!(),
                WhereClauseData::ForAll(_) => todo!(),
                WhereClauseData::TypeOfConst(konst, ty) => {
                    let konst = self.const_to_string(konst)?;
                    let ty = self.ty_to_string(ty)?;
                    write!(buffer, "const {konst}: {ty}")?;
                }
            }
            write!(out, "{sep}")?;
            sep = ", ";
        }

        if !buffer.is_empty() {
            write!(out, "<{buffer}>")?;
        }

        Ok(())
    }

    fn write_fn_body(&mut self, out: &mut CodeWriter, fn_body: &FnBody) -> Fallible<()> {
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
                    fn run() -> i32 {trusted}
                }
            ],
            fn run() -> i32 { panic!("Trusted Fn Body") }
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
            fn run(p1: i32, p2: i32) -> i32 { panic!("Trusted Fn Body") }
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
            fn run<T1>(p1: T1) -> T1 { panic!("Trusted Fn Body") }
        );
    }

    #[test]
    fn simple_fn_with_bounds() {
        crate::assert_rust!(
            [
                crate Foo {
                    fn run<T>(p1: T) -> T where T: Bar {trusted}
                }
            ],
            fn run<T1>(p1: T1) -> T1 where T1: Bar { panic!("Trusted Fn Body") }
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
            fn run<const N1: i32>() -> i32 { panic!("Trusted Fn Body") }
        );
    }
}
