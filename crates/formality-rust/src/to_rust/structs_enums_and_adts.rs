use crate::grammar::{Enum, Fallible, Field, FieldName, Struct, Variant};
use crate::to_rust::{CodeWriter, RustBuilder};

use std::{fmt::Write, ops::Deref};

impl RustBuilder {
    pub fn write_struct(&mut self, out: &mut CodeWriter, strukt: &Struct) -> Fallible<()> {
        self.with_binder(&strukt.binder, |term, pp| {
            write!(out, "struct {}", strukt.id.deref())?;

            pp.write_generic_params(out, &term.where_clauses)?;
            pp.write_where_bounds(out, &term.where_clauses)?;

            writeln!(out, " {{")?;
            for field in &term.fields {
                let ty = pp.ty_to_string(&field.ty)?;
                let name = pp.field_name_to_string(&field.name);
                writeln!(out, "{name}: {ty},")?;
            }
            writeln!(out, "}}")?;
            Ok(())
        })
    }

    pub fn write_enum(&mut self, out: &mut CodeWriter, e: &Enum) -> Fallible<()> {
        self.with_binder(&e.binder, |term, pp| {
            write!(out, "enum {}", e.id.deref())?;

            pp.write_generic_params(out, &term.where_clauses)?;
            pp.write_where_bounds(out, &term.where_clauses)?;

            writeln!(out, " {{")?;
            for variant in &term.variants {
                pp.write_variant(out, variant)?;
            }

            writeln!(out, "}}")?;
            Ok(())
        })
    }

    pub fn field_name_to_string(&mut self, field_name: &FieldName) -> String {
        match field_name {
            FieldName::Id(id) => id.deref().clone(),
            FieldName::Index(idx) => format!("{idx}"),
        }
    }

    fn write_variant(&mut self, out: &mut CodeWriter, variant: &Variant) -> Fallible<()> {
        write!(out, "{}", variant.name.deref())?;
        self.write_fields(out, &variant.fields)?;
        writeln!(out, ",")?;
        Ok(())
    }

    fn write_fields(&mut self, out: &mut CodeWriter, fields: &Vec<Field>) -> Fallible<()> {
        if fields.len() == 0 {
            return Ok(());
        }

        let closing = if matches!(fields[0].name, FieldName::Index(_)) {
            write!(out, "(")?;
            ")"
        } else {
            write!(out, " {{ ")?;
            " }"
        };

        let mut sep = "";
        for field in fields {
            match &field.name {
                FieldName::Id(field_id) => {
                    let ty = self.ty_to_string(&field.ty)?;
                    write!(out, "{sep}{}: {ty}", field_id.deref())?;
                    sep = ",\n";
                }
                FieldName::Index(_) => {
                    let ty = self.ty_to_string(&field.ty)?;
                    write!(out, "{sep}{ty}")?;
                    sep = ",\n";
                }
            }
        }
        write!(out, "{closing}")?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn simple_struct() {
        crate::assert_rust!(
            [
                crate Foo {
                    struct Bar {
                        a: i32,
                        b: i32,
                    }
                }
            ],
            struct Bar {
                a: i32,
                b: i32,
            }
        );
    }

    #[test]
    fn struct_with_where_implemented() {
        crate::assert_rust!(
            [
                crate Foo {
                    struct Bar
                        where
                        T: Baz
                    {
                        a: T,
                    }
                }
            ],
            struct Bar<T>
            where
                T: Baz
            {
                a: T,
            }
        );
    }

    #[test]
    fn simple_enum() {
        crate::assert_rust!(
            [
                crate Foo {
                    enum Bar {
                        A { },
                        B { }
                    }
                }
            ],
            enum Bar {
                A,
                B,
            }
        );
    }

    #[test]
    fn enum_implemented() {
        crate::assert_rust!(
            [
                crate Foo {
                    enum Bar
                        where
                        T : Baz
                    {
                        A { t: T },
                        B { 0: T },
                    }
                }
            ],
            enum Bar<T>
            where
                T: Baz
            {
                A { t: T },
                B(T),
            }
        );
    }
}
