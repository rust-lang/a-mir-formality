use crate::convert::emit_where;
use crate::grammar::{Enum, Field, FieldName, Struct};

use std::fmt::{self, Write};

pub fn emit_struct<W: Write>(w: &mut W, strukt: &Struct) -> fmt::Result {
    write!(w, "struct {}", *strukt.id)?;
    let data = strukt.binder.peek();
    emit_where(w, &data.where_clauses)?;

    writeln!(w, " {{")?;
    for f in &data.fields {
        writeln!(w, "    {:?}: {:?},", f.name, f.ty)?;
    }
    write!(w, "}}")?;

    Ok(())
}

pub fn emit_enum<W: Write>(w: &mut W, e: &Enum) -> fmt::Result {
    write!(w, "enum {}", *e.id)?;
    let data = e.binder.peek();
    emit_where(w, &data.where_clauses)?;

    writeln!(w, " {{")?;
    for v in &data.variants {
        write!(w, "    {}", *v.name)?;
        emit_fields(w, &v.fields)?;
        write!(w, ", ")?;
    }
    write!(w, "}}")
}

pub fn emit_fields<W: Write>(w: &mut W, fields: &Vec<Field>) -> fmt::Result {
    if fields.len() == 0 {
        return Ok(());
    }

    let is_tuple = matches!(fields[0].name, FieldName::Index(_));
    if is_tuple {
        write!(w, "(")?;
    } else {
        write!(w, " {{")?;
    }
    for field in fields {
        match &field.name {
            FieldName::Id(field_id) => write!(w, " {:?}: {:?} ", *field_id, field.ty)?,
            FieldName::Index(_) => write!(w, "{:?}", field.ty)?,
        }
    }
    if is_tuple {
        write!(w, ")")
    } else {
        write!(w, " }}")
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
                T: Baz,
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
                        B { },
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
                T: Baz,
            {
                A { t: T },
                B(T),
            }
        );
    }
}
