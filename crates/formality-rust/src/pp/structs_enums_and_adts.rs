use crate::grammar::{Enum, Field, Struct, Variant};
use crate::pp::PrettyPrinter;
use itertools::Itertools;

use std::ops::Deref;

impl PrettyPrinter {
    pub fn print_struct(&mut self, strukt: &Struct) -> String {
        let id = strukt.id.deref();
        let data = strukt.binder.peek();
        // emit_where(w, &data.where_clauses)?;

        let fields = data
            .fields
            .iter()
            .map(|f| format!("{:?}: {}", f.name, self.pretty_print_type(&f.ty))) // TODO: Implement pp for names
            .join(", ");
        format!("structr {id} {{ {fields} }}")
    }

    pub fn print_enum(&mut self, e: &Enum) -> String {
        let id = e.id.deref();
        let data = e.binder.peek();
        // emit_where(w, &data.where_clauses)?;

        let variants = data
            .variants
            .iter()
            .map(|v| self.print_variant(v))
            .join(", ");

        format!("enum {id} {{ {variants} }}")
    }

    pub fn print_variant(&mut self, _variant: &Variant) -> String {
        todo!()
    }

    pub fn print_fields(&mut self, _fields: &Vec<Field>) -> String {
        // if fields.len() == 0 {
        //     return Ok(());
        // }

        // let is_tuple = matches!(fields[0].name, FieldName::Index(_));
        // if is_tuple {
        //     write!(w, "(")?;
        // } else {
        //     write!(w, " {{")?;
        // }
        // for field in fields {
        //     match &field.name {
        //         FieldName::Id(field_id) => write!(w, " {:?}: {:?} ", *field_id, field.ty)?,
        //         FieldName::Index(_) => write!(w, "{:?}", field.ty)?,
        //     }
        // }
        // if is_tuple {
        //     write!(w, ")")
        // } else {
        //     write!(w, " }}")
        // }
        todo!()
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
