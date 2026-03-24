use crate::grammar::{Enum, Field, FieldName, Struct, Variant};
use crate::pp::PrettyPrinter;
use itertools::Itertools;

use std::ops::Deref;

impl PrettyPrinter {
    pub fn print_struct(&mut self, strukt: &Struct) -> String {
        let id = strukt.id.deref();
        let data = strukt.binder.peek();
        let wc = self.print_where(&data.where_clauses);

        let fields = data
            .fields
            .iter()
            .map(|f| format!("{:?}: {}", f.name, self.pretty_print_type(&f.ty))) // TODO: Implement pp for names
            .join(", ");
        format!("struct {id}{wc} {{ {fields} }}")
    }

    pub fn print_enum(&mut self, e: &Enum) -> String {
        let id = e.id.deref();
        let data = e.binder.peek();
        let wc = self.print_where(&data.where_clauses);

        let variants = data
            .variants
            .iter()
            .map(|v| self.print_variant(v))
            .join(", ");

        format!("enum {id}{wc} {{ {variants} }}")
    }

    pub fn print_variant(&mut self, variant: &Variant) -> String {
        let name = variant.name.deref();
        let fields = self.print_fields(&variant.fields);

        format!("{name}{fields}")
    }

    pub fn print_fields(&mut self, fields: &Vec<Field>) -> String {
        if fields.len() == 0 {
            return "".into();
        }
        let (opening, closing) = if matches!(fields[0].name, FieldName::Index(_)) {
            ("(", ")")
        } else {
            (" { ", " }")
        };

        let fields = fields
            .iter()
            .map(|f| match &f.name {
                FieldName::Id(field_id) => {
                    format!("{}: {}", field_id.deref(), self.pretty_print_type(&f.ty))
                }
                FieldName::Index(_) => format!("{}", self.pretty_print_type(&f.ty)),
            })
            .join(", ");
        format!("{opening}{fields}{closing}")
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
                b: i32
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
                a: T
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
                B
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
                B(T)
            }
        );
    }
}
