use crate::grammar::rust_builder::RustBuilder;
use crate::grammar::{Enum, Fallible, Field, FieldName, Struct, Variant};

use std::ops::Deref;

impl RustBuilder {
    pub fn build_struct(&mut self, strukt: &Struct) -> Fallible<String> {
        let id = strukt.id.deref();
        let data = strukt.binder.peek();
        let wc = self.build_where(&data.where_clauses)?;

        let fields = data
            .fields
            .iter()
            .map(|f| {
                self.pretty_print_type(&f.ty)
                    .map(|ty| format!("{:?}: {}", f.name, ty))
            }) // TODO: Implement pp for names
            .collect::<Result<Vec<_>, _>>()?
            .join(", ");
        Ok(format!("struct {id}{wc} {{ {fields} }}"))
    }

    pub fn build_enum(&mut self, e: &Enum) -> Fallible<String> {
        let id = e.id.deref();
        let data = e.binder.peek();
        let wc = self.build_where(&data.where_clauses)?;

        let variants = data
            .variants
            .iter()
            .map(|v| self.build_print_variant(v))
            .collect::<Result<Vec<_>, _>>()?
            .join(", ");

        Ok(format!("enum {id}{wc} {{ {variants} }}"))
    }

    pub fn build_print_variant(&mut self, variant: &Variant) -> Fallible<String> {
        let name = variant.name.deref();
        let fields = self.build_fields(&variant.fields)?;

        Ok(format!("{name}{fields}"))
    }

    pub fn build_fields(&mut self, fields: &Vec<Field>) -> Fallible<String> {
        if fields.len() == 0 {
            return Ok("".into());
        }

        let (opening, closing) = if matches!(fields[0].name, FieldName::Index(_)) {
            ("(", ")")
        } else {
            (" { ", " }")
        };

        let fields = fields
            .iter()
            .map(|f| match &f.name {
                FieldName::Id(field_id) => self
                    .pretty_print_type(&f.ty)
                    .map(|ty| format!("{}: {}", field_id.deref(), ty)),
                FieldName::Index(_) => self.pretty_print_type(&f.ty).map(|ty| format!("{}", ty)),
            })
            .collect::<Result<Vec<_>, _>>()?
            .join(", ");
        Ok(format!("{opening}{fields}{closing}"))
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
