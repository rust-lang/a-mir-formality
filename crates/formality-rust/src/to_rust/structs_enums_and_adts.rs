use std::ops::Deref;

use crate::grammar::{Enum, Fallible, Field, FieldName, Struct, Variant};

use super::{syntax, RustBuilder};

impl RustBuilder {
    pub fn lower_struct(&mut self, strukt: &Struct) -> Fallible<syntax::StructItem> {
        self.with_binder(&strukt.binder, |term, pp| {
            let generics =
                pp.lower_generics_for_binder(strukt.binder.kinds(), &term.where_clauses, false)?;
            let fields = term
                .fields
                .iter()
                .map(|field| pp.lower_named_struct_field(field))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(syntax::StructItem {
                name: strukt.id.deref().clone(),
                generics,
                fields,
            })
        })
    }

    pub fn lower_enum(&mut self, e: &Enum) -> Fallible<syntax::EnumItem> {
        self.with_binder(&e.binder, |term, pp| {
            let generics =
                pp.lower_generics_for_binder(e.binder.kinds(), &term.where_clauses, false)?;
            let variants = term
                .variants
                .iter()
                .map(|variant| pp.lower_variant(variant))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(syntax::EnumItem {
                name: e.id.deref().clone(),
                generics,
                variants,
            })
        })
    }

    fn lower_variant(&mut self, variant: &Variant) -> Fallible<syntax::EnumVariant> {
        let fields = self.lower_variant_fields(&variant.fields)?;
        Ok(syntax::EnumVariant {
            name: variant.name.deref().clone(),
            fields,
        })
    }

    fn lower_variant_fields(&mut self, fields: &[Field]) -> Fallible<syntax::VariantFields> {
        if fields.is_empty() {
            return Ok(syntax::VariantFields::Unit);
        }

        if matches!(fields[0].name, FieldName::Index(_)) {
            let tys = fields
                .iter()
                .map(|field| self.lower_ty(&field.ty))
                .collect::<Result<Vec<_>, _>>()?;
            return Ok(syntax::VariantFields::Tuple(tys));
        }

        let named = fields
            .iter()
            .map(|field| self.lower_named_struct_field(field))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(syntax::VariantFields::Struct(named))
    }

    fn lower_named_struct_field(&mut self, field: &Field) -> Fallible<syntax::StructField> {
        match &field.name {
            FieldName::Id(id) => Ok(syntax::StructField {
                name: id.deref().clone(),
                ty: self.lower_ty(&field.ty)?,
            }),
            FieldName::Index(idx) => anyhow::bail!(
                "expected named field but found tuple field index `{idx}` while lowering struct"
            ),
        }
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
            r#"
struct Bar {
    a: i32,
    b: i32,
}
"#
        );
    }

    #[test]
    fn struct_with_where_implemented() {
        crate::assert_rust!(
            [
                crate Foo {
                    trait Baz { }
                    struct Bar<T>
                        where
                        T: Baz
                    {
                        a: T,
                    }
                }
            ],
            r#"
trait Baz { }

struct Bar<T1> where T1: Baz {
    a: T1,
}
"#
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
            r#"
enum Bar {
    A,
    B,
}
"#
        );
    }

    #[test]
    fn enum_implemented() {
        crate::assert_rust!(
            [
                crate Foo {
                    trait Baz { }
                    enum Bar<T>
                        where
                        T : Baz
                    {
                        A { t: T },
                        B { 0: T },
                    }
                }
            ],
            r#"
trait Baz { }

enum Bar<T1> where T1: Baz {
    A { t: T1 },
    B(T1),
}
"#
        );
    }
}
