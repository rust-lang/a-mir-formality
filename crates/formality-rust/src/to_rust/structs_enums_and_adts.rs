use std::ops::Deref;

use crate::grammar::{Enum, Fallible, Field, FieldName, Struct, Variant};

use crate::to_rust::{
    context::{open_bounded, Context},
    syntax, tys,
};

pub fn lower_struct(ctx: &mut Context, strukt: &Struct) -> Fallible<syntax::StructItem> {
    let (term, generics) = open_bounded!(ctx, &strukt.binder);
    let fields = term
        .fields
        .iter()
        .map(|field| lower_named_struct_field(ctx, field))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(syntax::StructItem {
        name: strukt.id.deref().clone(),
        generics,
        fields,
    })
}

pub fn lower_enum(ctx: &mut Context, e: &Enum) -> Fallible<syntax::EnumItem> {
    let (term, generics) = open_bounded!(ctx, &e.binder);
    let variants = term
        .variants
        .iter()
        .map(|variant| lower_variant(ctx, variant))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(syntax::EnumItem {
        name: e.id.deref().clone(),
        generics,
        variants,
    })
}

pub fn lower_variant(ctx: &mut Context, variant: &Variant) -> Fallible<syntax::EnumVariant> {
    let fields = lower_variant_fields(ctx, &variant.fields)?;
    Ok(syntax::EnumVariant {
        name: variant.name.deref().clone(),
        fields,
    })
}

pub fn lower_variant_fields(
    ctx: &mut Context,
    fields: &[Field],
) -> Fallible<syntax::VariantFields> {
    if fields.is_empty() {
        return Ok(syntax::VariantFields::Unit);
    }

    if matches!(fields[0].name, FieldName::Index(_)) {
        let tys = fields
            .iter()
            .map(|field| tys::lower_ty(ctx, &field.ty))
            .collect::<Result<Vec<_>, _>>()?;
        return Ok(syntax::VariantFields::Tuple(tys));
    }

    let named = fields
        .iter()
        .map(|field| lower_named_struct_field(ctx, field))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(syntax::VariantFields::Struct(named))
}

pub fn lower_named_struct_field(ctx: &mut Context, field: &Field) -> Fallible<syntax::StructField> {
    match &field.name {
        FieldName::Id(id) => Ok(syntax::StructField {
            name: id.deref().clone(),
            ty: tys::lower_ty(ctx, &field.ty)?,
        }),
        FieldName::Index(idx) => anyhow::bail!(
            "expected named field but found tuple field index `{idx}` while lowering struct"
        ),
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
