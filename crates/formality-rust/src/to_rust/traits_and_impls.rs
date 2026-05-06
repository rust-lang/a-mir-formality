use std::ops::Deref;

use crate::grammar::{
    AssociatedTy, AssociatedTyValue, Fallible, ImplItem, NegTraitImpl, Trait, TraitImpl, TraitItem,
    WhereBoundData,
};
use crate::prove::prove::Safety;

use crate::to_rust::{self, fns, syntax, tys, Context};

pub fn lower_trait(ctx: &mut Context, t: &Trait) -> Fallible<syntax::TraitItem> {
    // NOTE: Is this right with explicit binder?
    ctx.with_binder(
        &t.binder.explicit_binder,
        true,
        |term| &term.where_clauses,
        |term, generics, ctx| {
            let mut items = Vec::new();
            for item in &term.trait_items {
                match item {
                    TraitItem::Fn(f) => {
                        items.push(syntax::TraitMember::Function(fns::lower_fn(ctx, f)?))
                    }
                    TraitItem::AssociatedTy(assoc_ty) => items.push(
                        syntax::TraitMember::AssociatedType(lower_assoc_ty(ctx, assoc_ty)?),
                    ),
                }
            }

            Ok(syntax::TraitItem {
                is_unsafe: matches!(t.safety, Safety::Unsafe),
                name: t.id.deref().clone(),
                generics,
                items,
            })
        },
    )
}

pub fn lower_trait_impl(ctx: &mut Context, trait_impl: &TraitImpl) -> Fallible<syntax::ImplItem> {
    ctx.with_binder(
        &trait_impl.binder,
        false,
        |term| &term.where_clauses,
        |term, generics, ctx| {
            let mut items = Vec::new();
            for item in &term.impl_items {
                match item {
                    ImplItem::Fn(f) => {
                        items.push(syntax::ImplMember::Function(fns::lower_fn(ctx, f)?))
                    }
                    ImplItem::AssociatedTyValue(v) => {
                        items.push(syntax::ImplMember::AssociatedTypeValue(
                            lower_assoc_ty_value(ctx, v)?,
                        ));
                    }
                }
            }

            let trait_args = term
                .trait_parameters
                .iter()
                .map(|arg| tys::lower_generic_arg(ctx, arg))
                .collect::<Result<Vec<_>, _>>()?;
            let self_ty = tys::lower_ty(ctx, &term.self_ty)?;

            Ok(syntax::ImplItem {
                is_unsafe: matches!(trait_impl.safety, Safety::Unsafe),
                generics,
                trait_name: term.trait_id.deref().clone(),
                trait_args,
                self_ty,
                items,
            })
        },
    )
}

pub fn lower_neg_trait_impl(
    ctx: &mut Context,
    neg_trait_impl: &NegTraitImpl,
) -> Fallible<syntax::NegImplItem> {
    ctx.with_binder(
        &neg_trait_impl.binder,
        false,
        |term| &term.where_clauses,
        |term, generics, ctx| {
            let trait_args = term
                .trait_parameters
                .iter()
                .map(|arg| tys::lower_generic_arg(ctx, arg))
                .collect::<Result<Vec<_>, _>>()?;
            let self_ty = tys::lower_ty(ctx, &term.self_ty)?;
            let where_clauses = to_rust::lower_where_clauses(ctx, &term.where_clauses)?;

            Ok(syntax::NegImplItem {
                is_unsafe: matches!(neg_trait_impl.safety, Safety::Unsafe),
                generics,
                trait_name: term.trait_id.deref().clone(),
                trait_args,
                self_ty,
                where_clauses,
            })
        },
    )
}

pub fn lower_assoc_ty(
    ctx: &mut Context,
    assoc_ty: &AssociatedTy,
) -> Fallible<syntax::AssociatedTypeItem> {
    ctx.with_binder(
        &assoc_ty.binder,
        false,
        |term| &term.where_clauses,
        |term, generics, ctx| {
            let mut bounds = Vec::new();
            for ensure in &term.ensures {
                match ensure {
                    WhereBoundData::IsImplemented(trait_id, parameters) => {
                        bounds.push(syntax::TypeBound::Trait {
                            trait_name: trait_id.deref().clone(),
                            args: parameters
                                .iter()
                                .map(|arg| tys::lower_generic_arg(ctx, arg))
                                .collect::<Result<Vec<_>, _>>()?,
                        });
                    }
                    WhereBoundData::Outlives(_) => {
                        anyhow::bail!(
                            "lowering associated type outlives bounds is not implemented yet"
                        )
                    }
                    WhereBoundData::ForAll(_) => {
                        anyhow::bail!(
                            "lowering associated type `for` bounds is not implemented yet"
                        )
                    }
                }
            }

            Ok(syntax::AssociatedTypeItem {
                name: assoc_ty.id.deref().clone(),
                generics,
                bounds,
            })
        },
    )
}

pub fn lower_assoc_ty_value(
    ctx: &mut Context,
    assoc_ty: &AssociatedTyValue,
) -> Fallible<syntax::AssociatedTypeValueItem> {
    ctx.with_binder(
        &assoc_ty.binder,
        false,
        |term| &term.where_clauses,
        |term, generics, ctx| {
            let ty = tys::lower_ty(ctx, &term.ty)?;
            Ok(syntax::AssociatedTypeValueItem {
                name: assoc_ty.id.deref().clone(),
                generics,
                ty,
            })
        },
    )
}

#[cfg(test)]
mod test {

    #[test]
    fn simple_trait() {
        crate::assert_rust!(
            [
                crate Foo {
                    trait Write {
                        fn test() -> i32;
                    }
                }
            ],
            r#"
trait Write {
    fn test() -> i32;
}
"#
        );
    }

    #[test]
    fn simple_trait_with_associated_ty() {
        crate::assert_rust!(
            [
                crate Foo {
                    trait Write {
                        type Error: [];
                        fn test() -> i32;
                    }
                }
            ],
            r#"
trait Write {
    type Error;
    fn test() -> i32;
}
"#
        );
    }

    #[test]
    fn simple_trait_with_associated_ty2() {
        crate::assert_rust!(
            [
                crate Foo {
                    trait Write {
                        type Error<T, K>: [Sized, Bar] where T: Read, K: Write;
                        fn test() -> i32;
                    }
                }
            ],
            r#"
trait Write {
    type Error<T1, T2>: Sized + Bar where T1: Read, T2: Write;
    fn test() -> i32;
}
"#
        );
    }

    #[test]
    fn where_is_implemented() {
        crate::assert_rust!(
            [
                crate Foo {
                    trait Baz { }
                    trait Bar<T> where T: Baz {}
                }
            ],
            "
trait Baz { }

trait Bar<T2> where T2: Baz { }
"
        );
    }

    #[test]
    fn where_is_implemented_with_params() {
        crate::assert_rust!(
            [
                crate Foo {
                    trait Baz<T, K> { }
                    trait Bar<V> where V: Baz<i32, u8> {}
                }
            ],
            "
trait Baz<T1, T2> { }

trait Bar<T4> where T4: Baz<i32, u8> { }
"
        );
    }

    #[test]
    fn trait_assoc_type() {
        crate::assert_rust!(
            [
                crate Foo {
                    trait Baz { }
                    trait Bar<K> where K: Baz {
                        type Error: [];
                        fn test() -> K;
                    }
                }
            ],
            r#"
trait Baz { }

trait Bar<T2> where T2: Baz {
    type Error;
    fn test() -> T2;
}
"#
        );
    }

    #[test]
    fn where_type_of_const() {
        crate::assert_rust!(
            [
                crate Foo {
                    trait Bar<const C> where type_of_const C is bool {}
                }
            ],
            "trait Bar<const N1: bool> { }"
        );
    }

    #[test]
    fn simple_trait_impl() {
        crate::assert_rust!(
            [
                crate Foo {
                    trait Bar<T> {
                        fn run() -> T;
                    }
                    trait Bur { }
                    struct Baz { }
                    impl<T> Bar<T> for Baz where T: Bur {
                        fn run() -> T {trusted}
                    }
                }
            ],
            r#"
trait Bar<T1> {
    fn run() -> T1;
}

trait Bur { }

struct Baz {}

impl<T3> Bar<T3> for Baz where T3: Bur {
    fn run() -> T3 {
        panic!("Trusted Fn Body")
    }
}"#
        );
    }

    #[test]
    fn trait_impl_bounds() {
        crate::assert_rust!(
            [
                crate Foo {
                    trait Bar { fn run() -> i32; }
                    struct Baz { }
                    impl Bar for Baz {
                        fn run() -> i32 {trusted}
                    }
                }
            ],
            r#"
trait Bar {
    fn run() -> i32;
}

struct Baz {}

impl Bar for Baz {
    fn run() -> i32 {
        panic!("Trusted Fn Body")
    }
}
"#
        );
    }

    #[test]
    fn simple_neg_trait_impl() {
        crate::assert_rust!(
            [
                crate Foo {
                    trait Bar { }
                    struct Baz { }
                    impl !Bar for Baz { }
                }
            ],
            r#"
trait Bar { }

struct Baz {}

impl !Bar for Baz {}
"#
        );
    }
}
