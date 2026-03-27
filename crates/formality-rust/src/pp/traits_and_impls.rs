use std::ops::Deref;

use itertools::Itertools;

use crate::grammar::{
    AssociatedTy, Fallible, ImplItem, NegTraitImpl, Trait, TraitImpl, TraitItem, WhereClause,
    WhereClauseData,
};
use crate::pp::PrettyPrinter;
use crate::prove::prove::Safety;

impl PrettyPrinter {
    pub fn print_trait(&mut self, t: &Trait) -> Fallible<String> {
        self.with_binder(&t.binder.explicit_binder, |term, pp| {
            let safety = if let Safety::Unsafe = t.safety {
                "unsafe "
            } else {
                ""
            };

            let id = t.id.deref();
            let wc = pp.print_where(&term.where_clauses)?;

            let items = term
                .trait_items
                .iter()
                .map(|i| match i {
                    TraitItem::Fn(f) => pp.print_fn(f),
                    TraitItem::AssociatedTy(assoc_ty) => pp.print_assoc_ty(assoc_ty),
                })
                .collect::<Result<Vec<_>, _>>()?
                .join("\n");

            Ok(format!("{safety}trait {id}{wc} {{ {items} }}"))
        })
    }

    pub fn print_assoc_ty(&mut self, assoc_ty: &AssociatedTy) -> Fallible<String> {
        self.with_binder(&assoc_ty.binder, |term, pp| {
            let id = assoc_ty.id.deref();

            let params = term
                .where_clauses
                .iter()
                .map(|w| match w.data() {
                    WhereClauseData::IsImplemented(ty, _trait_id, _parameters) => {
                        pp.pretty_print_type(ty)
                    }
                    _ => unimplemented!(),
                })
                .collect::<Result<Vec<_>, _>>()?
                .join(", ");

            // Bounds := :Bar + Sized
            // Bounds := <empty string>
            let bound = if term.ensures.is_empty() {
                "".into()
            } else {
                format!(
                    ": {}",
                    term.ensures.iter().map(|b| format!("{b:?}")).join(" + ")
                )
            };

            // Where := where Bar, Baz
            let wc = term
                .where_clauses
                .iter()
                .map(|w| match w.data() {
                    WhereClauseData::IsImplemented(ty, trait_id, _parameters) => pp
                        .pretty_print_type(ty)
                        .map(|ty| format!("{}: {}", ty, trait_id.deref())),
                    WhereClauseData::AliasEq(_alias_ty, _ty) => todo!(),
                    WhereClauseData::Outlives(_parameter, _lt) => todo!(),
                    WhereClauseData::ForAll(_core_binder) => todo!(),
                    WhereClauseData::TypeOfConst(_, _ty) => todo!(),
                })
                .collect::<Result<Vec<_>, _>>()?
                .join(", ");

            let wc = if wc.is_empty() {
                wc
            } else {
                format!(" where {wc}")
            };
            let params = if params.is_empty() {
                params
            } else {
                format!("<{params}>")
            };

            Ok(format!("type {id}{params}{bound}{wc};",))
        })
    }

    pub fn print_trait_impl(&mut self, trait_impl: &TraitImpl) -> Fallible<String> {
        self.with_binder(&trait_impl.binder, |term, pp| {
            let safety = if let Safety::Unsafe = trait_impl.safety {
                "unsafe "
            } else {
                ""
            };
            let id = term.trait_id.deref();
            let ty = pp.pretty_print_type(&term.self_ty)?;
            // let where = print_where(w, &data.where_clauses)?;
            let items = term
                .impl_items
                .iter()
                .map(|item| match item {
                    ImplItem::Fn(f) => pp.print_fn(f),
                    ImplItem::AssociatedTyValue(_) => todo!(),
                })
                .collect::<Result<Vec<_>, _>>()?
                .join("\n");

            Ok(format!("{safety}impl {id} for {ty} {{ {items} }}"))
        })
    }

    pub fn print_neg_trait_impl(&mut self, neg_trait_impl: &NegTraitImpl) -> Fallible<String> {
        self.with_binder(&neg_trait_impl.binder, |term, pp| {
            let safety = if let Safety::Unsafe = neg_trait_impl.safety {
                "unsafe "
            } else {
                ""
            };
            let id = term.trait_id.deref();
            let ty = pp.pretty_print_type(&term.self_ty)?;
            // let wc = self.print_where(&data.where_clauses);

            Ok(format!("{safety}impl !{id} for {ty} {{}}"))
        })
    }

    /// Prints a where clauses according to the [this](https://doc.rust-lang.org/reference/items/generics.html#where-clauses)
    pub fn print_where(&mut self, where_clauses: &Vec<WhereClause>) -> Fallible<String> {
        if where_clauses.is_empty() {
            return Ok("".into());
        }

        let params = where_clauses
            .iter()
            .map(|i| match i.data() {
                WhereClauseData::IsImplemented(ty, _, _) => self.pretty_print_type(ty),
                WhereClauseData::AliasEq(_, _) => todo!(),
                WhereClauseData::Outlives(_, _) => todo!(),
                WhereClauseData::ForAll(_) => todo!(),
                WhereClauseData::TypeOfConst(konst, ty) => self
                    .pretty_print_const(konst)
                    .and_then(|konst| self.pretty_print_type(ty).map(|ty| (konst, ty)))
                    .map(|(konst, ty)| format!("const {konst}: {ty}",)),
            })
            .collect::<Result<Vec<_>, _>>()?
            .join(", ");

        let bounds = where_clauses
            .iter()
            .filter_map(|i| match i.data() {
                WhereClauseData::IsImplemented(ty, trait_id, params) => {
                    let params = if params.is_empty() {
                        "".into()
                    } else {
                        let params = params
                            .iter()
                            .map(|p| format!("{:?}", p))
                            .collect::<Vec<_>>()
                            .join(", ");
                        format!("<{params}>")
                    };

                    Some(
                        self.pretty_print_type(ty)
                            .map(|ty| format!("{}: {}{params}", ty, trait_id.deref())),
                    )
                }
                WhereClauseData::AliasEq(_, _) => todo!(),
                WhereClauseData::Outlives(_, _) => todo!(),
                WhereClauseData::ForAll(_) => todo!(),
                WhereClauseData::TypeOfConst(_, _) => None,
            })
            .collect::<Result<Vec<_>, _>>()?
            .join("\n");

        Ok(if bounds.is_empty() {
            format!("<{params}>")
        } else {
            format!("<{params}> where {bounds}")
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

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
            trait Write {
                fn test() -> i32;
            }
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
            trait Write {
                type Error;
                fn test() -> i32;
            }
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
            trait Write {
                type Error<T2, T3>: Sized + Bar where T2: Read, T3: Write;
                fn test() -> i32;
            }
        );
    }

    #[test]
    #[ignore]
    fn simple_trait_impl() {
        crate::assert_rust!(
            [
                crate Foo {
                    impl Write for Bar {
                        fn write() -> i32 trusted;
                    }
                }
            ],
            impl Write for Bar {
                fn write() -> i32 {
                    panic!("Trusted Fn Body")
                };
            }
        );
    }

    #[test]
    fn simple_neg_trait_impl() {
        crate::assert_rust!(
            [
                crate Foo {
                    impl !Write for Bar { }
                }
            ],
            impl !Write for Bar { }
        );
    }

    #[test]
    fn where_is_implemented() {
        fn t(term: crate::grammar::Trait) -> String {
            PrettyPrinter::default().print_trait(&term).unwrap()
        }

        crate::assert_rust2!(
            [trait Foo where T: Bar {}],
            t,
            "trait Foo<T> where T: Bar { }"
        );
    }

    #[test]
    fn where_is_implemented_with_params() {
        fn t(term: crate::grammar::Trait) -> String {
            PrettyPrinter::default().print_trait(&term).unwrap()
        }
        crate::assert_rust2!(
            [trait Foo where T: Bar<i32, String> {}],
            t,
            "trait Foo<T> where T: Bar<i32, String> { }"
        );
    }

    #[test]
    fn trait_assoc_type() {
        fn t(term: crate::grammar::Trait) -> String {
            PrettyPrinter::default().print_trait(&term).unwrap()
        }
        crate::assert_rust2!(
            [trait Foo where K: Bar { type Error: []; fn test() -> K; }],
            t,
            "trait Foo<K> where K: Bar { type Error; fn test() -> K; }"
        );
    }

    #[test]
    fn where_type_of_const() {
        fn t(term: crate::grammar::Trait) -> String {
            PrettyPrinter::default().print_trait(&term).unwrap()
        }
        crate::assert_rust2!(
            [trait Foo<const C> where type_of_const C is bool {}],
            t,
            "trait Foo<const N1: bool> { }"
        );
    }
}
