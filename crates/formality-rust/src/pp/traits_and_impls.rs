use std::ops::Deref;

use itertools::Itertools;

use crate::grammar::{
    AssociatedTy, ImplItem, NegTraitImpl, ParameterKind, Trait, TraitImpl, TraitItem, WhereClause,
    WhereClauseData,
};
use crate::pp::PrettyPrinter;
use crate::prove::prove::Safety;

impl PrettyPrinter {
    pub fn print_trait(&mut self, t: &Trait) -> String {
        let (bound, data) = t.binder.open();
        let mut ty = 0;
        let mut lt = 0;
        let mut c = 0;
        dbg!(t.binder.explicit_binder.kinds());
        dbg!(bound);
        t.binder
            .explicit_binder
            .kinds()
            .iter()
            .for_each(|k| match k {
                ParameterKind::Ty => ty += 1,
                ParameterKind::Lt => lt += 1,
                ParameterKind::Const => c += 1,
            });

        self.ctx.push_tys(ty);
        self.ctx.push_lts(lt);
        self.ctx.push_const(c);

        let safety = if let Safety::Unsafe = t.safety {
            "unsafe "
        } else {
            ""
        };

        let id = t.id.deref();
        let wc = self.print_where(&data.where_clauses);

        let items = data
            .trait_items
            .iter()
            .map(|i| match i {
                TraitItem::Fn(f) => self.print_fn(f),
                TraitItem::AssociatedTy(assoc_ty) => self.print_assoc_ty(assoc_ty),
            })
            .join("\n");

        self.ctx.pop_tys(ty);
        self.ctx.pop_lts(lt);
        self.ctx.pop_const(c);
        format!("{safety}trait {id}{wc} {{ {items} }}")
    }

    pub fn print_assoc_ty(&mut self, assoc_ty: &AssociatedTy) -> String {
        let (bound_vars, term) = assoc_ty.binder.open();
        dbg!(assoc_ty.binder.kinds());
        let id = assoc_ty.id.deref();

        let params = bound_vars
            .iter()
            .map(|v| self.ty_name(&formality_core::variable::CoreVariable::BoundVar(*v)))
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
                WhereClauseData::IsImplemented(_ty, trait_id, _parameters) => trait_id.deref(),
                WhereClauseData::AliasEq(_alias_ty, _ty) => todo!(),
                WhereClauseData::Outlives(_parameter, _lt) => todo!(),
                WhereClauseData::ForAll(_core_binder) => todo!(),
                WhereClauseData::TypeOfConst(_, _ty) => todo!(),
            })
            .join(", ");

        let wc = if wc.is_empty() {
            wc
        } else {
            format!("where {wc}")
        };
        let params = if params.is_empty() {
            params
        } else {
            format!("<{params}>")
        };

        format!("type {id}{params}{bound}{wc};",)
    }

    pub fn print_trait_impl(&mut self, trait_impl: &TraitImpl) -> String {
        let (_, data) = trait_impl.binder.open();

        let safety = if let Safety::Unsafe = trait_impl.safety {
            "unsafe "
        } else {
            ""
        };
        let id = data.trait_id.deref();
        let ty = self.pretty_print_type(&data.self_ty);
        // let where = print_where(w, &data.where_clauses)?;
        let items = data
            .impl_items
            .iter()
            .map(|item| match item {
                ImplItem::Fn(f) => self.print_fn(f),
                ImplItem::AssociatedTyValue(_) => todo!(),
            })
            .join("\n");

        format!("{safety}impl {id} for {ty} {{ {items} }}")
    }

    pub fn print_neg_trait_impl(&mut self, neg_trait_impl: &NegTraitImpl) -> String {
        let (_, data) = neg_trait_impl.binder.open();
        let safety = if let Safety::Unsafe = neg_trait_impl.safety {
            "unsafe "
        } else {
            ""
        };
        let id = data.trait_id.deref();
        let ty = self.pretty_print_type(&data.self_ty);
        // let wc = self.print_where(&data.where_clauses);

        format!("{safety}impl !{id} for {ty} {{}}")
    }

    /// Prints a where clauses according to the [this](https://doc.rust-lang.org/reference/items/generics.html#where-clauses)
    pub fn print_where(&mut self, where_clauses: &Vec<WhereClause>) -> String {
        if where_clauses.is_empty() {
            return "".into();
        }

        let params = where_clauses
            .iter()
            .map(|i| match i.data() {
                WhereClauseData::IsImplemented(ty, _, _) => self.pretty_print_type(ty),
                WhereClauseData::AliasEq(_, _) => todo!(),
                WhereClauseData::Outlives(_, _) => todo!(),
                WhereClauseData::ForAll(_) => todo!(),
                WhereClauseData::TypeOfConst(konst, ty) => {
                    format!(
                        "const {:?}: {}",
                        self.pretty_print_const(konst),
                        self.pretty_print_type(ty)
                    )
                }
            })
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

                    Some(format!(
                        "{}: {}{params}",
                        self.pretty_print_type(ty),
                        trait_id.deref()
                    ))
                }
                WhereClauseData::AliasEq(_, _) => todo!(),
                WhereClauseData::Outlives(_, _) => todo!(),
                WhereClauseData::ForAll(_) => todo!(),
                WhereClauseData::TypeOfConst(_, _) => None,
            })
            .join("\n");

        format!("<{params}> where {bounds}")
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
                        type Error<T>: [Sized, Bar] where T: Read;
                        fn test() -> i32;
                    }
                }
            ],
            trait Write {
                type Error<T>: Sized + Bar where T: Read;
                fn test() -> i32;
            }
        );
    }

    #[test]
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
            PrettyPrinter::default().print_trait(&term)
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
            PrettyPrinter::default().print_trait(&term)
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
            PrettyPrinter::default().print_trait(&term)
        }
        crate::assert_rust2!(
            [trait Foo where K: Bar { type Error: []; fn test() -> K; }],
            t,
            "trait Foo { type Error<T>; }"
        );
    }

    #[test]
    #[ignore = "figure out how to get const variable names"]
    fn where_type_of_const() {
        fn t(term: crate::grammar::Trait) -> String {
            PrettyPrinter::default().print_trait(&term)
        }
        crate::assert_rust2!(
            [trait Foo<const C> where type_of_const C is bool {}],
            t,
            "trait Foo<const C: bool> {}"
        );
    }
}
