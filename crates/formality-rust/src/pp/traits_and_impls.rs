use std::fmt::{self, Write};
use std::ops::Deref;

use itertools::Itertools;

use crate::grammar::{
    AssociatedTy, ImplItem, NegTraitImpl, Trait, TraitImpl, TraitItem, WhereClause, WhereClauseData,
};
use crate::pp::PrettyPrinter;
use crate::prove::prove::Safety;

impl PrettyPrinter {
    pub fn print_trait(&mut self, t: &Trait) -> String {
        let data = t.binder.explicit_binder.peek();

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
            .join(", ");

        format!("{safety}trait {id} {wc} {{ {items} }}")
    }

    pub fn print_assoc_ty(&mut self, assoc_ty: &AssociatedTy) -> String {
        let data = assoc_ty.binder.peek();
        let id = assoc_ty.id.deref();

        // Bounds := :Bar + Sized
        // Bounds := <empty string>
        let bound = if data.ensures.is_empty() {
            "".into()
        } else {
            format!(
                ": {}",
                data.ensures.iter().map(|b| format!("{b:?}")).join(" + ")
            )
        };

        // Where := where Bar, Baz
        let wc = data.where_clauses.iter().map(|w| match w.data() {
            WhereClauseData::IsImplemented(ty, trait_id, _parameters) => {
                (self.pretty_print_type(ty), trait_id.deref())
            }
            WhereClauseData::AliasEq(_alias_ty, _ty) => todo!(),
            WhereClauseData::Outlives(_parameter, _lt) => todo!(),
            WhereClauseData::ForAll(_core_binder) => todo!(),
            WhereClauseData::TypeOfConst(_, _ty) => todo!(),
        });

        let mut sep = " where ";
        let mut buffer_params = String::new();
        let mut buffer_where = String::new();
        // for bound in &data.where_clauses {
        //     match bound.data() {
        //         WhereClauseData::IsImplemented(ty, trait_id, _parameters) => {
        //             write!(buffer_params, "{:?}", ty)?; // FIXME: How to get a nice type?
        //             write!(buffer_where, "{sep}{}", trait_id.deref())?;
        //         }
        //         WhereClauseData::AliasEq(_alias_ty, _ty) => todo!(),
        //         WhereClauseData::Outlives(_parameter, _lt) => todo!(),
        //         WhereClauseData::ForAll(_core_binder) => todo!(),
        //         WhereClauseData::TypeOfConst(_, _ty) => todo!(),
        //     };
        // sep = ", ";
        // }

        let params = if buffer_params.is_empty() {
            buffer_params
        } else {
            format!("<{buffer_params}>")
        };

        format!("type {id}{params}{bound}{wc};",)
    }

    pub fn print_trait_impl(&mut self, trait_impl: &TraitImpl) -> String {
        if let Safety::Unsafe = trait_impl.safety {
            write!(w, "unsafe ")?;
        }

        let data = trait_impl.binder.peek();
        write!(w, "impl {} for {:?}", *data.trait_id, data.self_ty)?;
        print_where(w, &data.where_clauses)?;

        writeln!(w, " {{")?;
        for item in &data.impl_items {
            write!(w, "    ")?;
            match item {
                ImplItem::Fn(f) => print_fn(w, f)?,
                ImplItem::AssociatedTyValue(_) => todo!(),
            }
        }
        write!(w, "}}")
    }

    pub fn print_neg_trait_impl(&mut self, neg_trait_impl: &NegTraitImpl) -> String {
        if let Safety::Unsafe = neg_trait_impl.safety {
            write!(w, "unsafe ")?;
        }

        let data = neg_trait_impl.binder.peek();
        write!(w, "impl !{} for {:?}", *data.trait_id, data.self_ty)?;
        print_where(w, &data.where_clauses)?;
        write!(w, " {{}}")
    }

    /// Prints a where clauses according to the [this](https://doc.rust-lang.org/reference/items/generics.html#where-clauses)
    pub fn print_where(&mut self, where_clauses: &Vec<WhereClause>) -> String {
        if where_clauses.is_empty() {
            return Ok(());
        }

        write!(w, "<")?;
        let mut sep = "";
        for item in where_clauses.iter() {
            match &*item.data {
                WhereClauseData::IsImplemented(ty, _, _) => {
                    write!(w, "{}{:?}", sep, ty)?;
                }
                WhereClauseData::AliasEq(_, _) => todo!(),
                WhereClauseData::Outlives(_, _) => todo!(),
                WhereClauseData::ForAll(_) => todo!(),
                WhereClauseData::TypeOfConst(konst, ty) => {
                    dbg!(konst);
                    write!(w, "const {:?}: {:?}", konst, ty)?;
                }
            };
            sep = ", ";
        }
        writeln!(w, ">\nwhere")?;

        for item in where_clauses.iter() {
            match &*item.data {
                WhereClauseData::IsImplemented(ty, trait_id, params) => {
                    write!(w, "{:?}: {:?}", ty, trait_id)?;
                    if params.is_empty() {
                        writeln!(w, ",")?;
                    } else {
                        let params = params
                            .iter()
                            .map(|p| format!("{:?}", p))
                            .collect::<Vec<_>>()
                            .join(", ");
                        writeln!(w, "<{params}>,")?;
                    }
                }
                WhereClauseData::AliasEq(_, _) => todo!(),
                WhereClauseData::Outlives(_, _) => todo!(),
                WhereClauseData::ForAll(_) => todo!(),
                WhereClauseData::TypeOfConst(_, _) => continue,
            };
        }

        Ok(())
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
                        fn write() -> i32 0 _ i32
                    }
                }
            ],
            impl Write for Bar {
                fn write() -> i32 {
                    0
                }
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
        fn t(buffer: &mut String, term: crate::grammar::Trait) {
            print_trait(buffer, &term).unwrap();
        }

        crate::assert_rust2!(
            [trait Foo where T: Bar {}],
            t,
            "trait Foo<T> where T: Bar, { }"
        );
    }

    #[test]
    fn where_is_implemented_with_params() {
        fn t(buffer: &mut String, term: crate::grammar::Trait) {
            print_trait(buffer, &term).unwrap();
        }
        crate::assert_rust2!(
            [trait Foo where T: Bar<i32, String> {}],
            t,
            "trait Foo<T> where T: Bar<i32, String>, { }"
        );
    }

    #[test]
    #[ignore = "figure out how to get const variable names"]
    fn where_type_of_const() {
        fn t(buffer: &mut String, term: crate::grammar::Trait) {
            print_trait(buffer, &term).unwrap()
        }
        crate::assert_rust2!(
            [trait Foo<const C> where type_of_const C is bool {}],
            t,
            "trait Foo<const C: bool> {}"
        );
    }
}
