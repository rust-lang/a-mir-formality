use std::{fmt::Write, ops::Deref};

use itertools::Itertools;

use crate::grammar::{
    AssociatedTy, Fallible, ImplItem, NegTraitImpl, Trait, TraitImpl, TraitItem, WhereClause,
    WhereClauseData,
};
use crate::prove::prove::Safety;
use crate::to_rust::{CodeWriter, RustBuilder};

impl RustBuilder {
    pub fn write_trait(&mut self, out: &mut CodeWriter, t: &Trait) -> Fallible<()> {
        self.with_binder(&t.binder.explicit_binder, |term, pp| {
            if let Safety::Unsafe = t.safety {
                write!(out, "unsafe ")?;
            }

            write!(out, "trait {}", t.id.deref())?;
            pp.write_where(out, &term.where_clauses)?;
            writeln!(out, " {{")?;

            for item in &term.trait_items {
                match item {
                    TraitItem::Fn(f) => pp.write_fn(out, f)?,
                    TraitItem::AssociatedTy(assoc_ty) => pp.write_assoc_ty(out, assoc_ty)?,
                }
            }

            writeln!(out, "}}")?;
            Ok(())
        })
    }

    pub fn write_assoc_ty(
        &mut self,
        out: &mut CodeWriter,
        assoc_ty: &AssociatedTy,
    ) -> Fallible<()> {
        self.with_binder(&assoc_ty.binder, |term, pp| {
            write!(out, "type {}", assoc_ty.id.deref())?;

            if !&term.where_clauses.is_empty() {
                write!(out, "<")?;
            }

            let mut sep = "";
            for param in &term.where_clauses {
                match param.data() {
                    WhereClauseData::IsImplemented(ty, _trait_id, _parameters) => {
                        let ty = pp.ty_to_string(ty)?;
                        write!(out, "{sep}{ty}")?;
                        sep = ", ";
                    }
                    _ => unimplemented!(),
                }
            }

            if !&term.where_clauses.is_empty() {
                write!(out, ">")?;
            }

            // Bounds := :Bar + Sized
            // Bounds := <empty string>
            if !term.ensures.is_empty() {
                write!(
                    out,
                    ": {}",
                    term.ensures.iter().map(|b| format!("{b:?}")).join(" + ")
                )?;
            };

            // Where := where Bar, Baz
            // TODO: write where
            let mut sep = "";
            let mut first = true;
            for wc in &term.where_clauses {
                if first {
                    write!(out, " where ")?;
                    first = false;
                }
                match wc.data() {
                    WhereClauseData::IsImplemented(ty, trait_id, _parameters) => {
                        let ty = pp.ty_to_string(ty)?;
                        // TODO: parameters are probably needed as well?
                        write!(out, "{sep}{ty}: {}", trait_id.deref())?;
                    }
                    WhereClauseData::AliasEq(_alias_ty, _ty) => todo!(),
                    WhereClauseData::Outlives(_parameter, _lt) => todo!(),
                    WhereClauseData::ForAll(_core_binder) => todo!(),
                    WhereClauseData::TypeOfConst(_, _ty) => todo!(),
                }
                sep = ", ";
            }

            writeln!(out, ";")?;
            Ok(())
        })
    }

    pub fn write_trait_impl(
        &mut self,
        out: &mut CodeWriter,
        trait_impl: &TraitImpl,
    ) -> Fallible<()> {
        self.with_binder(&trait_impl.binder, |term, pp| {
            if let Safety::Unsafe = trait_impl.safety {
                write!(out, "unsafe ")?;
            }

            let id = term.trait_id.deref();
            let ty = pp.ty_to_string(&term.self_ty)?;
            writeln!(out, "impl {id} for {ty} {{")?;
            // TODO: write where

            for item in &term.impl_items {
                match item {
                    ImplItem::Fn(f) => pp.write_fn(out, f)?,
                    ImplItem::AssociatedTyValue(_) => todo!(),
                }
            }

            // Ok(format!("{safety}impl {id} for {ty} {{ {items} }}"))
            writeln!(out, "}}")?;
            Ok(())
        })
    }

    pub fn write_neg_trait_impl(
        &mut self,
        out: &mut CodeWriter,
        neg_trait_impl: &NegTraitImpl,
    ) -> Fallible<()> {
        self.with_binder(&neg_trait_impl.binder, |term, pp| {
            if let Safety::Unsafe = neg_trait_impl.safety {
                write!(out, "unsafe ")?;
            }

            let id = term.trait_id.deref();
            let ty = pp.ty_to_string(&term.self_ty)?;
            // let wc = self.print_where(&data.where_clauses);
            writeln!(out, "impl !{id} for {ty} {{}}")?;
            Ok(())
        })
    }

    /// Prints a where clauses according to the [this](https://doc.rust-lang.org/reference/items/generics.html#where-clauses)
    pub fn write_where(
        &mut self,
        out: &mut CodeWriter,
        where_clauses: &Vec<WhereClause>,
    ) -> Fallible<()> {
        if where_clauses.is_empty() {
            return Ok(());
        }

        write!(out, "<")?;
        let mut has_bounds = false;
        for param in where_clauses {
            match param.data() {
                WhereClauseData::IsImplemented(ty, _, _) => {
                    has_bounds = true;
                    let ty = self.ty_to_string(ty)?;
                    write!(out, "{ty}")?;
                }
                WhereClauseData::AliasEq(_, _) => todo!(),
                WhereClauseData::Outlives(_, _) => todo!(),
                WhereClauseData::ForAll(_) => todo!(),
                WhereClauseData::TypeOfConst(konst, ty) => {
                    let konst = self.const_to_string(konst)?;
                    let ty = self.ty_to_string(ty)?;
                    write!(out, "const {konst}: {ty}")?;
                }
            }
        }
        write!(out, ">")?;

        // Condition
        if !has_bounds {
            return Ok(());
        }

        write!(out, " where ")?;
        for bound in where_clauses {
            match bound.data() {
                WhereClauseData::IsImplemented(ty, trait_id, params) => {
                    let ty = self.ty_to_string(ty)?;
                    let trait_id = trait_id.deref();
                    write!(out, "{ty}: {trait_id}")?;

                    if params.is_empty() {
                        continue;
                    }

                    write!(out, "<")?;
                    let mut sep = "";
                    for param in params {
                        write!(out, "{sep}{param:?}")?; // TODO: param_to_string
                        sep = ", ";
                    }
                    write!(out, ">")?;
                }
                WhereClauseData::AliasEq(_, _) => todo!(),
                WhereClauseData::Outlives(_, _) => todo!(),
                WhereClauseData::ForAll(_) => todo!(),
                WhereClauseData::TypeOfConst(_, _) => continue,
            }
        }

        Ok(())
    }
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
    fn simple_trait_impl() {
        crate::assert_rust!(
            [
                crate Foo {
                    impl Write for Bar {
                        fn write() -> i32 {trusted}
                    }
                }
            ],
            impl Write for Bar {
                fn write() -> i32 {
                    panic!("Trusted Fn Body")
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
            "impl !Write for Bar {}"
        );
    }

    #[test]
    fn where_is_implemented() {
        crate::assert_rust!(
            [
                crate Foo {
                    trait Bar where T: Baz {}
                }
            ],
            "trait Bar<T> where T: Baz { }"
        );
    }

    #[test]
    fn where_is_implemented_with_params() {
        crate::assert_rust!(
            [
                crate Foo {
                    trait Bar where T: Baz<i32, String> {}
                }
            ],
            "trait Bar<T> where T: Baz<i32, String> { }"
        );
    }

    #[test]
    fn trait_assoc_type() {
        crate::assert_rust!(
            [
                crate Foo {
                    trait Bar where K: Baz {
                        type Error: [];
                        fn test() -> K;
                    }
                }
            ],
            "trait Bar<K> where K: Baz { type Error; fn test() -> K; }"
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
}
