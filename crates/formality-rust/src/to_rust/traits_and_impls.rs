use std::{fmt::Write, ops::Deref};

use itertools::Itertools;

use crate::grammar::{
    AssociatedTy, Fallible, ImplItem, NegTraitImpl, Parameter, Trait, TraitImpl, TraitItem, TyData,
    WhereClause, WhereClauseData,
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
            pp.write_generic_params(out, &term.where_clauses)?;
            pp.write_where_bounds(out, &term.where_clauses)?;
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

    pub fn write_trait_impl(
        &mut self,
        out: &mut CodeWriter,
        trait_impl: &TraitImpl,
    ) -> Fallible<()> {
        // Ok(format!("{safety}impl<params> {id} for {ty} where {bounds} {{ {items} }}"))
        self.with_binder(&trait_impl.binder, |term, pp| {
            if let Safety::Unsafe = trait_impl.safety {
                write!(out, "unsafe ")?;
            }

            let id = term.trait_id.deref();
            let ty = pp.ty_to_string(&term.self_ty)?;
            write!(out, "impl {id}")?;
            pp.write_impl_trait_params(out, &term.trait_parameters)?;
            writeln!(out, " for {ty} ")?;
            pp.write_where_bounds(out, &term.where_clauses)?;
            writeln!(out, " {{")?;

            for item in &term.impl_items {
                match item {
                    ImplItem::Fn(f) => pp.write_fn(out, f)?,
                    ImplItem::AssociatedTyValue(_) => todo!(),
                }
            }

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

    pub fn write_where_bounds(
        &mut self,
        out: &mut CodeWriter,
        where_clauses: &Vec<WhereClause>,
    ) -> Fallible<()> {
        if where_clauses.is_empty() {
            return Ok(());
        }

        let mut buffer = String::new();
        let mut sep = "";
        for bound in where_clauses {
            write!(buffer, "{sep}")?;
            sep = ", ";
            match bound.data() {
                WhereClauseData::IsImplemented(ty, trait_id, params) => {
                    let ty = self.ty_to_string(ty)?;
                    let trait_id = trait_id.deref();
                    write!(buffer, "{ty}: {trait_id}")?;

                    if params.is_empty() {
                        continue;
                    }

                    write!(buffer, "<")?;
                    let mut sep = "";
                    for param in params {
                        let param = self.parameter_to_string(param)?;
                        write!(buffer, "{sep}{param}")?;
                        sep = ", ";
                    }
                    write!(buffer, ">")?;
                }
                WhereClauseData::AliasEq(_, _) => todo!(),
                WhereClauseData::Outlives(_, _) => todo!(),
                WhereClauseData::ForAll(_) => todo!(),
                WhereClauseData::TypeOfConst(_, _) => continue,
            }
        }

        if !buffer.is_empty() {
            write!(out, " where {buffer}")?;
        }
        Ok(())
    }

    fn write_impl_trait_params(
        &mut self,
        out: &mut CodeWriter,
        trait_parameters: &Vec<Parameter>,
    ) -> Fallible<()> {
        if trait_parameters.is_empty() {
            return Ok(());
        }

        write!(out, "<")?;
        let mut sep = "";
        for parameter in trait_parameters {
            let p = self.parameter_to_string(parameter)?;
            write!(out, "{sep}{}", p)?;
            sep = ", "
        }

        write!(out, ">")?;
        Ok(())
    }

    fn write_assoc_ty(&mut self, out: &mut CodeWriter, assoc_ty: &AssociatedTy) -> Fallible<()> {
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
            let mut buffer = String::new();
            let mut sep = "";
            for wc in &term.where_clauses {
                match wc.data() {
                    WhereClauseData::IsImplemented(ty, trait_id, _parameters) => {
                        let ty = pp.ty_to_string(ty)?;
                        // TODO: parameters are probably needed as well?
                        write!(buffer, "{sep}{ty}: {}", trait_id.deref())?;
                    }
                    WhereClauseData::AliasEq(_alias_ty, _ty) => todo!(),
                    WhereClauseData::Outlives(_parameter, _lt) => todo!(),
                    WhereClauseData::ForAll(_core_binder) => todo!(),
                    WhereClauseData::TypeOfConst(_, _ty) => todo!(),
                }
                sep = ", ";
            }

            if !buffer.is_empty() {
                write!(out, " where {buffer}")?;
            }
            writeln!(out, ";")?;
            Ok(())
        })
    }

    pub fn write_generic_params(
        &mut self,
        out: &mut CodeWriter,
        where_clauses: &Vec<WhereClause>,
    ) -> Fallible<()> {
        if where_clauses.is_empty() {
            return Ok(());
        }

        write!(out, "<")?;
        let mut sep = "";
        for param in where_clauses {
            write!(out, "{sep}")?;
            match param.data() {
                WhereClauseData::IsImplemented(ty, _, _) => {
                    if let TyData::Variable(var) = ty.data() {
                        let var = self.core_variable_to_string(var)?;
                        write!(out, "{var}")?;
                    } else {
                        continue;
                    }
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
            sep = ", ";
        }
        write!(out, ">")?;
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

    #[test]
    fn simple_trait_impl() {
        crate::assert_rust!(
            [
                crate Foo {
                    impl Bar<T> for Baz where T: Bur {
                        fn run() -> T {trusted}
                    }
                }
            ],
            impl Bar<T> for Baz where T: Bur {
                fn run() -> T {
                    panic!("Trusted Fn Body")
                }
            }
        );
    }

    #[test]
    fn trait_impl_bounds() {
        crate::assert_rust!(
            [
                crate Foo {
                    impl Bar for Baz {
                        fn run() -> i32 {trusted}
                    }
                }
            ],
            impl Bar for Baz {
                fn run() -> i32 {
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
}
