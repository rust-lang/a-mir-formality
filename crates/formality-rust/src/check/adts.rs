use std::collections::HashSet;

use crate::check::{prove_goal, where_clauses::prove_where_clauses_well_formed};
use crate::grammar::Fallible;
use crate::grammar::{Adt, AdtBoundData, AdtItem, AtomicPredicate, Field, Predicates, Variant};
use crate::prove::{enter_universally, Env, Program};
use anyhow::bail;
use formality_core::judgment::ProofTree;
use formality_core::judgment_fn;

judgment_fn! {
    pub(super) fn check_adt(
        program: Program,
        adt_item: AdtItem,
    ) => () {
        debug(adt_item)
        (
            (let adt = adt_item.to_adt())
            (check_adt_variant_names_unique(adt) => ())
            (scope(enter_universally(Env::default(), &adt.binder) => (env, AdtBoundData { where_clauses, variants })) with()
                (prove_where_clauses_well_formed(program, env, where_clauses, where_clauses) => ())
                (for_all(variant in variants)
                    (let Variant { fields, .. } = variant)
                    (for_all(field in fields)
                        (let Field { ty, .. } = field)
                        (prove_goal(program, env, where_clauses, AtomicPredicate::well_formed(ty)) => ()))
                    (check_fields_sized(program, env, where_clauses, adt_item, fields) => ())))
            ------------------------------------------------------------ ("check adt")
            (check_adt(program, adt_item) => ())
        )
    }
}

judgment_fn! {
    /// Only the last field of a struct may be unsized, and every enum field must be sized.
    /// `prove_sized` relies on this.
    fn check_fields_sized(
        program: Program,
        env: Env,
        assumptions: Predicates,
        adt_item: AdtItem,
        fields: Vec<Field>,
    ) => () {
        debug(fields, assumptions, env)

        (
            (for_all(field in &fields[..fields.len().saturating_sub(1)])
                (prove_goal(program, env, assumptions, AtomicPredicate::sized(&field.ty)) => ()))
            ------------------------------------------------------------ ("struct")
            (check_fields_sized(program, env, assumptions, AdtItem::Struct(_), fields) => ())
        )

        (
            (for_all(field in fields)
                (prove_goal(program, env, assumptions, AtomicPredicate::sized(&field.ty)) => ()))
            ------------------------------------------------------------ ("enum")
            (check_fields_sized(program, env, assumptions, AdtItem::Enum(_), fields) => ())
        )
    }
}

fn check_adt_variant_names_unique(adt: &Adt) -> Fallible<ProofTree> {
    // names is used to check that there are no name conflicts
    let mut names = HashSet::new();
    for Variant { name, fields } in &adt.binder.peek().variants {
        if !names.insert((name, None)) {
            bail!("variant \"{name:?}\" defined multiple times");
        }
        let vname = name;
        for Field { name, ty: _ } in fields {
            if !names.insert((vname, Some(name))) {
                bail!("field \"{name:?}\" of variant \"{vname:?}\" defined multiple times");
            }
        }
    }

    Ok(ProofTree::leaf(
        "check_adt_names_unique: all variant and field names unique",
    ))
}
