use std::collections::HashSet;

use crate::check::{prove_goal, where_clauses::prove_where_clauses_well_formed};
use crate::grammar::Fallible;
use crate::grammar::{Adt, AdtBoundData, Field, Relation, Variant};
use crate::prove::{Env, Program};
use anyhow::bail;
use formality_core::judgment::ProofTree;
use formality_core::judgment_fn;

judgment_fn! {
    pub(super) fn check_adt(
        program: Program,
        adt: Adt,
    ) => () {
        debug(adt)
        (
            (check_adt_variant_names_unique(adt) => ())
            (let (env, bound_data) = Env::default().instantiate_universally(&adt.binder))
            (let AdtBoundData { where_clauses, variants } = bound_data)
            (prove_where_clauses_well_formed(program, env, where_clauses, where_clauses) => ())
            (for_all(variant in variants)
                (let Variant { fields, .. } = variant)
                (for_all(field in fields)
                    (let Field { ty, .. } = field)
                    (prove_goal(program, env, where_clauses, Relation::well_formed(ty)) => ())))
            ------------------------------------------------------------ ("check adt")
            (check_adt(program, adt) => ())
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
