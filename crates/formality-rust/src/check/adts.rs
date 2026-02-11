use std::collections::HashSet;

use crate::grammar::Fallible;
use crate::grammar::{Adt, AdtBoundData, Field, Variant};
use crate::prove::prove::Env;
use anyhow::bail;
use formality_core::judgment::ProofTree;

impl super::Check<'_> {
    pub(super) fn check_adt(&self, adt: &Adt) -> Fallible<ProofTree> {
        let Adt { id, binder } = adt;
        let mut proof_tree = ProofTree::leaf(format!("check_adt({id:?})"));

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

        let mut env = Env::default();

        let AdtBoundData {
            where_clauses,
            variants,
        } = env.instantiate_universally(binder);

        proof_tree
            .children
            .push(self.prove_where_clauses_well_formed(&env, &where_clauses, &where_clauses)?);

        for Variant { name: _, fields } in &variants {
            for Field { name: _, ty } in fields {
                proof_tree.children.push(self.prove_goal(
                    &env,
                    &where_clauses,
                    ty.well_formed(),
                )?);
            }
        }

        Ok(proof_tree)
    }
}
