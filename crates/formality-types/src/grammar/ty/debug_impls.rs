use super::{AliasName, AliasTy, AssociatedTyName};
use std::fmt::Debug;

impl Debug for AliasTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let AliasTy { name, parameters } = self;
        match name {
            AliasName::AssociatedTyId(AssociatedTyName { trait_id, item_id }) => {
                // Grr, wish we would remember the number of parameters assigned to each position.
                write!(
                    f,
                    "({:?}::{:?})<{}>",
                    trait_id,
                    item_id,
                    parameters
                        .iter()
                        .map(|p| format!("{p:?}"))
                        .collect::<Vec<String>>()
                        .join(","),
                )
            }
        }
    }
}
