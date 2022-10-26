use formality_types::{
    db::Db,
    derive_links::Parameter,
    grammar::{Fallible, Goal},
};

use super::Env;

impl Env {
    /// Require `a : b`, yielding a new environment + list of goals that must all be solved for `a : b` to be true.
    /// Returns `Err` if the two parameters can never be related.
    pub(super) fn outlives(
        &self,
        _db: &Db,
        _a: &Parameter,
        _b: &Parameter,
    ) -> Fallible<(Env, Vec<Goal>)> {
        todo!()
    }
}
