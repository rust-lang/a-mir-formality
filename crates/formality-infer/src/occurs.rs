use formality_types::{
    grammar::{Fallible, Goal, InferenceVar, Variable},
    term::Term,
};

use crate::Env;

impl Env {
    /// Occurs check: given a variable `mapped_var` that is not yet mapped, ensures that
    ///
    /// * `mapped_var` does not appear in `value`
    /// * all free variables that appear in `value` can be named from the universe of `mapped_var`
    ///   (both now and, in the case of unmapped existential variables, for all future values).
    ///
    /// Returns a list of goals that must be proven for the above conditions to be true.
    pub(crate) fn occurs_check(
        &mut self,
        mapped_var: InferenceVar,
        value: &impl Term,
    ) -> Fallible<Vec<Goal>> {
        // the `mapped_var` is not yet mapped
        assert!(self.data(mapped_var).mapped_to.is_none());

        let mut goals = vec![];
        let mapped_universe = self.data(mapped_var).universe;

        for free_var in value.free_variables() {
            match free_var {
                Variable::PlaceholderVar(v) => {
                    if mapped_universe < v.universe {
                        anyhow::bail!(
                            "cannot map `{mapped_var:?}` in universe {mapped_universe:?} \
                            to `{value:?}`, which contains `{v:?}`",
                        );
                    }
                }
                Variable::InferenceVar(free_var) => {
                    if let Some(m) = &self.data(free_var).mapped_to {
                        let m = m.clone();
                        goals.extend(self.occurs_check(mapped_var, &m)?);
                        continue;
                    }

                    // Prevent `mapped_var` from occurring in `value`.
                    if mapped_var == free_var {
                        anyhow::bail!(
                            "cannot map `{mapped_var:?}` to `{value:?}` as it would produce a cycle"
                        );
                    }

                    // This is a subtle case. Imagine we have `?0` in U0 being
                    // mapped to `Vec<?1>`, where `?1` is in `U1`. This is not an error --
                    // after all, `?1` can be mapped to a type like `u32` that is in U0.
                    // But `?1` can *also* be mapped to a placeholder like `!1`, and that would
                    // be a problem, because then the value of `?0` would be `Vec<!1>`,
                    // and that is not nameable from U0. To avoid that, we map `?1` with
                    // a fresh variable `?2` that is in U0. Now `?1` can only be mapped to values
                    // in U0. (In the paper, they modify the universe of `free_var` in place, but I'd
                    // prefer to avoid that, as it is the only time that the universe of a variable changes
                    // from the way it is declared.)
                    if mapped_universe < self.data(free_var).universe {
                        let fresh_var = self.next_inference_variable(
                            self.data(free_var).kind,
                            self.data(mapped_var).universe,
                        );
                        goals.extend(
                            self.map_to(free_var, self.parameter(fresh_var))
                                .expect("fresh variable cannot fail the occurs check"),
                        );
                    }
                }
                Variable::BoundVar(_) => panic!("free-variables should not return bound variables"),
            }
        }

        Ok(goals)
    }
}
