use contracts::requires;
use formality_types::{
    grammar::{Fallible, Goal, InferenceVar, Universe, Variable},
    term::Term,
};

use crate::Env;

impl Env {
    #[requires(!self.is_mapped(mapped_var))]
    #[requires(self.fully_refreshed(value))]
    pub(crate) fn occurs_in(&self, mapped_var: InferenceVar, value: &impl Term) -> bool {
        value
            .free_variables()
            .into_iter()
            .any(|free_var| match free_var {
                Variable::PlaceholderVar(_) => false,
                Variable::InferenceVar(v) => {
                    assert!(self.data(v).mapped_to.is_none()); // input is fully refreshed
                    v == mapped_var // FIXME: check subtype-of / supertype-of?
                }
                Variable::BoundVar(_) => panic!("did not expect bound variable"),
            })
    }

    /// Returns a version of `value` in which every variable whose universe
    /// exceeds `universe` is replaced with a fresh inference variable.
    #[requires(self.fully_refreshed(value))]
    pub(crate) fn generalize_universe<T: Term>(&mut self, universe: Universe, value: &T) -> T {
        value.substitute(&mut |kind, &var| {
            if self.universe(var) > universe {
                Some(
                    self.next_inference_variable(kind, universe)
                        .into_parameter(kind),
                )
            } else {
                None
            }
        })
    }

    /// Occurs check (part 1): given a variable `mapped_var` that is not yet mapped, ensures that
    ///
    /// * `mapped_var` does not appear in `value`
    /// * all placeholders that appear in `value` can be named from the universe of `mapped_var`
    ///
    /// Returns an error if the above conditions are not met.
    ///
    /// If the above conditions *are* met, then returns a list of the unmapped existential
    /// that appaer in `value` but whose universe exceeds the universe of `mapped_var`.
    /// These are variables whose future values might not be nameable from `mapped_`var.
    /// The caller must invoke `constrain_vars` with this list if the unification succeeds.
    pub(crate) fn occurs_check(
        &self,
        mapped_var: InferenceVar,
        value: &impl Term,
    ) -> Fallible<Vec<InferenceVar>> {
        // the `mapped_var` is not yet mapped
        assert!(self.data(mapped_var).mapped_to.is_none());

        let mut results = vec![];
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
                        results.extend(self.occurs_check(mapped_var, &m)?);
                        continue;
                    }

                    // Prevent `mapped_var` from occurring in `value`.
                    if mapped_var == free_var {
                        anyhow::bail!(
                            "cannot map `{mapped_var:?}` to `{value:?}` as it would produce a cycle"
                        );
                    }

                    // Check if the universe `free_var` exceeds `mapped_universe`;
                    // this is not an error, but we have to handle this case carefully,
                    // so we return that to the user.
                    if mapped_universe < self.data(free_var).universe {
                        results.push(free_var);
                    }
                }
                Variable::BoundVar(_) => panic!("free-variables should not return bound variables"),
            }
        }

        Ok(results)
    }

    /// Given the (successful) results from `occurs_check`, constraints each variable
    /// in `variables` so that it cannot contain values that exceed the unvierse of
    /// `mapped_var`. This is done by mapping it to a fresh variable in `mapped_var`'s
    /// universe. Returns a list of goals that must be proven as a result.
    ///
    /// # Explanation for why this is necessary
    ///
    /// This is a subtle case. Imagine we have `?0` in U0 being
    /// mapped to `Vec<?1>`, where `?1` is in `U1`. This is not an error --
    /// after all, `?1` can be mapped to a type like `u32` that is in U0.
    /// But `?1` can *also* be mapped to a placeholder like `!1`, and that would
    /// be a problem, because then the value of `?0` would be `Vec<!1>`,
    /// and that is not nameable from U0. To avoid that, we map `?1` with
    /// a fresh variable `?2` that is in U0. Now `?1` can only be mapped to values
    /// in U0. (In the paper, they modify the universe of `free_var` in place, but I'd
    /// prefer to avoid that, as it is the only time that the universe of a variable changes
    /// from the way it is declared.)
    pub(crate) fn constrain_vars(
        &mut self,
        mapped_var: InferenceVar,
        variables: Vec<InferenceVar>,
    ) -> Vec<Goal> {
        let mapped_universe = self.data(mapped_var).universe;
        let mut goals = vec![];
        for free_var in variables {
            if mapped_universe < self.data(free_var).universe {
                let fresh_var = self.next_inference_variable(
                    self.data(free_var).kind,
                    self.data(mapped_var).universe,
                );
                goals.extend(self.map_to(free_var, &self.parameter(fresh_var)));
            }
        }
        goals
    }
}
