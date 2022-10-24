use anyhow::anyhow;
use formality_macros::term;

use crate::{
    cast::Upcast,
    derive_links::Variable,
    fold::Fold,
    grammar::{
        Binder, Fallible, Goal, InferenceVar, Parameter, ParameterKind, PlaceholderVar, Ty,
        Universe,
    },
    term::Term,
};

mod query;
mod simple_sub;
mod test;

#[term]
pub struct Env {
    universe: Universe,
    inference_data: Vec<InferenceVarData>,
    coherence_mode: CoherenceMode,
}

#[term]
pub enum CoherenceMode {
    Yes,
    No,
}

impl Default for Env {
    fn default() -> Self {
        Self {
            universe: Universe::ROOT,
            inference_data: Default::default(),
            coherence_mode: CoherenceMode::No,
        }
    }
}

#[term]
struct InferenceVarData {
    /// Type, lifetime, etc
    kind: ParameterKind,

    /// Universe in which this variable was created.
    universe: Universe,

    /// If `Some`, then this variable must be equal to the specific value
    /// given, and the other fields will all be empty.
    mapped_to: Option<Parameter>,

    /// Types that this variable is a subtype of.
    ///
    /// If this is a lifetime variable, this will be empty.
    subtype_of: Vec<Ty>,

    /// Types that are subtypes of this variable.
    ///
    /// If this is a lifetime variable, this will be empty.
    supertype_of: Vec<Ty>,

    /// Types or lifetimes that this variable outlives.
    ///
    /// Values here will always have the same kind as this variable.
    outlives: Vec<Parameter>,

    /// Types or lifetimes that outlive this variable.
    ///
    /// Values here will always have the same kind as this variable.
    outlived_by: Vec<Parameter>,
}

impl Env {
    /// Returns the same environment, but in coherence mode.
    pub fn in_coherence_mode(self) -> Self {
        Env {
            coherence_mode: CoherenceMode::Yes,
            ..self
        }
    }

    /// Increment the universe counter and return the resulting universe.
    fn next_universe(&mut self) -> Universe {
        self.universe.index += 1;
        self.universe
    }

    /// Create a new inference variable with the given `kind` (type, lifetime, etc) in the given universe.
    fn next_inference_variable(&mut self, kind: ParameterKind, universe: Universe) -> InferenceVar {
        let index = self.inference_data.len();
        self.inference_data.push(InferenceVarData {
            kind,
            universe,
            mapped_to: None,
            subtype_of: vec![],
            supertype_of: vec![],
            outlives: vec![],
            outlived_by: vec![],
        });
        InferenceVar { index }
    }

    fn data(&self, var: InferenceVar) -> &InferenceVarData {
        &self.inference_data[var.index]
    }

    /// Replace all bound variables in `binder` with universal placeholders in a fresh universe.
    pub fn instantiate_universally<T: Term>(&mut self, binder: &Binder<T>) -> T {
        let universe = self.next_universe();
        binder.instantiate(|kind, var_index| {
            PlaceholderVar {
                universe,
                var_index,
            }
            .into_parameter(kind)
        })
    }

    /// Replace all bound variables in `binder` with existential inference variables in the current universe.
    pub fn instantiate_existentially<T: Term>(&mut self, binder: &Binder<T>) -> T {
        binder.instantiate(|kind, _var_index| {
            self.next_inference_variable(kind, self.universe)
                .into_parameter(kind)
        })
    }

    /// Replace any mapped inference variables in `term` with the values they are mapped to.
    pub fn refresh_inference_variables<T: Term>(&self, term: &T) -> T {
        term.substitute(&mut |_kind, var| {
            if let Variable::InferenceVar(var) = var {
                self.data(*var).mapped_to.clone()
            } else {
                None
            }
        })
    }

    /// Returns the universe of a (free) variable.
    pub fn universe(&self, var: Variable) -> Universe {
        assert!(var.is_free());

        match var {
            Variable::PlaceholderVar(pv) => pv.universe,
            Variable::InferenceVar(iv) => self.data(iv).universe,
            Variable::BoundVar(_) => panic!("bound variable not expected"),
        }
    }

    /// Maps the inference var `var` to the value `value`. This may fail, if parameter contains
    /// values that are not in a suitable universe for `var`. It may also produce a list of goals that
    /// must be proven, if `var` had acquired constraints.
    pub fn map_to(
        &mut self,
        var: InferenceVar,
        value: impl Upcast<Parameter>,
    ) -> Fallible<Vec<Goal>> {
        let value = value.upcast();

        assert_eq!(self.data(var).kind, value.kind());

        // If var is already mapped, then equate the value we were mapped to with the new value.
        if let Some(m) = &self.data(var).mapped_to {
            return Ok(vec![Goal::eq(m.clone(), value)]);
        }

        let mut goals = self.occurs_check(var, &value)?;

        let InferenceVarData {
            kind: _,
            universe: _,
            mapped_to,
            subtype_of,
            supertype_of,
            outlives,
            outlived_by,
        } = &mut self.inference_data[var.index];

        // Convert all the relations we've stored up on this variable to goals
        // that relate to the variable's mapped value.
        goals.extend(
            std::iter::empty()
                .chain(
                    std::mem::replace(subtype_of, vec![])
                        .into_iter()
                        .map(|t| Goal::sub(value.clone(), t)),
                )
                .chain(
                    std::mem::replace(supertype_of, vec![])
                        .into_iter()
                        .map(|t| Goal::sub(t, value.clone())),
                )
                .chain(
                    std::mem::replace(outlives, vec![])
                        .into_iter()
                        .map(|t| Goal::outlives(value.clone(), t)),
                )
                .chain(
                    std::mem::replace(outlived_by, vec![])
                        .into_iter()
                        .map(|t| Goal::outlives(t, value.clone())),
                ),
        );

        *mapped_to = Some(value);

        Ok(goals)
    }

    fn parameter(&self, var: InferenceVar) -> Parameter {
        var.into_parameter(self.data(var).kind)
    }

    /// Occurs check: given a variable `mapped_var` that is not yet mapped, ensures that
    ///
    /// * `mapped_var` does not appear in `value`
    /// * all free variables that appear in `value` can be named from the universe of `mapped_var`
    ///   (both now and, in the case of unmapped existential variables, for all future values).
    ///
    /// Returns a list of goals that must be proven for the above conditions to be true.
    fn occurs_check(&mut self, mapped_var: InferenceVar, value: &Parameter) -> Fallible<Vec<Goal>> {
        // the `mapped_var` is not yet mapped
        assert!(self.data(mapped_var).mapped_to.is_none());

        let mut goals = vec![];
        let mapped_universe = self.data(mapped_var).universe;

        for free_var in value.free_variables() {
            match free_var {
                Variable::PlaceholderVar(v) => {
                    if mapped_universe < v.universe {
                        return Err(anyhow!(
                            "cannot map `{mapped_var:?}` in universe {mapped_universe:?} \
                            to `{value:?}`, which contains `{v:?}`",
                        ));
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
                        return Err(anyhow!(
                            "cannot map `{mapped_var:?}` to `{value:?}` as it would produce a cycle"
                        ));
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
