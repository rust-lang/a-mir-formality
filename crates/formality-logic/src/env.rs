use crate::Db;
use contracts::requires;
use formality_macros::term;
use formality_types::cast::{To, Upcast};
use formality_types::grammar::{ElaboratedHypotheses, LtData, ParameterData, TyData};
use formality_types::{
    derive_links::Variable,
    grammar::{
        AtomicRelation, Binder, Fallible, Goal, InferenceVar, Parameter, ParameterKind,
        PlaceholderVar, Ty, Universe, VarSubstitution,
    },
    term::Term,
};

use self::query::{querify, Query};

mod bound;
mod eq;
mod extrude;
mod occurs;
mod outlives;
pub mod query;
mod rigid;
mod sub;
mod test;
mod well_formed;

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
        InferenceVar { index, kind }
    }

    fn data(&self, var: InferenceVar) -> &InferenceVarData {
        &self.inference_data[var.index]
    }

    fn is_mapped(&self, var: InferenceVar) -> bool {
        self.inference_data[var.index].mapped_to.is_some()
    }

    /// Returns what this inference variable is mapped to, if anything.
    pub fn mapped_to(&self, var: InferenceVar) -> Option<Parameter> {
        self.inference_data[var.index].mapped_to.clone()
    }

    fn data_mut(&mut self, var: InferenceVar) -> &mut InferenceVarData {
        &mut self.inference_data[var.index]
    }

    /// Creates a new existential inference variable in the current universe.
    pub fn new_existential_variable(&mut self, kind: ParameterKind) -> InferenceVar {
        self.next_inference_variable(kind, self.universe)
    }

    /// Replace all bound variables in `binder` with universal placeholders in a fresh universe.
    pub fn instantiate_universally<T: Term>(&mut self, binder: &Binder<T>) -> T {
        let universe = self.next_universe();
        binder.instantiate(|kind, var_index| {
            PlaceholderVar {
                universe,
                var_index,
                kind,
            }
            .upcast()
        })
    }

    /// Replace all bound variables in `binder` with existential inference variables in the current universe.
    pub fn instantiate_existentially<T: Term>(&mut self, binder: &Binder<T>) -> T {
        binder.instantiate(|kind, _var_index| {
            self.next_inference_variable(kind, self.universe).upcast()
        })
    }

    /// Replace any mapped inference variables in `term` with the values they are mapped to.
    pub fn refresh_inference_variables<T: Term>(&self, term: &T) -> T {
        term.substitute(&mut |var| {
            if let Variable::InferenceVar(var) = var {
                self.data(var).mapped_to.clone()
            } else {
                None
            }
        })
    }

    /// True if all inference variables in `term` are unmapped.
    pub fn fully_refreshed(&self, term: &impl Term) -> bool {
        term.free_variables().iter().all(|var| {
            if let Variable::InferenceVar(var) = var {
                !self.is_mapped(*var)
            } else {
                true
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

    /// Returns the maximum universe that appears in this term (or which could appear
    /// in the future).
    pub fn term_universe(&self, term: &impl Term) -> Universe {
        self.refresh_inference_variables(term)
            .free_variables()
            .into_iter()
            .map(|free_var| self.universe(free_var))
            .max()
            .unwrap_or(Universe::ROOT)
    }

    /// Maps the inference var `var` to the value `value`.
    #[requires(!self.is_mapped(var))]
    #[requires(self.data(var).kind == value.kind())]
    #[requires(!self.occurs_in(var, value))]
    #[requires(self.data(var).universe >= self.term_universe(value))]
    #[ensures(self.is_mapped(var))]
    fn map_to(&mut self, var: InferenceVar, value: &Parameter) -> Vec<Goal> {
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
        let goals = Vec::from_iter(
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

        *mapped_to = Some(value.clone());

        goals
    }

    /// Creates a *query* from the given goal in this environment, along
    /// with a mapping from the variables defined in that query to the
    /// variables in this environment.
    ///
    /// A *query* packages up portions of this environment along with the goal
    /// into a canonical wrapper that is independent of this environment.
    ///
    /// The goal can then be solved and the resulting solution can be applied
    /// back onto this original environment via the `VarSubstitution`.
    pub fn query(&self, goal: &Goal) -> (Query, VarSubstitution) {
        querify(self, goal)
    }

    /// Apply a relation using the builtin rules, returning
    /// either an error (if they cannot be equated) or a new environment
    /// plus a list of goals that must still be proven.
    pub fn apply_relation(
        &self,
        db: &Db,
        assumptions: &ElaboratedHypotheses,
        r: &AtomicRelation,
    ) -> Fallible<(Env, Vec<Goal>)> {
        match r {
            AtomicRelation::Equals(a, b) => self.eq(db, a, b),
            AtomicRelation::Sub(a, b) => self.sub(db, a, b),
            AtomicRelation::Outlives(a, b) => self.outlives(db, assumptions, a, b),
            AtomicRelation::WellFormed(a) => self.well_formed(a),
        }
    }

    /// Returns a set of relations affecting variables visible from the given universe.
    pub fn inference_var_relations(&self, universe: Universe) -> Vec<AtomicRelation> {
        self.inference_data
            .iter()
            .zip(0..)
            .filter(|(data, _)| data.universe <= universe)
            .flat_map(|(data, index)| {
                let InferenceVarData {
                    kind,
                    universe: _,
                    mapped_to,
                    subtype_of,
                    supertype_of,
                    outlives,
                    outlived_by,
                } = data;
                let v = InferenceVar { index, kind: *kind };

                std::iter::empty()
                    .chain(
                        mapped_to
                            .iter()
                            .map(|p| AtomicRelation::Equals(v.to(), p.to())),
                    )
                    .chain(
                        subtype_of
                            .iter()
                            .map(|t| AtomicRelation::Sub(v.to(), t.to())),
                    )
                    .chain(
                        supertype_of
                            .iter()
                            .map(|t| AtomicRelation::Sub(t.to(), v.to())),
                    )
                    .chain(
                        outlives
                            .iter()
                            .map(|t| AtomicRelation::Outlives(v.to(), t.to())),
                    )
                    .chain(
                        outlived_by
                            .iter()
                            .map(|t| AtomicRelation::Outlives(t.to(), v.to())),
                    )
                    .collect::<Vec<_>>()
            })
            .collect()
    }

    /// True if `parameter` is an unbound inference variable (i.e., a value not yet equated with
    /// a specific value). Note that it may be a variable that has sub/super/outlives constraints.
    pub fn is_unbound_inference_variable(&self, parameter: impl Upcast<Parameter>) -> bool {
        let parameter: Parameter = parameter.upcast();
        match parameter.data() {
            ParameterData::Ty(TyData::Variable(Variable::InferenceVar(v)))
            | ParameterData::Lt(LtData::Variable(Variable::InferenceVar(v))) => {
                if let Some(p) = &self.data(*v).mapped_to {
                    self.is_unbound_inference_variable(p)
                } else {
                    true
                }
            }
            _ => false,
        }
    }
}
