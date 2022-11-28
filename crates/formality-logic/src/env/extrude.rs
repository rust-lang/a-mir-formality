use contracts::requires;
use formality_macros::term;
use formality_types::{
    cast::{Upcast, Upcasted},
    fold::Fold,
    grammar::{AtomicRelation, Goal, InferenceVar, Parameter, Universe, Variable},
    seq,
    term::Term,
};

use super::Env;

#[term]
#[derive(Copy)]
pub enum Relationship {
    SubtypeOf,
    SupertypeOf,
    Outlives,
    OutlivedBy,
}

impl Relationship {
    fn invert(self) -> Relationship {
        match self {
            Relationship::SubtypeOf => Relationship::SupertypeOf,
            Relationship::SupertypeOf => Relationship::SubtypeOf,
            Relationship::Outlives => Relationship::OutlivedBy,
            Relationship::OutlivedBy => Relationship::Outlives,
        }
    }

    fn relation(self, a: &Parameter, b: &Parameter) -> AtomicRelation {
        match self {
            Relationship::SubtypeOf => AtomicRelation::Sub(a.clone(), b.clone()),
            Relationship::Outlives => AtomicRelation::Outlives(a.clone(), b.clone()),
            Relationship::SupertypeOf => AtomicRelation::Sub(b.clone(), a.clone()),
            Relationship::OutlivedBy => AtomicRelation::Outlives(b.clone(), a.clone()),
        }
    }
}

impl Env {
    #[requires(!self.is_mapped(variable_a))]
    pub(super) fn relate_parameter(
        &mut self,
        variable_a: InferenceVar,
        relationship: Relationship,
        parameter_b: impl Upcast<Parameter>,
    ) -> Vec<Goal> {
        let parameter_b = parameter_b.upcast();

        let known_bounds = self.known_bounds(variable_a, relationship);
        if known_bounds.iter().any(|p| *p == parameter_b) {
            return seq![];
        }

        self.bound_variable(variable_a, relationship, &parameter_b);

        self.known_bounds(variable_a, relationship.invert())
            .into_iter()
            .map(|bound| relationship.relation(&bound, &parameter_b))
            .upcasted()
            .collect()
    }

    /// Returns a new parameter `P` in `universe` such that `P (R) parameter`
    /// (assuming that the goals are proven).
    ///
    /// i.e., `extrude_parameter(U0, SubtypeOf, X)` would return a variable `?Y`
    /// in `U0` where `?Y <: X`.
    pub(super) fn extrude_parameter(
        &mut self,
        universe: Universe,
        relationship: Relationship,
        parameter: impl Upcast<Parameter>,
    ) -> (Parameter, Vec<Goal>) {
        let parameter = parameter.upcast();
        let mut stack = vec![];
        self.extrude_parameter_with_stack(&mut stack, universe, relationship, &parameter)
    }

    fn extrude_parameter_with_stack(
        &mut self,
        stack: &mut Vec<(Variable, Parameter)>,
        universe: Universe,
        relationship: Relationship,
        parameter: &Parameter,
    ) -> (Parameter, Vec<Goal>) {
        let mut goals = vec![];

        let result = parameter.substitute(&mut |variable| {
            let (parameter, g) = self.extrude_variable(stack, universe, relationship, variable);
            goals.extend(g);
            Some(parameter)
        });

        (result, goals)
    }

    fn extrude_variable(
        &mut self,
        stack: &mut Vec<(Variable, Parameter)>,
        universe: Universe,
        relationship: Relationship,
        variable: Variable,
    ) -> (Parameter, Vec<Goal>) {
        // Check if we are mapping a variable that's already on the stack.
        if let Some(pair) = stack.iter().find(|pair| pair.0 == variable) {
            return (pair.1.clone(), vec![]);
        }

        // Variables visible from Universe are just mapped to themselves
        if self.universe(variable) <= universe {
            return (variable.upcast(), vec![]);
        }

        match variable {
            // Given an inference variable `?X` in some universe not visible from `universe`,
            // which has some known bounds `B0..Bn`:
            //
            //     ?X -(R)-> B0...Bn
            //
            // we will create an extruded variable like so...
            //
            //     ?X ---(R)-> B0   ... Bn
            //      |           ^        ^
            //     (~R)         :        :
            //      |          (R)      (R)
            //      v           :        :
            //     ?X_e -(R)-> B0_e ... B0_n
            //
            // (Note that `:` edges are "logical" edges and may not appear directly
            // in the graph (i.e., it may be that the graph has an edge `B0 -(~R)-> B0_e`).)
            //
            // So, assuming `(R)` is `<:`, then `?X` effectively becomes some subtype
            // of `?X` that is also required to be a subtype of everything `?X` is a subtype
            // of (i.e., `?X` is "between" `?X_e` and the bounds `B0..Bn`).
            //
            // Note: if a new bound is added to `?X` later on, it will get propagated
            // to `?X_e` because each subtype of `?X` is related to all its supertypes,
            // and `?X_e` is registered as a supertype.
            Variable::InferenceVar(variable) => {
                // Create the result variable `?X_e`.
                let variable_extruded = self.next_inference_variable(variable.kind, universe);
                stack.push((variable.upcast(), variable_extruded.upcast()));

                // For each bound B where `?X (R) B`, extrude a bound `B_e` where `B_e (R) B`.
                let bounds = self.known_bounds(variable, relationship);
                let (bounds_extruded, bound_goals): (Vec<Parameter>, Vec<Vec<Goal>>) = bounds
                    .iter()
                    .map(|b| self.extrude_parameter(universe, relationship, b))
                    .unzip();

                // Relate result variable `?X_e` to each extruded bound `B_e` such that
                // `?X_e (R) B_e` (and therefore `?X_e (R) B`, transitively).
                for bound in &bounds_extruded {
                    self.bound_variable(variable_extruded, relationship, bound);
                }

                // Relate `?X_e (R) ?X` -- but we cannot directly add this edge,
                // because `?X_e` is in a lower universe, so we add the inverse, `?X (~R) ?X_e`.
                //
                // This preserves the invariant that a variable in universe U has no
                // outgoing edge to anything in a higher universe.
                self.bound_variable(variable, relationship.invert(), &variable_extruded.upcast());

                stack.pop();

                (
                    variable_extruded.upcast(),
                    bound_goals.into_iter().flatten().collect(),
                )
            }

            Variable::PlaceholderVar(_) => todo!(),

            Variable::BoundVar(_) => panic!("only free variables expected in extrude"),
        }
    }

    /// Returns all known bounds `B` on `variable` such that `variable (R) B`.
    fn known_bounds(&self, variable: InferenceVar, relationship: Relationship) -> Vec<Parameter> {
        match relationship {
            Relationship::SubtypeOf => self.data(variable).subtype_of.iter().upcasted().collect(),
            Relationship::SupertypeOf => {
                self.data(variable).supertype_of.iter().upcasted().collect()
            }
            Relationship::Outlives => self.data(variable).outlives.clone(),
            Relationship::OutlivedBy => self.data(variable).outlived_by.clone(),
        }
    }

    /// Add `parameter` as a bound on variable.
    fn bound_variable(
        &mut self,
        variable: InferenceVar,
        relationship: Relationship,
        parameter: &Parameter,
    ) {
        assert!(self.universe_term(parameter) <= self.data(variable).universe);
        match relationship {
            Relationship::SubtypeOf => match parameter {
                Parameter::Ty(t) => self.data_mut(variable).subtype_of.push(t.clone()),
                _ => panic!("expected a type"),
            },
            Relationship::SupertypeOf => match parameter {
                Parameter::Ty(t) => self.data_mut(variable).supertype_of.push(t.clone()),
                _ => panic!("expected a type"),
            },
            Relationship::Outlives => self.data_mut(variable).outlives.push(parameter.clone()),
            Relationship::OutlivedBy => self.data_mut(variable).outlived_by.push(parameter.clone()),
        }
    }

    /// Returns the maximal universe of any variable that appears in the given term.
    fn universe_term(&self, term: &impl Term) -> Universe {
        term.free_variables()
            .iter()
            .map(|v| self.universe(*v))
            .max()
            .unwrap_or(Universe::ROOT)
    }
}
