use crate::grammar::{
    ExistentialVar, Parameter, Substitution, UniversalVar, VarIndex, VarSubstitution, Variable,
};
use crate::rust::Term;
use formality_core::{Deduplicate, Downcast, Upcast};

use super::{Constraints, Env};

#[cfg(test)]
mod test;

/// Stores a mapping resulting from [`minimize`][], see that function for detils.
pub struct Minimization {
    env_max: Env,
    min2max_subst: VarSubstitution,
}

/// Rewrites the environment given the term so that...
///
/// * variables have minimized names starting from `0` based on which appears first
/// * unused variables that don't appear in the term are removed from the environment.
///
/// Returns a [`Minimization`][] which is a mapping from the new variable names *back*
/// to the old ones. This allows us to translate the result from our minimized variables
/// back to the names that were being used on entry.
///
/// # Why do we do this?
///
/// It helps to detect cycles. For example, if you were in the midst of proving
///
/// exists<A> { A: Trait }
///
/// and that causes you to prove `exists<B> { B: Trait }`, the initial environment
/// would look like
///
/// exists<A, B> { B: Trait}
///
/// i.e., the environment contains A and B are existential variables that are in scope.
/// This does not at first glance appear to be a cycle. But if we apply the same rule
/// we would next wind up proving `exists<A, B, C> { C: Trait }` and so on, and
/// never terminate.
///
/// Because we minimize it, we see that `exists<A, B> { B: Trait }` is equivalent to
/// `exists<A> { A: Trait }`, and hence we detect the cycle successfully.
///
/// *But* then we have a result that references the variable `A`  and that result must be
/// translated back to reference the variable `B` in the original context.
pub fn minimize<T: Term>(env_max: Env, term: T) -> (Env, T, Minimization) {
    let fv = term.free_variables().deduplicate();

    let renamed_fv: Vec<Variable> = fv
        .iter()
        .zip(0..)
        .map(|(&fv, index)| match fv {
            Variable::UniversalVar(UniversalVar { kind, var_index: _ }) => UniversalVar {
                kind,
                var_index: VarIndex { index },
            }
            .upcast(),
            Variable::ExistentialVar(ExistentialVar { kind, var_index: _ }) => ExistentialVar {
                kind,
                var_index: VarIndex { index },
            }
            .upcast(),
            Variable::BoundVar(_) => panic!("unexpected bound variable {fv:?}"),
        })
        .collect();

    let max2min_subst: VarSubstitution = fv.iter().copied().zip(renamed_fv).collect();

    let mut env_min = env_max.clone();

    env_min.remove_variables_unless_within(&fv);
    let env_min = env_min.substitute(&max2min_subst);
    let term_min = max2min_subst.apply(&term);

    (
        env_min,
        term_min,
        Minimization {
            env_max,
            min2max_subst: max2min_subst.reverse(),
        },
    )
}

impl Minimization {
    pub fn reconstitute(&self, constraints: Constraints) -> Constraints {
        // Our task:
        //
        // For `env_min`, we had a set of variables like `A, B, C`
        //
        // For `env_max`, we removed unused variables (say B) and remapped remaining
        // ones to sequential names (say X, Y). We have a list of removed variables `[B]`
        // and a map `[X -> A, Y -> C]` back to the original names.
        //
        // In `env`, we have a variant of `env_max` in which some fresh variables have
        // been added, e.g., `X, Z, Y`. The placement of these variables is significant.
        //
        // We wish to produce a map back to `env_min`, e.g., `A, B, D, C`, and a substitution
        // `[X -> A, Y -> C, Z -> D]` to go back to it. This substitution should produce
        // results that respect the relative ordering in `env`.
        //
        // How we do this:
        //
        // * Create an iterator over `env_min` and `env`
        // * Loop
        //     * If the next `env` item maps to the next env_min, advance both
        //     * Else if the next `env_min` item was deleted, advance `env_min`
        //     * Else, the next `env` item is fresh, so create a fresh variable in `env_min` and insert it

        let mut env_out = self.env_max.clone();

        let mut fresh_vars: Vec<ExistentialVar> = vec![];
        let mut env2out_subst = self.min2max_subst.clone();
        for &var in constraints.env().variables() {
            if let Some(var_out) = env2out_subst.map_var(var) {
                // e.g., here next variable might be Y in the list above...
                for fresh_var in fresh_vars.drain(..) {
                    // ...so we remove Z, create a fresh variable D for env_min
                    // and place it before Y...
                    let universe_out = env_out.universe(var_out);
                    let fresh_var_out = env_out.insert_fresh_before(fresh_var.kind, universe_out);
                    // ...and map Z to D.
                    env2out_subst.insert_mapping(fresh_var, fresh_var_out);
                }
            } else {
                // e.g., Z in the example above
                let var: ExistentialVar = var.downcast().unwrap(); // we only ever insert fresh existential variables
                fresh_vars.push(var);
            }
        }

        // Assert that we for each two consecutive variables A < B in env,
        // the same relative ordering exists in env_out.
        let env_variables = constraints.env().variables();
        for i in 1..env_variables.len() {
            let a = env_variables[i - 1];
            let b = env_variables[i];
            let a_out = env2out_subst.map_var(a).unwrap();
            let b_out = env2out_subst.map_var(b).unwrap();
            assert!(env_out.universe(a_out) < env_out.universe(b_out));
        }

        let Constraints {
            env: env_in,
            known_true,
            substitution,
        } = constraints;
        let substitution: Substitution = substitution
            .iter()
            .map(|(x, p)| -> (Variable, Parameter) {
                (env2out_subst.map_var(x).unwrap(), env2out_subst.apply(&p))
            })
            .collect();

        for pending_in in env_in.pending() {
            env_out = env_out.with_pending(env2out_subst.apply(pending_in));
        }

        Constraints {
            env: env_out,
            known_true,
            substitution,
        }
    }
}
