use formality_types::{
    cast::Upcast,
    collections::{Map, Set},
    derive_links::Variable,
    grammar::{ElaboratedHypotheses, Goal, PlaceholderVar, Universe, VarIndex, VarSubstitution},
    visit::Visit,
};

use crate::{env::query::dedup, Env};

use super::Query;

/// Given an env, assumptions, and goal, returns a `Query`
/// and a mapping from variables in the query back to the
/// variables in `source_env`.
pub fn querify(
    source_env: &Env,
    source_assumptions: &ElaboratedHypotheses,
    source_goal: &Goal,
) -> (Query, VarSubstitution) {
    let source_assumptions = source_env.refresh_inference_variables(source_assumptions);
    let source_goal = source_env.refresh_inference_variables(source_goal);

    // Get a list of the free variables in `goal`, in some deterministic order
    // that will be true across invocations of `querify`.
    let mut source_free_vars = vec![];
    source_free_vars.extend(source_assumptions.free_variables());
    source_free_vars.extend(source_goal.free_variables());
    dedup(&mut source_free_vars);

    // Find the universes of each placeholder in the list, in ascending order,
    // apart from the ROOT universe.
    let source_universes: Set<Universe> = source_free_vars
        .iter()
        .flat_map(|fv| {
            if let Variable::PlaceholderVar(pv) = fv {
                Some(pv.universe)
            } else {
                None
            }
        })
        .collect();
    assert!(
        !source_universes.contains(&Universe::ROOT),
        "unexpected placeholder in root universe"
    );

    // Create a map from each source universe to universe in target;
    // map ROOT to ROOT.
    let mut target_env = Env::default();
    let universes_map: Map<Universe, Universe> = source_universes
        .iter()
        .map(|&source_universe| (source_universe, target_env.next_universe()))
        .chain(Some((Universe::ROOT, Universe::ROOT)))
        .collect();

    // The ROOT universe always maps to itself.
    assert_eq!(universes_map[&Universe::ROOT], Universe::ROOT);

    let mut universe_counters: Map<Universe, VarIndex> = Map::default();
    let var_substitution: VarSubstitution = source_free_vars
        .iter()
        .map(|&source_free_var| {
            (
                source_free_var,
                match source_free_var {
                    Variable::PlaceholderVar(PlaceholderVar {
                        universe: source_universe,
                        var_index: _,
                        kind,
                    }) => {
                        let target_universe = universes_map[&source_universe];
                        let next_var_index = universe_counters
                            .entry(target_universe)
                            .or_insert(VarIndex::ZERO);
                        let var_index = *next_var_index;
                        next_var_index.index += 1;
                        PlaceholderVar {
                            universe: target_universe,
                            var_index,
                            kind,
                        }
                        .upcast()
                    }
                    Variable::InferenceVar(var) => {
                        let kind = source_env.data(var).kind;
                        let source_universe = source_env.data(var).universe;
                        let target_universe = find_target_universe_for_existential_variable(
                            &universes_map,
                            source_universe,
                        );
                        target_env
                            .next_inference_variable(kind, target_universe)
                            .upcast()
                    }
                    Variable::BoundVar(_) => panic!("only free variables expected"),
                },
            )
        })
        .collect();

    let target_assumptions = var_substitution.apply(&source_assumptions);
    let target_goal = var_substitution.apply(&source_goal);
    (
        Query {
            env: target_env,
            assumptions: target_assumptions,
            goal: target_goal,
        },
        var_substitution.reverse(),
    )
}

/// Find the largest universe in the universe map that
/// is less than or equal to `source_universe`. This is used
/// for mapping the universe of an existential variable to the
/// target universe. The idea is that we create a target universe in
/// the query for each placeholder universe we encounter; we then map
/// existential variables such that they can see the same set of placeholders
/// that they could in the original environment.
fn find_target_universe_for_existential_variable(
    universe_map: &Map<Universe, Universe>,
    source_universe: Universe,
) -> Universe {
    universe_map
        .iter()
        .rev()
        .skip_while(|&(&s, _)| s > source_universe)
        .map(|(_, &t)| t)
        .next()
        .unwrap()
}
