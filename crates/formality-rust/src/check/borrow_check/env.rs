use std::collections::BTreeSet;
use std::sync::Arc;

use crate::check::borrow_check::flow_state::{FlowState, PendingOutlives};

use crate::check::borrow_check::outlives::verify_universal_outlives;
use crate::grammar::{Binder, ExistentialVar, Relation, Ty, UniversalVar, Wcs};
use crate::grammar::{Parameter, Program};
use crate::prove::prove::{prove_normalize, Constrained, Constraints, Decls, Env};
use crate::rust::Fold;
use formality_core::judgment::{FailureLocation, ProofTree, Proven};
use formality_core::{cast_impl, Downcast, DowncastTo, Set, Upcast};

use crate::check::{Debug, ProvenSet, ToWcs, Visit};

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Clone, Hash)]
pub struct TypeckEnv {
    /// Program being typechecked that contains this functon
    pub program: Arc<Program>,

    /// The environment (set of universal, existential variables)
    pub env: Env,

    /// The declared return type from the function signature.
    pub output_ty: Option<Ty>,

    pub decls: Decls,
}

cast_impl!(TypeckEnv);

impl TypeckEnv {
    pub(crate) fn for_const(env: impl Upcast<Env>, decls: &Decls) -> Self {
        Self {
            program: Arc::new(decls.program.clone()),
            env: env.upcast(),
            output_ty: None,
            decls: decls.clone(),
        }
    }

    pub(crate) fn for_fn_body(env: impl Upcast<Env>, decls: &Decls, output_ty: &Ty) -> Self {
        Self {
            program: Arc::new(decls.program.clone()),
            env: env.upcast(),
            output_ty: Some(output_ty.clone()),
            decls: decls.clone(),
        }
    }

    /// Prove the goal in this environment, accumulating any pending outlive constraints
    /// onto the input set and returning the result.
    #[track_caller]
    pub(crate) fn prove_goal(
        &self,
        assumptions: impl ToWcs,
        state: &FlowState,
        goal: impl ToWcs + Debug,
    ) -> ProvenSet<FlowState> {
        let goal: Wcs = goal.to_wcs();
        self.prove_judgment(
            state,
            assumptions,
            goal.to_wcs(),
            crate::prove::prove::prove,
        )
        .map(|(((), state), proof_tree)| (state, proof_tree))
    }

    #[track_caller]
    pub(crate) fn prove_normalize<T>(
        &self,
        assumptions: impl ToWcs,
        state: &FlowState,
        goal: &T,
    ) -> ProvenSet<(T, FlowState)>
    where
        T: Upcast<Parameter> + Ord + Debug,
        Parameter: DowncastTo<T>,
    {
        let goal: Parameter = goal.upcast();
        self.prove_judgment(state, assumptions, goal, prove_normalize)
            .map(
                |((value, state), proof_tree): Proven<(Parameter, FlowState)>| {
                    (
                        (
                            value
                                .downcast()
                                .expect("to be the same kind as we started with"),
                            state,
                        ),
                        proof_tree,
                    )
                },
            )
    }

    /// Prove the goal with the function `judgment_fn`,
    /// accumulating pending outlive constraints onto the input set.
    ///
    /// One of the difference between this prove_judgment and the one in impl Check is
    /// that this version can accept existential variable, which is needed for handling lifetime.
    /// In the compiler, we insert existential variables for all
    /// lifetimes that appear in the MIR body, and I expect we will do the same here.
    #[track_caller]
    fn prove_judgment<G, C, T>(
        &self,
        state: &FlowState,
        assumptions: impl ToWcs,
        goal: G,
        judgment_fn: impl FnOnce(Decls, Env, Wcs, G) -> ProvenSet<C>,
    ) -> ProvenSet<(T, FlowState)>
    where
        G: Debug + Visit + Clone,
        C: Upcast<Constrained<T>> + Ord + Debug,
        T: Clone + Ord + Debug,
    {
        let assumptions: Wcs = assumptions.to_wcs();

        assert!(self.env.encloses((&assumptions, &goal)));

        // Prove the goal using the given judgment + assumptions.
        // We allow pending outlives so that outlives constraints can be deferred
        // and later verified by the borrow checker.
        let cs = judgment_fn(
            self.decls.clone(),
            self.env.with_allow_pending_outlives(true),
            assumptions.clone(),
            goal.clone(),
        );
        let cs: ProvenSet<Constrained<T>> = cs.map(|(c, proof_tree)| (c.upcast(), proof_tree));

        // Each member `c \in cs` is a valid way corresponds
        // to a set of constraints which, if true, mean the goal is true.
        //
        // i.e., `\forall c \in cs. (c => (assumptions => goal))`
        let cs = match cs.into_map() {
            Ok(cs) => cs,
            Err(e) => return ProvenSet::from(*e),
        };

        // The set of constraints is always non-empty or else the judgment is considered to have failed.
        assert!(!cs.is_empty());

        // If there is anything *unconditionally true*, that's great
        if let Some((Constrained(v, _), proof_tree)) = cs
            .iter()
            .find(|(Constrained(_, c), _)| c.unconditionally_true())
        {
            return ProvenSet::singleton(((v.clone(), state.clone()), proof_tree.clone()));
        }

        // Each `c` in `cs` is a set of [`Constraints`][] that, if they hold,
        // imply the judgment is true. These are all independent. In the trait
        // solver, when we have multiple choices, we explore them ALL, simulating
        // nondeterministic choice. This is what judgment functions do.
        // But we don't want to do that here, we are writing
        // Rust code that is deterministic and only explores a single option.
        // This matches the compiler's behavior, where the trait solver picks the "best"
        // choice of the options it can see, and returns those constraints to the
        // type checker. In our version, the trait solver gives *all* the constraints
        // and the type checker decices the "best" choice. Same basic idea.
        //
        // So which one do we want? We can take any `c` in `cs` and it will be *sound*
        // but it may not be *complete*. In particular, if it has more constraints than
        // are necessary, it could lead us to conclude that the code does not type
        // check -- when it COULD have type checked if we had picked a better choice.
        //
        // Example: suppose that `cs` returns two choices: {
        //     [], // unconditionally true
        //     ['a: 'b], // only true if 'a: 'b
        // }
        //
        // If we pick the second one, and `'a: 'b` does not hold, we get an error
        // in the borrow checker later on. But it wasn't necessary.\
        //
        // So what we do is we look for a *minimal result* -- if there isn't one,
        // for now, we fail with ambiguity.

        // Convert these to pending-outlives; if conversion fails, bail because we don't know
        // whether it would be more minimal than the others or not.
        let mut pending_outlives_sets: Vec<(Set<PendingOutlives>, &T, &ProofTree)> = vec![];
        for (Constrained(v, c), proof_tree) in &cs {
            // Ignore constraints that only prove the goal "might" be true, irrelevant here
            if !c.known_true {
                continue;
            }

            // We already filtered this above
            assert!(c.known_true);

            // We don't have any existential variables, so there can't be a substitution
            assert!(c.substitution().is_empty());

            match self.convert_to_pending_outlives(c) {
                Some(p_o) => pending_outlives_sets.push((p_o, v, proof_tree)),
                None => {
                    return ProvenSet::failed(
                        format!("prove_judgment({goal:?})"),
                        FailureLocation::caller(),
                        format!("failed to convert `{c:?}` to pending-outlives"),
                    );
                }
            }
        }

        // Find the minimal set of the remaining solutions.
        let mut pending_outlives_iter = pending_outlives_sets.into_iter();
        let Some(mut pending_outlives_minimal) = pending_outlives_iter.next() else {
            return ProvenSet::failed(
                format!("prove_judgment({goal:?})"),
                FailureLocation::caller(),
                format!("final constraint set had only ambiguous elements: {cs:#?}"),
            );
        };
        for pending_outlives in pending_outlives_iter {
            // If these outlives constraints are not ordered with respect to `pending_outlives_minimal`, then bail.
            if !pending_outlives.0.is_subset(&pending_outlives_minimal.0)
                && !pending_outlives_minimal.0.is_subset(&pending_outlives.0)
            {
                return ProvenSet::failed(
                    format!("prove_judgment({goal:?})"),
                    FailureLocation::caller(),
                    format!(
                        "no relationship between `{pending_outlives:?}` and `{pending_outlives_minimal:?}`"
                    ),
                );
            }

            // If this set of outlives is a subset of the previous minimal, then use it instead.
            if pending_outlives.0.is_subset(&pending_outlives_minimal.0) {
                pending_outlives_minimal = pending_outlives;
            }
        }

        // Accumulate the new constraints onto the input set
        let (new_outlives, value, proof_tree) = pending_outlives_minimal;
        let state = state.with_outlives(&new_outlives);

        // Verify that the accumulated outlives constraints between universal lifetime
        // variables are justified by the function's where-clause assumptions. This check
        // runs every time outlives may change, so unsound relationships are caught
        // immediately rather than only at return terminators.
        if let Err(e) =
            verify_universal_outlives(self, assumptions, &state.current.outlives).check_proven()
        {
            return ProvenSet::from(*e);
        }

        ProvenSet::singleton(((value.clone(), state), proof_tree.clone()))
    }

    // Convert the pending goals into a series of `PendingOutlives`.
    //
    // The `Constraints` struct contains a set of "pending where-clauses"
    // which must still be proven. In practice, the final result of the
    // top-level judgments we use in the type checker should only have
    // pending outlives requests. This function checks that this is true,
    // converting to a set of `PendingOutlives`, and returns `None` if
    // any other sort of where-clause is found.
    fn convert_to_pending_outlives(&self, c: &Constraints) -> Option<BTreeSet<PendingOutlives>> {
        let mut c_outlives = BTreeSet::default();

        for pending in c.env.pending() {
            match pending.downcast::<Relation>() {
                Some(Relation::Outlives(a, b)) => {
                    c_outlives.insert(PendingOutlives { a, b });
                }

                _ => {
                    // give up
                    return None;
                }
            }
        }

        Some(c_outlives)
    }

    /// Return a new environment creating fresh existential variables suitable for instantiating `binder`
    /// and a substitution with those same variables.
    pub fn instantiate_existentially<T: Fold>(
        &self,
        binder: &Binder<T>,
    ) -> (Self, Vec<ExistentialVar>, T) {
        let (env, subst) = self.env.existential_substitution(binder);
        let value = binder
            .instantiate_with(&subst)
            .expect("suitable substitution");

        (
            Self {
                env,
                ..self.clone()
            },
            subst,
            value,
        )
    }

    /// Instantiate the given binder universally in this environment,
    pub fn instantiate_universally<T>(
        &self,
        binder: &Binder<T>,
    ) -> (TypeckEnv, Vec<UniversalVar>, T)
    where
        T: Fold + Clone,
    {
        let (env, subst) = self.env.universal_substitution(binder);
        let value = binder
            .instantiate_with(&subst)
            .expect("suitable substitution");

        (
            TypeckEnv {
                env,
                ..self.clone()
            },
            subst,
            value,
        )
    }
}
