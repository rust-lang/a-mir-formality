use crate::check::borrow_check::liveness::LivePlaces;
use crate::check::borrow_check::typed_place_expression::TypedPlaceExpr;
use crate::grammar::expr::{Label, LabelId, PlaceExpr};
use crate::grammar::{InputArg, Lt, Parameter, Ty, ValueId};
use crate::grammar::{RefKind, Variable};
use crate::prove::{Env, MaxUniverse};
use formality_core::visit::CoreVisit;
use formality_core::{term, Fallible, Set, Union, Upcast, UpcastFrom};

/// A scope in the scope stack, tracking labeled blocks and loops.
/// Scopes live in `PointFlowState` and track locals for drop purposes.
/// The types are stored here so that type lookup can happen through the flow state.
#[term]
pub struct Scope {
    /// Maximum universe for types of variables appearing in this scope.
    pub max_universe: MaxUniverse,

    /// Label on this scope (if any).
    pub label: Option<LabelId>,

    /// Places live after this scope exits (break target liveness).
    pub break_live_places: LivePlaces,

    /// If `Some`, this scope can be targeted by `continue`, and the value
    /// is the set of places live at the entry to the loop body.
    /// If `None`, this is a plain block scope (no `continue` allowed).
    pub continue_live_places: Option<LivePlaces>,

    /// Local variables declared in this scope, with their types.
    /// Used for type lookup (name resolution). Searched by `local_variable`, `has_local`.
    /// Always added to the innermost scope, regardless of label.
    pub locals: Vec<(ValueId, Ty)>,

    /// Local variables to drop when this scope exits.
    /// For `let 'a: x = ...`, `x` goes into the named scope `'a`'s `drop_locals`.
    /// For `let x = ...`, `x` goes into the innermost scope's `drop_locals`.
    pub drop_places: Vec<TypedPlaceExpr>,
}

impl Scope {
    pub fn has_label(&self, label: &LabelId) -> bool {
        self.label.as_ref() == Some(label)
    }

    /// At control-flow joins (if/else), assert that `locals` is identical
    /// and union only the `drop_locals` from `other` into `self`.
    fn union_drop_locals(&mut self, other: &Scope) {
        assert_eq!(
            self.locals, other.locals,
            "locals must be identical at join points"
        );
        for id in &other.drop_places {
            if !self.drop_places.contains(id) {
                self.drop_places.push(id.clone());
            }
        }
    }
}

/// Represents flow-sensitive state at a single program point
#[term]
#[derive(Default)]
pub struct PointFlowState {
    /// The set of outlives relationships that must hold
    pub outlives: Set<PendingOutlives>,

    /// The set of loans that are live (issued, not killed)
    pub loans_live: Set<Loan>,

    /// Places that are uninitialized or have been moved from
    /// Read or write or move from any place whose prefix is in this set is an error
    pub uninit: Set<PlaceExpr>,

    /// For each live loan, the outlives edges that already existed when it was
    /// issued. See [`LoanOrigin`] and [`PointFlowState::outlives_after_loan`].
    pub loan_origins: Set<LoanOrigin>,
}

impl PointFlowState {
    pub fn with_loan(&self, loan: Loan) -> Self {
        let mut this = self.clone();
        // A loop body re-issues the same loan on its second pass, against a
        // larger outlives set. Keep one entry per loan, intersecting as a join
        // would, so the entry stays deterministic and the loop rule's fixed
        // point still converges.
        let outlives_at_issue = match this.loan_origins.iter().find(|o| o.loan == loan) {
            Some(previous) => previous
                .outlives_at_issue
                .intersection(&self.outlives)
                .cloned()
                .collect(),
            None => self.outlives.clone(),
        };
        this.loan_origins.retain(|o| o.loan != loan);
        this.loan_origins.insert(LoanOrigin {
            loan: loan.clone(),
            outlives_at_issue,
        });
        this.loans_live.insert(loan);
        this
    }

    pub fn with_outlives(&self, outlives: &Set<PendingOutlives>) -> Self {
        PointFlowState {
            outlives: Union((&self.outlives, outlives)).upcast(),
            loans_live: self.loans_live.clone(),
            uninit: self.uninit.clone(),
            loan_origins: self.loan_origins.clone(),
        }
    }

    /// The outlives edges that came into being *after* `loan` was issued.
    ///
    /// A loan enters `'b` only if it is already in `'a` at the point `'a: 'b`
    /// is applied -- polonius's eager propagation, written here as a lazy
    /// subtraction. An edge that already existed when the loan was issued was
    /// created earlier, possibly on a path that never reaches the loan at all,
    /// so it cannot carry the loan forward.
    pub fn outlives_after_loan(&self, loan: &Loan) -> Set<PendingOutlives> {
        self.outlives
            .iter()
            .filter(|o| !self.predates_loan(o, loan))
            .cloned()
            .collect()
    }

    /// Whether `edge` was already in force when `loan` was issued.
    pub fn predates_loan(&self, edge: &PendingOutlives, loan: &Loan) -> bool {
        let Some(origin) = self.loan_origins.iter().find(|o| o.loan == *loan) else {
            panic!("Loan origin not recorded for loan: {:?}", loan);
        };

        origin.outlives_at_issue.contains(edge)
    }

    /// Mark a place as initialized: remove it and all sub-paths from uninit
    pub fn mark_initialized(&mut self, place: &PlaceExpr) {
        self.uninit = self
            .uninit
            .iter()
            .filter(|u| !place.is_prefix_of(u))
            .cloned()
            .collect();
    }

    /// Mark a place as moved/uninitialized: add it to uninit and
    /// remove any proper sub-paths (covered by the more general path).
    pub fn mark_uninit(&mut self, place: &PlaceExpr) {
        // Remove any sub-paths that are covered by this more general path
        self.uninit = self
            .uninit
            .iter()
            .filter(|u| !place.is_prefix_of(u))
            .cloned()
            .collect();
        // Don't add if a prefix is already uninit (already covered)
        if !self.uninit.iter().any(|u| u.is_prefix_of(place)) {
            self.uninit.insert(place.clone());
        }
    }

    /// Modifies `self` by removing all variables in `variables` from the environment and from the substitution.
    fn pop_vars(&mut self, variables: &Set<Variable>) {
        self.outlives
            .retain(|v| !v.free_variables().iter().any(|v| variables.contains(v)));
        self.loans_live
            .retain(|v| !v.free_variables().iter().any(|v| variables.contains(v)));
        self.uninit
            .retain(|v| !v.free_variables().iter().any(|v| variables.contains(v)));

        // A loan's recorded origin is pruned the same way, but edge-by-edge:
        // dropping the whole entry because one edge mentions a popped variable
        // would silently restore the full-set behaviour for that loan.
        self.loan_origins = self
            .loan_origins
            .iter()
            .filter(|o| self.loans_live.contains(&o.loan))
            .filter(|o| {
                !o.loan
                    .free_variables()
                    .iter()
                    .any(|v| variables.contains(v))
            })
            .map(|o| LoanOrigin {
                loan: o.loan.clone(),
                outlives_at_issue: o
                    .outlives_at_issue
                    .iter()
                    .filter(|e| !e.free_variables().iter().any(|v| variables.contains(v)))
                    .cloned()
                    .collect(),
            })
            .collect();
    }
}

impl UpcastFrom<Union<(PointFlowState, PointFlowState)>> for PointFlowState {
    fn upcast_from(term: Union<(PointFlowState, PointFlowState)>) -> Self {
        let Union((a, b)) = term;

        Self {
            outlives: Union((a.outlives, b.outlives)).upcast(),
            loans_live: Union((a.loans_live, b.loans_live)).upcast(),
            uninit: Union((a.uninit, b.uninit)).upcast(),
            loan_origins: join_loan_origins(a.loan_origins, b.loan_origins),
        }
    }
}

/// Join the recorded loan origins from two incoming paths.
///
/// A loan reaching the join on both paths keeps the *intersection* of the two
/// origin sets: an edge is only "older than the loan" if it was older on every
/// path that gets here, otherwise there is a path on which it can still carry
/// the loan. A loan that only flows in along one path keeps that path's set,
/// since the other path never issued it.
///
/// Intersection is also what makes the loop rule's fixed point converge: a
/// second pass through a loop body re-issues the same loan against a larger
/// outlives set, and intersecting pins the entry back to the first pass's.
fn join_loan_origins(a: Set<LoanOrigin>, b: Set<LoanOrigin>) -> Set<LoanOrigin> {
    let mut result = Set::new();

    for origin_a in &a {
        let outlives_at_issue = match b.iter().find(|o| o.loan == origin_a.loan) {
            Some(origin_b) => origin_a
                .outlives_at_issue
                .intersection(&origin_b.outlives_at_issue)
                .cloned()
                .collect(),
            None => origin_a.outlives_at_issue.clone(),
        };
        result.insert(LoanOrigin {
            loan: origin_a.loan.clone(),
            outlives_at_issue,
        });
    }

    for origin_b in b {
        if !a.iter().any(|o| o.loan == origin_b.loan) {
            result.insert(origin_b.clone());
        }
    }

    result
}

impl<A, B> UpcastFrom<Union<(A, B)>> for FlowState
where
    A: Upcast<FlowState>,
    B: Upcast<FlowState>,
{
    fn upcast_from(term: Union<(A, B)>) -> Self {
        let Union((a, b)) = term;
        let a: FlowState = a.upcast();
        let b: FlowState = b.upcast();

        // At join points, locals (for name resolution) must be identical on both sides.
        // Only drop_locals may differ (due to `let 'a: x = ...` in one branch).
        let mut scopes = a.scopes;
        assert_eq!(
            scopes.len(),
            b.scopes.len(),
            "scope stacks must have same length at join points"
        );
        for (scope_a, scope_b) in scopes.iter_mut().zip(b.scopes.iter()) {
            scope_a.union_drop_locals(scope_b);
        }

        Self {
            current: Union((a.current, b.current)).upcast(),
            breaks: Union((a.breaks, b.breaks)).upcast(),
            continues: Union((a.continues, b.continues)).upcast(),
            all_outlives: Union((a.all_outlives, b.all_outlives)).upcast(),
            scopes,
        }
    }
}

/// Flow state captured at a control transfer (break or continue) targeting a labeled scope.
#[term]
pub struct LabeledFlowState {
    /// The label being targeted
    pub label: LabelId,

    /// The flow state at the point of the control transfer
    pub state: PointFlowState,
}

/// Represents flow-sensitive state propagated by the borrow checker
#[term]
#[derive(Default)]
pub struct FlowState {
    /// Stack of scopes (blocks, loops) currently in scope.
    /// Each scope tracks its local variables (with types) for drop and type lookup.
    pub scopes: Vec<Scope>,

    /// State at the end of the current program point
    pub current: PointFlowState,

    /// State to be incorporated at the end of the labeled block/loop/etc
    pub breaks: Set<LabeledFlowState>,

    /// State to be incorporated at a continue targeting the labeled block/loop/etc
    pub continues: Set<LabeledFlowState>,

    pub all_outlives: Set<PendingOutlives>,
}

impl FlowState {
    /// Create an initial FlowState for a function body with input args as locals
    /// in an initial scope.
    pub fn for_fn_body(env: &Env, input_args: &[InputArg]) -> Fallible<Self> {
        let mut this = FlowState {
            scopes: vec![Scope {
                max_universe: env.max_universe(),
                label: None,
                break_live_places: LivePlaces::default(),
                continue_live_places: None,
                locals: Default::default(),
                drop_places: Default::default(),
            }],
            current: Default::default(),
            ..Default::default()
        };

        for input_arg in input_args {
            this = this.with_local_in_scope(env, &None, &input_arg.id, &input_arg.ty)?;
        }

        Ok(this)
    }

    pub fn with_loan(&self, loan: Loan) -> Self {
        Self {
            current: self.current.with_loan(loan),
            breaks: self.breaks.clone(),
            continues: self.continues.clone(),
            scopes: self.scopes.clone(),
            all_outlives: self.all_outlives.clone(),
        }
    }

    pub fn with_outlives(&self, outlives: &Set<PendingOutlives>) -> Self {
        Self {
            current: self.current.with_outlives(outlives),
            breaks: self.breaks.clone(),
            continues: self.continues.clone(),
            scopes: self.scopes.clone(),
            all_outlives: Union((&self.all_outlives, outlives)).upcast(),
        }
    }

    /// The types of every local currently in scope.
    ///
    /// Used to decide how a region is used (see `variance_of_lifetime_in_ty`),
    /// the counterpart of rustc's `live_region_variances`.
    pub fn local_types(&self) -> impl Iterator<Item = &Ty> {
        self.scopes
            .iter()
            .flat_map(|scope| scope.locals.iter().map(|(_, ty)| ty))
    }

    pub fn with_initialized(&self, place: &PlaceExpr) -> Self {
        let mut this = self.clone();
        this.current.mark_initialized(place);
        this
    }

    pub fn with_uninit(&self, place: &PlaceExpr) -> Self {
        let mut this = self.clone();
        this.current.mark_uninit(place);
        this
    }

    /// Returns a new flow state with `current` cleared to default.
    /// Used after diverging statements (return, break, continue) since
    /// code after them is dead and cannot observe any loans or
    /// generate meaningful outlives constraints.
    pub fn diverges(&self) -> Self {
        Self {
            current: PointFlowState::default(),
            ..self.clone()
        }
    }

    pub fn with_break(&self, label: &LabelId) -> Self {
        let mut this = self.clone();
        this.breaks.insert(LabeledFlowState {
            label: label.clone(),
            state: this.current.clone(),
        });
        this
    }

    pub fn with_continue(&self, label: &LabelId) -> Self {
        let mut this = self.clone();
        this.continues.insert(LabeledFlowState {
            label: label.clone(),
            state: this.current.clone(),
        });
        this
    }

    /// Push a new scope.
    ///
    /// # Parameters
    ///
    /// * `env` -- the environment
    /// * `label` -- the label
    /// * `break_live_places` -- the set of places live after this scope exits.
    pub fn push_scope(
        &self,
        env: &Env,
        label: &Option<Label>,
        break_live_places: impl Upcast<LivePlaces>,
    ) -> Fallible<Self> {
        self.with_scope_common(env, label, break_live_places.upcast(), None)
    }

    /// Push a new scope that can be targeted by `continue`.
    ///
    /// # Parameters
    ///
    /// * `env` -- the environment
    /// * `label` -- the label
    /// * `continue_live_places` -- the set of places live when this scope is continued.
    /// * `break_live_places` -- the set of places live after this scope exits.
    pub fn push_continue_scope(
        &self,
        env: &Env,
        label: &Option<Label>,
        break_live_places: impl Upcast<LivePlaces>,
        continue_live_places: impl Upcast<LivePlaces>,
    ) -> Fallible<Self> {
        self.with_scope_common(
            env,
            label,
            break_live_places.upcast(),
            Some(continue_live_places.upcast()),
        )
    }

    fn with_scope_common(
        &self,
        env: &Env,
        label: &Option<Label>,
        break_live_places: LivePlaces,
        continue_live_places: Option<LivePlaces>,
    ) -> Fallible<Self> {
        let mut this = self.clone();
        let label_id = label.as_ref().map(|l| l.id.clone());
        if let Some(id) = &label_id {
            if this.scopes.iter().any(|s| s.has_label(id)) {
                anyhow::bail!("shadowed label `{id:?}`");
            }
        }
        this.scopes.push(Scope {
            max_universe: env.max_universe(),
            label: label_id,
            break_live_places: break_live_places.upcast(),
            continue_live_places: continue_live_places.upcast(),
            locals: Vec::new(),
            drop_places: Vec::new(),
        });
        Ok(this)
    }

    /// Add a local variable declaration.
    ///
    /// - `locals` (for type lookup) always goes to the **innermost** scope.
    /// - `drop_locals` (for drop tracking) goes to the **named scope** if labeled,
    ///   or the **innermost** scope if unlabeled.
    ///
    /// The universe check uses the drop scope, since that's where the variable
    /// lives for lifetime purposes.
    pub fn with_local_in_scope(
        &self,
        env: &Env,
        label: &Option<Label>,
        id: &ValueId,
        ty: &Ty,
    ) -> Fallible<Self> {
        assert!(env.encloses(ty));
        let mut this = self.clone();

        // Find the drop scope (named or innermost) and check universe constraints
        let drop_scope = match label {
            Some(label) => this
                .scopes
                .iter_mut()
                .rev()
                .find(|s| s.has_label(&label.id))
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "no scope with label `{:?}` to add local `{id:?}` to",
                        label.id
                    )
                })?,
            None => this
                .scopes
                .last_mut()
                .ok_or_else(|| anyhow::anyhow!("no scope to add local `{id:?}` to"))?,
        };

        // check that the type of this variable is limited to variables that are in scope
        for v in ty.free_variables() {
            let universe = env.universe(v);
            anyhow::ensure!(
                drop_scope.max_universe.contains(universe),
                "local `{id:?}` references variable `{v:?}` that is not in scope",
                v = v,
            );
        }

        drop_scope.drop_places.push(TypedPlaceExpr::new(ty, id));

        // locals (for type lookup) always go to the innermost scope
        let innermost = this
            .scopes
            .last_mut()
            .ok_or_else(|| anyhow::anyhow!("no scope to add local `{id:?}` to"))?;
        innermost.locals.push((id.clone(), ty.clone()));

        Ok(this)
    }

    /// Look up a local variable's type by searching scopes from innermost to outermost.
    pub fn local_variable(&self, id: &ValueId) -> Fallible<Ty> {
        for scope in self.scopes.iter().rev() {
            for (local_id, ty) in scope.locals.iter().rev() {
                if local_id == id {
                    return Ok(ty.clone());
                }
            }
        }
        anyhow::bail!("unknown local variable `{id:?}`")
    }

    /// Check if any scope contains a local with this id.
    pub fn has_local(&self, id: &ValueId) -> bool {
        self.scopes
            .iter()
            .any(|s| s.locals.iter().any(|(local_id, _)| local_id == id))
    }

    /// Check if any scope has the given label.
    pub fn scope_has_label(&self, label: &LabelId) -> bool {
        self.scopes.iter().any(|s| s.has_label(label))
    }

    /// Check if the scope with the given label is a loop scope (has continue_live_places).
    pub fn live_after_continue(&self, label: &LabelId) -> Option<LivePlaces> {
        self.scopes
            .iter()
            .filter(|s| s.has_label(label))
            .find_map(|s| s.continue_live_places.clone())
    }

    /// Returns the locals that would be dropped when exiting to the scope with the given label,
    /// from innermost scope outward, each scope's drop_locals in reverse declaration order (LIFO).
    /// Includes the drop_locals of the target scope itself.
    pub fn locals_dropped_to_label(&self, label: &LabelId) -> Vec<TypedPlaceExpr> {
        let mut dropped = Vec::new();
        for scope in self.scopes.iter().rev() {
            for place in scope.drop_places.iter().rev() {
                dropped.push(place.clone());
            }
            if scope.has_label(label) {
                break;
            }
        }
        dropped
    }

    /// Returns the locals that would be dropped when the innermost scope exits,
    /// in reverse declaration order (LIFO).
    pub fn locals_dropped_in_innermost_scope(&self) -> Vec<TypedPlaceExpr> {
        self.scopes
            .last()
            .map(|scope| scope.drop_places.iter().rev().cloned().collect())
            .unwrap_or_default()
    }

    /// Removes the scope from the top of the stack and checks that its label
    /// matches `expected_label`. Any control-flow that is pending in the `breaks`
    /// set for a block with this label is incorporated into the current state.
    pub fn pop_scope(&self, expected_label: &Option<Label>) -> Self {
        let Self {
            mut scopes,
            current,
            breaks,
            continues,
            all_outlives,
        } = self.clone();

        // Pop and destructure the top scope.
        let Some(scope) = scopes.pop() else {
            panic!("no scope to pop")
        };
        let Scope {
            label: scope_label,
            break_live_places: _,
            continue_live_places: _,
            locals,
            drop_places: _,
            max_universe: _,
        } = scope;

        // Assert it has the expected label.
        assert_eq!(
            scope_label.as_ref(),
            expected_label.as_ref().map(|l| &l.id),
            "popped scope label does not match expected label"
        );

        // Extract any breaks that targeted the (new popped) scope
        // and integrate them into the current state.
        let (this_label, other_labels): (Set<LabeledFlowState>, Set<LabeledFlowState>) = breaks
            .into_iter()
            .partition(|lfs| Some(&lfs.label) == scope_label.as_ref());
        let mut successor = current;
        for lfs in this_label {
            successor = Union((successor, lfs.state)).upcast();
        }

        // Remove locals going out of scope from the uninit set
        for (id, _) in &locals {
            successor.uninit.remove(&PlaceExpr::Var(id.clone()));
        }

        FlowState {
            scopes,
            current: successor,
            breaks: other_labels,
            continues,
            all_outlives,
        }
    }

    pub fn merge_continues(&self, label: &Option<Label>) -> Self {
        let Some(Label { id: label }) = label else {
            return self.clone();
        };

        let Self {
            scopes,
            current: mut successor,
            breaks,
            continues,
            all_outlives,
        } = self.clone();

        let (this_label, other_labels): (Set<LabeledFlowState>, Set<LabeledFlowState>) =
            continues.into_iter().partition(|lfs| &lfs.label == label);

        for lfs in this_label {
            successor = Union((successor, lfs.state)).upcast();
        }

        FlowState {
            scopes,
            current: successor,
            breaks,
            continues: other_labels,
            all_outlives,
        }
    }

    /// Given a set of variables `v` created via [`Env::instantiate_universally`][]
    /// or [`Env::instantiate_existentially`][], removes `v` and all variables created *since* `v`
    /// from the environment and from the substitution.
    pub fn pop_subst<V>(&self, env: &Env, v: &[V]) -> Self
    where
        V: Upcast<Variable> + Copy,
    {
        let mut env = env.clone();
        let mut this = self.clone();

        let removed = env.pop_vars(v);
        this.current.pop_vars(&removed);
        this.breaks = this
            .breaks
            .into_iter()
            .map(|mut lfs| {
                lfs.state.pop_vars(&removed);
                lfs
            })
            .collect();
        this.continues = this
            .continues
            .into_iter()
            .map(|mut lfs| {
                lfs.state.pop_vars(&removed);
                lfs
            })
            .collect();
        this.all_outlives
            .retain(|v| !v.free_variables().iter().any(|v| removed.contains(v)));

        this
    }

    /// Check structural invariants on the scope stack:
    /// 1. No ValueId appears in more than one scope's `locals`
    /// 2. No ValueId appears in more than one scope's `drop_locals`
    /// 3. Every id across all scopes' `locals` appears in some scope's `drop_locals`
    pub fn check_invariants(&self) -> bool {
        let mut all_local_places: Set<PlaceExpr> = Default::default();
        let mut all_drop_places: Set<PlaceExpr> = Default::default();

        for scope in &self.scopes {
            for (id, _ty) in &scope.locals {
                if !all_local_places.insert(id.upcast()) {
                    panic!("local `{id:?}` appears in multiple scopes' locals");
                }
            }
            for expr in &scope.drop_places {
                if !all_drop_places.insert(expr.to_place_expression()) {
                    panic!("local `{expr:?}` appears in multiple scopes' drop_locals");
                }
            }
        }

        for id in &all_local_places {
            assert!(
                all_drop_places.contains(id),
                "local `{id:?}` is in locals but not in any scope's drop_locals"
            );
        }

        true
    }

    /// For NLL borrowck, we rerun borrowck on `exists` blocks with the full set
    /// of outlives constraints collected during the first pass. This function
    /// combines the initial entry state with the full set of outlives
    /// constraints collected over the program.
    pub(crate) fn with_global_outlives_from(&self, exit_state: &FlowState) -> Self {
        let all_outlives: Set<PendingOutlives> =
            Union((&self.all_outlives, &exit_state.all_outlives)).upcast();
        let current = PointFlowState {
            outlives: all_outlives.clone(),
            loans_live: self.current.loans_live.clone(),
            uninit: self.current.uninit.clone(),
            loan_origins: self.current.loan_origins.clone(),
        };
        FlowState {
            scopes: self.scopes.clone(),
            current,
            breaks: self.breaks.clone(),
            continues: self.continues.clone(),
            all_outlives,
        }
    }
}

/// A pending outlives constraint that we incurred during typechecking.
#[term]
pub struct PendingOutlives {
    /// The `a` in `a: b`
    pub a: Parameter,

    /// The `b` in `a: b`
    pub b: Parameter,
}

/// The outlives edges that were already in force when a loan was issued.
///
/// Recorded so that loan propagation can be location-sensitive: only edges
/// created *after* a loan can carry it into further regions. See
/// [`PointFlowState::outlives_after_loan`].
#[term]
pub struct LoanOrigin {
    /// The loan this origin belongs to.
    pub loan: Loan,

    /// The outlives edges in force at the point `loan` was issued.
    pub outlives_at_issue: Set<PendingOutlives>,
}

/// Represents a loan that resulted from executing a borrow expression like `&'0 place`.
#[term]
pub struct Loan {
    /// The region `'0` of the resulting reference from this borrow.
    pub lt: Lt,

    /// The place being borrowed.
    pub place: TypedPlaceExpr,

    /// The kind of borrow (shared, mutable, etc).
    pub kind: RefKind,
}
