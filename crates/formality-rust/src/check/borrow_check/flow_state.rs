use crate::check::borrow_check::liveness::LivePlaces;
use crate::check::borrow_check::typed_place_expression::TypedPlaceExpr;
use crate::grammar::expr::{Label, LabelId, PlaceExpr};
use crate::grammar::{InputArg, Lt, Parameter, Ty, ValueId};
use crate::grammar::{RefKind, Variable};
use crate::prove::prove::{Env, MaxUniverse};
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
}

impl PointFlowState {
    pub fn with_loan(&self, loan: Loan) -> Self {
        let mut this = self.clone();
        this.loans_live.insert(loan);
        this
    }

    pub fn with_outlives(&self, outlives: &Set<PendingOutlives>) -> Self {
        PointFlowState {
            outlives: Union((&self.outlives, outlives)).upcast(),
            loans_live: self.loans_live.clone(),
        }
    }

    /// Modifies `self` by removing all variables in `variables` from the environment and from the substitution.
    fn pop_vars(&mut self, variables: &Set<Variable>) {
        self.outlives
            .retain(|v| !v.free_variables().iter().any(|v| variables.contains(v)));
        self.loans_live
            .retain(|v| !v.free_variables().iter().any(|v| variables.contains(v)));
    }
}

impl UpcastFrom<Union<(PointFlowState, PointFlowState)>> for PointFlowState {
    fn upcast_from(term: Union<(PointFlowState, PointFlowState)>) -> Self {
        let Union((a, b)) = term;

        Self {
            outlives: Union((a.outlives, b.outlives)).upcast(),
            loans_live: Union((a.loans_live, b.loans_live)).upcast(),
        }
    }
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
        }
    }

    pub fn with_outlives(&self, outlives: &Set<PendingOutlives>) -> Self {
        Self {
            current: self.current.with_outlives(outlives),
            breaks: self.breaks.clone(),
            continues: self.continues.clone(),
            scopes: self.scopes.clone(),
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
        } = self.clone();

        // Pop and destructure the top scope.
        let Some(scope) = scopes.pop() else {
            panic!("no scope to pop")
        };
        let Scope {
            label: scope_label,
            break_live_places: _,
            continue_live_places: _,
            locals: _,
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

        FlowState {
            scopes,
            current: successor,
            breaks: other_labels,
            continues,
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
        }
    }

    /// Checks that there are no entries in the `continues` set targeting
    /// the given label. Returns `true` if no such entries exist (or if
    /// `label` is `None`).
    pub fn no_continues(&self, label: &Option<Label>) -> bool {
        let Some(Label { id: label }) = label else {
            return true;
        };

        self.continues.iter().all(|lfs| &lfs.label != label)
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
}

/// A pending outlives constraint that we incurred during typechecking.
#[term]
pub struct PendingOutlives {
    /// The `a` in `a: b`
    pub a: Parameter,

    /// The `b` in `a: b`
    pub b: Parameter,
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
