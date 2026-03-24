use crate::{
    check::borrow_check::env::TypeckEnv,
    check::borrow_check::flow_state::FlowState,
    grammar::expr::{Block, Expr, ExprData, FieldExpr, Init, Label, LabelId, PlaceExpr, Stmt},
};
use formality_core::{Set, Upcast};

pub type LivePlaces = Set<PlaceExpr>;

/// Liveness context for control-flow targets (break/continue).
/// Each entry corresponds to a labeled scope (block or loop).
#[derive(Clone, Debug)]
pub struct LivenessContext {
    scopes: Vec<LivenessScope>,
}

#[derive(Clone, Debug)]
struct LivenessScope {
    label: Option<LabelId>,
    /// Places live after this scope exits (break target).
    break_live: LivePlaces,
    /// Places live at loop entry (continue target). `None` for non-loop scopes.
    continue_live: Option<LivePlaces>,
}

impl LivenessContext {
    pub fn empty() -> Self {
        LivenessContext { scopes: Vec::new() }
    }

    /// Look up the break target liveness for the given label.
    fn break_live(&self, label: &LabelId) -> LivePlaces {
        self.scopes
            .iter()
            .rev()
            .find(|s| s.label.as_ref() == Some(label))
            .map(|s| s.break_live.clone())
            .unwrap_or_default()
    }

    /// Look up the continue target liveness for the given label.
    fn continue_live(&self, label: &LabelId) -> LivePlaces {
        self.scopes
            .iter()
            .rev()
            .find(|s| s.label.as_ref() == Some(label))
            .and_then(|s| s.continue_live.clone())
            .unwrap_or_default()
    }

    /// Push a loop scope and return a new context.
    fn with_loop(
        &self,
        label: &Option<Label>,
        break_live: LivePlaces,
        continue_live: LivePlaces,
    ) -> Self {
        let mut ctx = self.clone();
        ctx.scopes.push(LivenessScope {
            label: label.as_ref().map(|l| l.id.clone()),
            break_live,
            continue_live: Some(continue_live),
        });
        ctx
    }

    /// Update the continue liveness for the innermost scope.
    fn set_continue_live(&mut self, live: LivePlaces) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.continue_live = Some(live);
        }
    }

    /// Push a block scope (no continue target).
    fn with_block(&self, label: &Option<Label>, break_live: LivePlaces) -> Self {
        let mut ctx = self.clone();
        ctx.scopes.push(LivenessScope {
            label: label.as_ref().map(|l| l.id.clone()),
            break_live,
            continue_live: None,
        });
        ctx
    }
}

/// Trait for types that can be converted to a [`LivenessContext`].
/// This allows `live_before` to accept either a `&FlowState` or a `&LivenessContext`.
pub trait IntoLivenessContext {
    fn into_liveness_context(&self) -> LivenessContext;
}

impl IntoLivenessContext for FlowState {
    fn into_liveness_context(&self) -> LivenessContext {
        LivenessContext {
            scopes: self
                .scopes
                .iter()
                .map(|s| LivenessScope {
                    label: s.label.clone(),
                    break_live: s.break_live_places.clone(),
                    continue_live: s.continue_live_places.clone(),
                })
                .collect(),
        }
    }
}

impl<T: IntoLivenessContext> IntoLivenessContext for &T {
    fn into_liveness_context(&self) -> LivenessContext {
        T::into_liveness_context(self)
    }
}

impl IntoLivenessContext for LivenessContext {
    fn into_liveness_context(&self) -> LivenessContext {
        self.clone()
    }
}

pub trait LiveBefore {
    /// Given a term `term` and a set of places `places_live` that are live after `term` executes,
    /// returns the set of places live BEFORE `term`. This will include all places in `places_live`
    /// minus any places overwritten by `term`, plus any places accessed.
    ///
    /// The `scopes` parameter provides liveness targets for break/continue statements.
    fn live_before(
        &self,
        env: &TypeckEnv,
        scopes: &impl IntoLivenessContext,
        places_live: impl Upcast<LivePlaces>,
    ) -> LivePlaces;
}

/// Sequential execution: `a` runs before `b`.
/// Places live before `Seq(a, b)` = places live before `a`,
/// given that places live after `a` = places live before `b`.
pub struct Seq<A, B>(pub A, pub B);

impl<A: LiveBefore, B: LiveBefore> LiveBefore for Seq<A, B> {
    fn live_before(
        &self,
        env: &TypeckEnv,
        scopes: &impl IntoLivenessContext,
        places_live: impl Upcast<LivePlaces>,
    ) -> LivePlaces {
        let Seq(a, b) = self;
        let places_live = b.live_before(env, scopes, places_live);
        a.live_before(env, scopes, places_live)
    }
}

/// Parallel paths: either `a` or `b` executes (but not both).
/// Places live before `Either(a, b)` = union of places live before each.
pub struct Either<A, B>(pub A, pub B);

impl<A: LiveBefore, B: LiveBefore> LiveBefore for Either<A, B> {
    fn live_before(
        &self,
        env: &TypeckEnv,
        scopes: &impl IntoLivenessContext,
        places_live: impl Upcast<LivePlaces>,
    ) -> LivePlaces {
        let Either(a, b) = self;
        let places_live: LivePlaces = places_live.upcast();
        let a_live = a.live_before(env, scopes, &places_live);
        let b_live = b.live_before(env, scopes, &places_live);
        a_live.union(&b_live).cloned().collect()
    }
}

impl<T: ?Sized + LiveBefore> LiveBefore for &T {
    fn live_before(
        &self,
        env: &TypeckEnv,
        scopes: &impl IntoLivenessContext,
        places_live: impl Upcast<LivePlaces>,
    ) -> LivePlaces {
        T::live_before(self, env, scopes, places_live)
    }
}

impl<T: LiveBefore> LiveBefore for [T] {
    fn live_before(
        &self,
        env: &TypeckEnv,
        scopes: &impl IntoLivenessContext,
        places_live: impl Upcast<LivePlaces>,
    ) -> LivePlaces {
        let mut places_live: LivePlaces = places_live.upcast();
        for element in self.iter().rev() {
            places_live = T::live_before(element, env, scopes, places_live);
        }
        places_live
    }
}

impl<T: LiveBefore> LiveBefore for Vec<T> {
    fn live_before(
        &self,
        env: &TypeckEnv,
        scopes: &impl IntoLivenessContext,
        places_live: impl Upcast<LivePlaces>,
    ) -> LivePlaces {
        <[T]>::live_before(self, env, scopes, places_live)
    }
}

impl<T: LiveBefore> LiveBefore for Option<T> {
    fn live_before(
        &self,
        env: &TypeckEnv,
        scopes: &impl IntoLivenessContext,
        places_live: impl Upcast<LivePlaces>,
    ) -> LivePlaces {
        let places_live = places_live.upcast();
        match self {
            Some(t) => t.live_before(env, scopes, places_live),
            None => places_live,
        }
    }
}

impl LiveBefore for Stmt {
    fn live_before(
        &self,
        env: &TypeckEnv,
        scopes: &impl IntoLivenessContext,
        places_live: impl Upcast<LivePlaces>,
    ) -> LivePlaces {
        let places_live: LivePlaces = places_live.upcast();
        match self {
            Stmt::Let {
                label: _,
                id,
                ty: _,
                init,
            } => {
                let places_live = Assignment(id).live_before(env, scopes, places_live);
                init.live_before(env, scopes, places_live)
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                Seq(condition, Either(then_block, else_block)).live_before(env, scopes, places_live)
            }
            Stmt::Expr { expr } => expr.live_before(env, scopes, places_live),
            Stmt::Loop { label, body } => {
                //
                //             Before  .............. places_live
                //               |     :            :
                //               v     v            :
                //     +----->  Loop ----> After    :
                //     |         |            ^     :
                //     |         v            | <....
                //  continue -- Body --break--+
                //
                // So `places_live` is the liveness on exit from the
                // loop. The liveness on entry to the loop begins
                // as `places_live` but then we compute the liveness
                // of with the body taken into account.
                //
                // Meanwhile, the liveness on entry to the loop is the
                // union of after + body.
                //
                // We also record the liveness on exit / on loop entry
                // for use with embedded break/continue.
                let ctx = scopes.into_liveness_context();
                // Fixed-point iteration: start with places_live (live after loop)
                // as the initial approximation for both break and continue targets.
                // We assume all loops might not execute, so the loop entry starts
                // with the same liveness as the exit.
                let mut ctx = ctx.with_loop(label, places_live.clone(), places_live.clone());
                loop {
                    let new_live = body.live_before(env, &ctx, &places_live);
                    let prev_continue = ctx.scopes.last().unwrap().continue_live.clone();
                    if prev_continue.as_ref() == Some(&new_live) {
                        // Union with places_live for the zero-iteration path:
                        // the loop might not execute, so anything live after
                        // the loop is also live before it.
                        break new_live.union(&places_live).cloned().collect();
                    }
                    ctx.set_continue_live(new_live);
                }
            }
            Stmt::Break { label } => {
                let ctx = scopes.into_liveness_context();
                ctx.break_live(label)
            }
            Stmt::Continue { label } => {
                let ctx = scopes.into_liveness_context();
                ctx.continue_live(label)
            }
            Stmt::Return { expr } => {
                // The return expression is evaluated, but nothing after return is live.
                expr.live_before(env, scopes, LivePlaces::default())
            }
            Stmt::Block(block) => block.live_before(env, scopes, places_live),
            Stmt::Exists { binder } => {
                let (_vars, block) = binder.open();
                block.live_before(env, scopes, places_live)
            }
        }
    }
}

impl LiveBefore for Expr {
    fn live_before(
        &self,
        env: &TypeckEnv,
        scopes: &impl IntoLivenessContext,
        places_live: impl Upcast<LivePlaces>,
    ) -> LivePlaces {
        let places_live: LivePlaces = places_live.upcast();
        match self.data() {
            // place = expr: the assignment kills `place`, then we need liveness of `expr`.
            ExprData::Assign { place, expr } => {
                Seq(expr, Assignment(place)).live_before(env, scopes, places_live)
            }

            // call callee(args): callee and args are all evaluated (callee first, then args left-to-right).
            ExprData::Call { callee, args } => {
                Seq(callee, args).live_before(env, scopes, places_live)
            }

            // Literals don't access any places.
            ExprData::Literal { .. } | ExprData::True | ExprData::False => places_live,

            // &expr or &mut expr: the operand place is accessed.
            ExprData::Ref {
                kind: _,
                lt: _,
                place: expr,
            } => expr.live_before(env, scopes, places_live),

            // A place expression used as a value: the place is live.
            ExprData::Place(place_expr) => place_expr.live_before(env, scopes, places_live),

            // Struct { field_exprs }: each field expression is evaluated.
            ExprData::Struct {
                field_exprs,
                adt_id: _,
                turbofish: _,
            } => field_exprs.live_before(env, scopes, places_live),

            // Turbofish is just a variable reference with explicit type args —
            // the variable itself is live.
            ExprData::Turbofish { id, args: _ } => {
                let place_expr: PlaceExpr = id.upcast();
                place_expr.live_before(env, scopes, places_live)
            }
        }
    }
}

impl LiveBefore for Init {
    fn live_before(
        &self,
        env: &TypeckEnv,
        scopes: &impl IntoLivenessContext,
        places_live: impl Upcast<LivePlaces>,
    ) -> LivePlaces {
        self.expr.live_before(env, scopes, places_live)
    }
}

impl LiveBefore for FieldExpr {
    fn live_before(
        &self,
        env: &TypeckEnv,
        scopes: &impl IntoLivenessContext,
        places_live: impl Upcast<LivePlaces>,
    ) -> LivePlaces {
        self.value.live_before(env, scopes, places_live)
    }
}

impl LiveBefore for Block {
    fn live_before(
        &self,
        env: &TypeckEnv,
        scopes: &impl IntoLivenessContext,
        places_live: impl Upcast<LivePlaces>,
    ) -> LivePlaces {
        let places_live: LivePlaces = places_live.upcast();
        self.stmts.live_before(env, scopes, places_live)
    }
}

impl LiveBefore for PlaceExpr {
    fn live_before(
        &self,
        _env: &TypeckEnv,
        _scopes: &impl IntoLivenessContext,
        places_live: impl Upcast<LivePlaces>,
    ) -> LivePlaces {
        let mut places_live = places_live.upcast();
        places_live.insert(self.clone());
        places_live
    }
}

/// Given a place expression `place_assigned` and a set of places `places_live` that are live after `place_assigned`
/// is assigned, returns the set of places live BEFORE the assignment. This will include all places in `places_live`
/// except those that are overwritten by assigning to `place_assigned`.
///
/// This is because whatever value resides in `place_assigned` before the assignment is not relevant afterwards.
pub struct Assignment<P>(pub P);

impl<P> LiveBefore for Assignment<P>
where
    P: Upcast<PlaceExpr> + Clone,
{
    fn live_before(
        &self,
        _env: &TypeckEnv,
        _scopes: &impl IntoLivenessContext,
        places_live: impl Upcast<LivePlaces>,
    ) -> LivePlaces {
        let mut places_live = places_live.upcast();
        let place_expr: PlaceExpr = self.0.clone().upcast();
        places_live.retain(|p| !place_expr.is_prefix_of(p));
        places_live
    }
}
