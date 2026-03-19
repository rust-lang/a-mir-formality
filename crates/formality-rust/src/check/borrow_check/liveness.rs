use crate::{
    check::borrow_check::env::TypeckEnv,
    grammar::expr::{Block, Expr, ExprData, FieldExpr, Init, PlaceExpr, Stmt},
};
use formality_core::{Set, Upcast};

pub type LivePlaces = Set<PlaceExpr>;

pub trait LiveBefore {
    /// Given a term `term` and a set of places `places_live` that are live after `term` executes,
    /// returns the set of places live BEFORE `term`. This will include all places in `places_live`
    /// minus any places overwritten by `term`, plus any places accessed.
    fn live_before(&self, env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces;
}

/// Sequential execution: `a` runs before `b`.
/// Places live before `Seq(a, b)` = places live before `a`,
/// given that places live after `a` = places live before `b`.
pub struct Seq<A, B>(pub A, pub B);

impl<A: LiveBefore, B: LiveBefore> LiveBefore for Seq<A, B> {
    fn live_before(&self, env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        let Seq(a, b) = self;
        let places_live = b.live_before(env, places_live);
        a.live_before(env, places_live)
    }
}

/// Parallel paths: either `a` or `b` executes (but not both).
/// Places live before `Either(a, b)` = union of places live before each.
pub struct Either<A, B>(pub A, pub B);

impl<A: LiveBefore, B: LiveBefore> LiveBefore for Either<A, B> {
    fn live_before(&self, env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        let Either(a, b) = self;
        let places_live: LivePlaces = places_live.upcast();
        let a_live = a.live_before(env, &places_live);
        let b_live = b.live_before(env, &places_live);
        a_live.union(&b_live).cloned().collect()
    }
}

impl<T: ?Sized + LiveBefore> LiveBefore for &T {
    fn live_before(&self, env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        T::live_before(self, env, places_live)
    }
}

impl<T: LiveBefore> LiveBefore for [T] {
    fn live_before(&self, env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        let mut places_live: LivePlaces = places_live.upcast();
        for element in self.iter().rev() {
            places_live = T::live_before(element, env, places_live);
        }
        places_live
    }
}

impl<T: LiveBefore> LiveBefore for Vec<T> {
    fn live_before(&self, env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        <[T]>::live_before(self, env, places_live)
    }
}

impl<T: LiveBefore> LiveBefore for Option<T> {
    fn live_before(&self, env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        let places_live = places_live.upcast();
        match self {
            Some(t) => t.live_before(env, places_live),
            None => places_live,
        }
    }
}

impl LiveBefore for Stmt {
    fn live_before(&self, env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        let places_live: LivePlaces = places_live.upcast();
        match self {
            Stmt::Let {
                label: _,
                id,
                ty: _,
                init,
            } => {
                let places_live = Assignment(id).live_before(env, places_live);
                init.live_before(env, places_live)
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => Seq(condition, Either(then_block, else_block)).live_before(env, places_live),
            Stmt::Expr { expr } => expr.live_before(env, places_live),
            Stmt::Loop { label: _, body } => {
                // Fixed-point iteration: places live before the loop include
                // places live after the loop plus any places the body needs.
                let mut live = places_live;
                loop {
                    let new_live = body.live_before(env, &live);
                    if new_live == live {
                        break live;
                    }
                    live = new_live;
                }
            }
            // Divergent statements: nothing after them is reachable.
            Stmt::Break { .. } | Stmt::Continue { .. } => LivePlaces::default(),
            Stmt::Return { expr } => {
                // The return expression is evaluated, but nothing after return is live.
                expr.live_before(env, LivePlaces::default())
            }
            Stmt::Block(block) => block.live_before(env, places_live),
            Stmt::Exists { binder } => {
                let (_vars, block) = binder.open();
                block.live_before(env, places_live)
            }
        }
    }
}

impl LiveBefore for Expr {
    fn live_before(&self, env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        let places_live: LivePlaces = places_live.upcast();
        match self.data() {
            // place = expr: the assignment kills `place`, then we need liveness of `expr`.
            ExprData::Assign { place, expr } => {
                Seq(expr, Assignment(place)).live_before(env, places_live)
            }

            // call callee(args): callee and args are all evaluated (callee first, then args left-to-right).
            ExprData::Call { callee, args } => Seq(callee, args).live_before(env, places_live),

            // Literals don't access any places.
            ExprData::Literal { .. } | ExprData::True | ExprData::False => places_live,

            // &expr or &mut expr: the operand place is accessed.
            ExprData::Ref {
                kind: _,
                lt: _,
                place: expr,
            } => expr.live_before(env, places_live),

            // A place expression used as a value: the place is live.
            ExprData::Place(place_expr) => place_expr.live_before(env, places_live),

            // Struct { field_exprs }: each field expression is evaluated.
            ExprData::Struct {
                field_exprs,
                adt_id: _,
                turbofish: _,
            } => field_exprs.live_before(env, places_live),

            // Turbofish is just a variable reference with explicit type args —
            // the variable itself is live.
            ExprData::Turbofish { id, args: _ } => {
                let place_expr: PlaceExpr = id.upcast();
                place_expr.live_before(env, places_live)
            }
        }
    }
}

impl LiveBefore for Init {
    fn live_before(&self, env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        self.expr.live_before(env, places_live)
    }
}

impl LiveBefore for FieldExpr {
    fn live_before(&self, env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        self.value.live_before(env, places_live)
    }
}

impl LiveBefore for Block {
    fn live_before(&self, env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        let places_live: LivePlaces = places_live.upcast();
        self.stmts.live_before(env, places_live)
    }
}

impl LiveBefore for PlaceExpr {
    fn live_before(&self, _env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
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
    fn live_before(&self, _env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        let mut places_live = places_live.upcast();
        let place_expr: PlaceExpr = self.0.clone().upcast();
        places_live.retain(|p| !place_expr.is_prefix_of(p));
        places_live
    }
}
