use formality_core::{fixed_point, Set, SetExt, Upcast};
use formality_rust::grammar::minirust::{
    ArgumentExpression, BasicBlock, BbId, PlaceExpression, Statement, Terminator, ValueExpression,
};

use crate::mini_rust_check::TypeckEnv;

pub type LivePlaces = Set<PlaceExpression>;

/// Given a basic-block id, returns the places live on entry to the basic block.
#[fixed_point]
fn places_live_before_basic_block(env: &TypeckEnv, bb_id: &BbId) -> LivePlaces {
    let BasicBlock {
        id: _,
        statements,
        terminator,
    } = env.basic_block(bb_id).expect("valid basic block id");

    let places = places_live_before_terminator(env, terminator);
    let places = statements.live_before(env, places);
    places
}

pub fn places_live_before_basic_blocks<'a>(
    env: &TypeckEnv,
    bb_ids: impl IntoIterator<Item = &'a BbId>,
) -> LivePlaces {
    bb_ids
        .into_iter()
        .flat_map(|bb_id| places_live_before_basic_block(env, bb_id))
        .collect()
}

/// Returns the places that are live before the terminator executes.
pub fn places_live_before_terminator(env: &TypeckEnv, terminator: &Terminator) -> LivePlaces {
    match terminator {
        Terminator::Goto(bb_id) => places_live_before_basic_block(env, bb_id),
        Terminator::Switch {
            switch_value,
            switch_targets,
            fallback,
        } => {
            let mut places = places_live_before_basic_block(env, fallback);
            for target in switch_targets {
                places = places.union_with(places_live_before_basic_block(env, &target.target));
            }
            places = switch_value.live_before(env, places);
            places
        }
        Terminator::Call {
            callee,
            generic_arguments: _,
            arguments,
            ret,
            next_block,
        } => {
            let mut places_live = match next_block {
                Some(bb_id) => places_live_before_basic_block(env, bb_id),
                None => LivePlaces::default(),
            };
            places_live = Assignment(ret).live_before(env, places_live);
            places_live = arguments.live_before(env, places_live);
            places_live = callee.live_before(env, places_live);
            places_live
        }
        Terminator::Return => LivePlaces::default(),
    }
}

pub trait LiveBefore {
    /// Given a term `term` and a set of places `places_live` that are live after `term` executes,
    /// returns the set of places live BEFORE `term`. This will include all places in `places_live`
    /// minus any places overwritten by `term`, plus any places accessed.
    fn live_before(&self, env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces;
}

impl<A: LiveBefore, B: LiveBefore> LiveBefore for (A, B) {
    fn live_before(&self, env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        let (a, b) = self;
        let places_live = b.live_before(env, places_live);
        a.live_before(env, places_live)
    }
}

impl<A: LiveBefore, B: LiveBefore, C: LiveBefore> LiveBefore for (A, B, C) {
    fn live_before(&self, env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        let (a, b, c) = self;
        let places_live = c.live_before(env, places_live);
        let places_live = b.live_before(env, places_live);
        a.live_before(env, places_live)
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

impl LiveBefore for Statement {
    fn live_before(&self, env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        let mut places_live = places_live.upcast();
        match self {
            Statement::Assign(place_expression, value_expression) => {
                places_live = Assignment(place_expression).live_before(env, places_live);
                places_live = value_expression.live_before(env, places_live);
                places_live
            }
            Statement::PlaceMention(place_expression) => {
                place_expression.live_before(env, places_live)
            }
            Statement::StorageLive(_) | Statement::StorageDead(_) => places_live,
        }
    }
}

impl LiveBefore for ValueExpression {
    fn live_before(&self, env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        let places_live = places_live.upcast();
        match self {
            ValueExpression::Constant(_) => places_live,
            ValueExpression::Fn(_) => places_live,
            ValueExpression::Struct(value_expressions, _ty) => {
                value_expressions.live_before(env, places_live)
            }
            ValueExpression::Load(place_expression) => {
                place_expression.live_before(env, places_live)
            }
            ValueExpression::Ref(_lt, place_expression) => {
                place_expression.live_before(env, places_live)
            }
        }
    }
}

impl LiveBefore for PlaceExpression {
    fn live_before(&self, _env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        let mut places_live = places_live.upcast();
        places_live.insert(self.clone());
        places_live
    }
}

impl LiveBefore for ArgumentExpression {
    fn live_before(&self, env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        match self {
            ArgumentExpression::ByValue(value_expression) => {
                value_expression.live_before(env, places_live)
            }
            ArgumentExpression::InPlace(place_expression) => {
                place_expression.live_before(env, places_live)
            }
        }
    }
}

/// Given a place expression `place_assigned` and a set of places `places_live` that are live after `place_assigned`
/// is assigned, returns the set of places live BEFORE the assignment. This will include all places in `places_live`
/// except those that are overwritten by assigning to `place_assigned`.
///
/// This is because whatever value resides in `place_assigned` before the assignment is not relevant afterwards.
pub struct Assignment<'a>(pub &'a PlaceExpression);

impl LiveBefore for Assignment<'_> {
    fn live_before(&self, _env: &TypeckEnv, places_live: impl Upcast<LivePlaces>) -> LivePlaces {
        let mut places_live = places_live.upcast();
        let Assignment(place_assigned) = self;
        places_live.retain(|p| !place_assigned.is_prefix_of(p));
        places_live
    }
}
