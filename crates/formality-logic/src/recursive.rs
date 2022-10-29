use formality_infer::{query::Query, Env};
use formality_types::{
    db::Db,
    grammar::{Goal, Hypothesis},
};

pub fn solve(db: &Db, query: &Query) {
    let _solver = RecursiveSolver {
        db: db.clone(),
        frames: vec![Frame {
            query: query.clone(),
            result: RecursiveResult::No,
            env: query.env.clone(),
            assumptions: vec![], // FIXME
            goal: query.goal.clone(),
        }],
    };
}

struct RecursiveSolver {
    db: Db,
    frames: Vec<Frame>,
}

struct Frame {
    query: Query,
    result: RecursiveResult,
    env: Env,
    assumptions: Vec<Hypothesis>,
    goal: Goal,
}

enum RecursiveResult {
    No,
    Yes,
    Ambiguous,
}
