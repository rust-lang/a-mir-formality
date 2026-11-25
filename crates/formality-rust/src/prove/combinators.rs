use crate::prove::decls::Program;
use crate::rust::Term;
use formality_core::judgment::ProofTree;
use formality_core::ProvenSet;

use super::{Constraints, Env};

pub fn zip<A, B, C>(
    decls: &Program,
    env: &Env,
    context: &C,
    a: &[A],
    b: &[B],
    op: &impl Fn(Program, Env, C, A, B) -> ProvenSet<Constraints>,
) -> ProvenSet<Constraints>
where
    A: Term,
    B: Term,
    C: Term,
{
    assert_eq!(a.len(), b.len());

    if a.is_empty() && b.is_empty() {
        let constraints = Constraints::none(env);
        let leaf = ProofTree::leaf(format!("{:?}", constraints));
        return ProvenSet::singleton((constraints, leaf));
    }

    let (a0, a) = a.split_first().unwrap();
    let (b0, b) = b.split_first().unwrap();
    op(
        decls.clone(),
        env.clone(),
        context.clone(),
        a0.clone(),
        b0.clone(),
    )
    .flat_map(|(c1, tree1)| {
        let context = c1.substitution().apply(context);
        let a: Vec<_> = a.iter().map(|p| c1.substitution().apply(p)).collect();
        let b: Vec<_> = b.iter().map(|p| c1.substitution().apply(p)).collect();
        zip(decls, c1.env(), &context, &a, &b, op).map(move |(c2, tree2)| {
            (
                c1.seq(c2),
                ProofTree::new("zip", None, vec![tree1.clone(), tree2.clone()]),
            )
        })
    })
}

pub fn for_all<A, C>(
    decls: &Program,
    env: &Env,
    context: &C,
    a: &[A],
    op: &impl Fn(Program, Env, C, A) -> ProvenSet<Constraints>,
) -> ProvenSet<Constraints>
where
    A: Term,
    C: Term,
{
    if a.is_empty() {
        let constraints = Constraints::none(env);
        let leaf = ProofTree::leaf(format!("{:?}", constraints));
        return ProvenSet::singleton((constraints, leaf));
    }

    let a0 = a[0].clone();
    let a_remaining: Vec<A> = a[1..].to_vec();
    op(decls.clone(), env.clone(), context.clone(), a0).flat_map(|(c1, tree1)| {
        let context = c1.substitution().apply(context);
        let a_remaining = c1.substitution().apply(&a_remaining);
        for_all(decls, c1.env(), &context, &a_remaining, op).map(move |(c2, tree2)| {
            (
                c1.seq(c2),
                ProofTree::new("for_all", None, vec![tree1.clone(), tree2.clone()]),
            )
        })
    })
}
