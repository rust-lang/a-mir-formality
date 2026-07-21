use crate::prove::prove::decls::Program;
use crate::rust::Term;
use formality_core::judgment::ProofTree;
use formality_core::ProvenSet;

use super::Env;

pub fn zip<A, B, C>(
    decls: &Program,
    env: &Env,
    context: &C,
    mut a: Vec<A>,
    mut b: Vec<B>,
    op: &impl Fn(Program, Env, C, A, B) -> ProvenSet<Env>,
) -> ProvenSet<Env>
where
    A: Term,
    B: Term,
    C: Term,
{
    assert_eq!(a.len(), b.len());

    if a.is_empty() && b.is_empty() {
        let leaf = ProofTree::leaf(format!("{:?}", env));
        return ProvenSet::singleton((env.clone(), leaf));
    }

    let a0 = a.remove(0);
    let b0 = b.remove(0);
    op(decls.clone(), env.clone(), context.clone(), a0, b0).flat_map(|(c1, tree1)| {
        let context = c1.substitution().apply(context);
        let a = c1.substitution().apply(&a);
        let b = c1.substitution().apply(&b);
        zip(decls, &c1, &context, a, b, op).map(move |(c2, tree2)| {
            (
                c1.seq(&c2),
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
    op: &impl Fn(Program, Env, C, A) -> ProvenSet<Env>,
) -> ProvenSet<Env>
where
    A: Term,
    C: Term,
{
    if a.is_empty() {
        let leaf = ProofTree::leaf(format!("{:?}", env));
        return ProvenSet::singleton((env.clone(), leaf));
    }

    let a0 = a[0].clone();
    let a_remaining: Vec<A> = a[1..].to_vec();
    op(decls.clone(), env.clone(), context.clone(), a0).flat_map(|(c1, tree1)| {
        let context = c1.substitution().apply(context);
        let a_remaining = c1.substitution().apply(&a_remaining);
        for_all(decls, &c1, &context, &a_remaining, op).map(move |(c2, tree2)| {
            (
                c1.seq(&c2),
                ProofTree::new("for_all", None, vec![tree1.clone(), tree2.clone()]),
            )
        })
    })
}
