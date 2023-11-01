use crate::decls::Decls;
use formality_core::{set, Set, Upcast};
use formality_types::rust::Term;

use super::{Constraints, Env};

pub fn zip<A, B, C>(
    decls: &Decls,
    env: &Env,
    context: &C,
    mut a: Vec<A>,
    mut b: Vec<B>,
    op: &impl Fn(Decls, Env, C, A, B) -> Set<Constraints>,
) -> Set<Constraints>
where
    A: Term,
    B: Term,
    C: Term,
{
    assert_eq!(a.len(), b.len());

    if a.is_empty() && b.is_empty() {
        return set![Constraints::none(env.upcast())];
    }

    let a0 = a.remove(0);
    let b0 = b.remove(0);
    op(decls.clone(), env.clone(), context.clone(), a0, b0)
        .into_iter()
        .flat_map(|c1| {
            let context = c1.substitution().apply(context);
            let a = c1.substitution().apply(&a);
            let b = c1.substitution().apply(&b);
            zip(decls, c1.env(), &context, a, b, op)
                .into_iter()
                .map(move |c2| c1.seq(c2))
        })
        .collect()
}

pub fn for_all<A, C>(
    decls: &Decls,
    env: &Env,
    context: &C,
    a: &[A],
    op: &impl Fn(Decls, Env, C, A) -> Set<Constraints>,
) -> Set<Constraints>
where
    A: Term,
    C: Term,
{
    if a.is_empty() {
        return set![Constraints::none(env.upcast())];
    }

    let a0 = a[0].clone();
    let a_remaining: Vec<A> = a[1..].to_vec();
    op(decls.clone(), env.clone(), context.clone(), a0)
        .into_iter()
        .flat_map(|c1| {
            let context = c1.substitution().apply(context);
            let a_remaining = c1.substitution().apply(&a_remaining);
            for_all(decls, c1.env(), &context, &a_remaining, op)
                .into_iter()
                .map(move |c2| c1.seq(c2))
        })
        .collect()
}
