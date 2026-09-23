use crate::grammar::Wc;
use crate::grammar::{Lt, Parameter, Predicate, RigidName, RigidTy, Wcs};
use crate::prove::{decls::Program, prove, prove_after::prove_after, prove_normalize, Constrained};
use formality_core::{judgment_fn, Set, Upcast};

use super::{constraints::Constraints, env::Env};

judgment_fn! {
    /// A *outlives* B if --
    ///
    /// * "as long as B is valid, A is valid"
    /// * "if A is invalidated, B *may* be invalidated"
    ///
    /// Outlives is "reflexive" -- `'a: 'a`.
    ///
    /// Examples:
    ///
    /// * `'static: 'a` -- true
    /// *
    ///
    /// Borrow check flow example
    ///
    /// ```rust,ignore
    /// fn main() {
    ///     let mut i = 22;
    ///     let p: &'?0 i32 = &i;
    ///     let q: &'?1 i32 = p;   // subtyping requires `&'?0 i32 <: &'?1: i32` requires `'?0: '?1`
    ///     if condition() {
    ///         i += 1;         // <- ok, `p` is dead
    ///     } else {
    ///         i += 1;         // <- error, `p` is live (via `q`)
    ///         println("{q}");
    ///     }
    /// }
    /// ```
    pub fn prove_outlives(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        a: Parameter,
        b: Parameter,
    ) => Constraints {
        debug(a, b, assumptions, env)

        trivial(a == b => {
            Constraints::none(env)
        })

        // 'static outlives us all
        (
            ----------------------------- ("static outlives everything")
            (prove_outlives(_decls, _env, _assumptions, Lt::Static, _b) => Constraints::none(env))
        )

        // Everything outlives 'erased
        (
            ----------------------------- ("anything outlives erased")
            (prove_outlives(_decls, _env, _assumptions, _a, Lt::Erased) => Constraints::none(env))
        )

        // 'erased outlives 'static
        (
            ----------------------------- ("erased outlives static")
            (prove_outlives(_decls, _env, _assumptions, Lt::Erased, Lt::Static) => Constraints::none(env))
        )

        // A rigid type `r` outlives `b` if all of `r`'s parameters outlive `b`
        (
            (prove(decls, env, assumptions, Wcs::all_outlives(parameters, b)) => c)
            ----------------------------- ("rigid types")
            (prove_outlives(decls, env, assumptions, RigidTy { name: _, parameters }, b) => c)
        )

        (
            (prove_normalize(decls, env, assumptions, a) => Constrained(a1, c))
            (prove_after(decls, c, assumptions, Predicate::outlives(a1, b)) => c)
            ----------------------------- ("normalize-l")
            (prove_outlives(decls, env, assumptions, a, b) => c)
        )

        (
            (prove_normalize(decls, env, assumptions, b) => Constrained(b1, c))
            (prove_after(decls, c, assumptions, Predicate::outlives(a, b1)) => c)
            ----------------------------- ("normalize-r")
            (prove_outlives(decls, env, assumptions, a, b) => c)
        )

        // 'a : 'b if  we can find `'a : 'b` in assumptions, or if there is transtive
        // outlive relationship (if we have 'a : 'c and 'c : 'b, then we'd know 'a : 'b).
        (
            (let all_outlives = transitively_outlived_by(assumptions, a))
            (if all_outlives.contains(&*b))!
            ----------------------------- ("outlive through assumption")
            (prove_outlives(_decls, env, assumptions, a, b) => Constraints::none(env))
        )

        // Rather than proving `'a: 'b` locally, we can add it to the environment
        // as a "pending obligation" and leave it to the caller to prove.
        // This is only allowed when `allow_pending_outlives` is set on the environment.
        (
            (if env.allow_pending_outlives())!
            ----------------------------- ("anything can be pending")
            (prove_outlives(_decls, env, _assumptions, a, b) => Constraints::none(
                env.with_pending(Predicate::outlives(a, b))
            ))
        )
    }
}

judgment_fn! {
    pub(crate) fn prove_outlives_bound(
        decls: Program,
        env: Env,
        assumptions: Wcs,
        source: Parameter,
        source_region: Parameter,
        target: Parameter,
        target_region: Parameter,
    ) => Constraints {
        debug(source, source_region, target, target_region, assumptions, env)

        (
            (prove(decls, env, assumptions, Predicate::equals(source_region, target_region)) => c)
            (let (assumptions, source, target) = c.substitution().apply((assumptions, source, target)))
            (prove_outlives_component(decls, c.env(), assumptions, source, target) => c2)
            ----------------------------- ("same region")
            (prove_outlives_bound(decls, env, assumptions, source, source_region, target, target_region) => c.seq(c2))
        )

        (
            (prove(decls, env, assumptions, Predicate::outlives(source_region, target_region)) => c)
            (let (assumptions, source, target) = c.substitution().apply((assumptions, source, target)))
            (prove_outlives_component(decls, c.env(), assumptions, source, target) => c2)
            ----------------------------- ("shorter region")
            (prove_outlives_bound(decls, env, assumptions, source, source_region, target, target_region) => c.seq(c2))
        )
    }
}

judgment_fn! {
    fn prove_outlives_component(
        decls: Program,
        env: Env,
        assumptions: Wcs,
        source: Parameter,
        target: Parameter,
    ) => Constraints {
        debug(source, target, assumptions, env)

        (
            (if source.kind() == target.kind())!
            (prove(decls, env, assumptions, Predicate::equals(source, target)) => c)
            ----------------------------- ("equal component")
            (prove_outlives_component(decls, env, assumptions, source, target) => c)
        )

        (
            (parameter in parameters)
            (prove_outlives_component(decls, env, assumptions, parameter, target) => c)
            ----------------------------- ("reference components")
            (prove_outlives_component(decls, env, assumptions, RigidTy { name: RigidName::Ref(_), parameters }, target) => c)
        )

        (
            (prove_normalize(decls, env, assumptions, source) => Constrained(normalized, c1))
            (let (assumptions, normalized, target) = c1.substitution().apply((assumptions, normalized, target)))
            (prove_outlives_component(decls, c1.env(), assumptions, normalized, target) => c2)
            ----------------------------- ("normalized bound")
            (prove_outlives_component(decls, env, assumptions, source, target) => c1.seq(c2))
        )
    }
}

/// Given a region `r1`, find a set of all regions `r2` where `r1 : r2` transitively
/// according to the assumptions.
fn transitively_outlived_by(
    assumptions: impl Upcast<Wcs>,
    r1: impl Upcast<Parameter>,
) -> Set<Parameter> {
    let assumptions: Wcs = assumptions.upcast();
    let r1: Parameter = r1.upcast();
    let mut reachable = Set::new();

    reachable.insert(r1.clone());
    let mut worklist = vec![r1.clone()];

    // Take all the outlives assumptions.
    let outlives_assumptions: Vec<Predicate> = assumptions
        .iter()
        .filter_map(|wc| {
            if let Wc::Predicate(Predicate::Outlives(r1, r2)) = wc {
                return Some(Predicate::Outlives(r1, r2));
            }
            None
        })
        .collect();

    // Find the set of lifetime that is transitively outlived by r1.
    while let Some(current) = worklist.pop() {
        for relation in outlives_assumptions.iter() {
            let Predicate::Outlives(r1, r2) = relation else {
                panic!("we should only have outlive relation here");
            };

            if *r1 == current {
                if reachable.insert(r2.clone()) {
                    worklist.push(r2.clone());
                }
            }
        }
    }

    reachable
}

// test case
//
// fn foo<'a, 'b>(x: &'a u32, y: &'b u32) -> &'b u32 where 'a: 'b  { x } // OK
// fn foo<'a, 'b>(x: &'a u32, y: &'b u32) -> &'b u32 where 'a: 'b  { y } // ERROR
// fn foo<'a, 'b, 'c>(x: &'a u32, y: &'c u32) -> &'c u32 where 'a: 'b, 'b: 'c  { x } // OK
// fn foo<'b>(x: &'static u32, y: &'b u32) -> &'b u32 { x } // OK
// fn foo<'b>(x: &'b u32, y: &'static u32) -> &'b u32 { x } // ERROR
//
// What is going on here?
//
// (1) Two *universal* (lifetime) variables, 'a and 'b
// (2) Assumption: `Outlives('a, 'b)`
// (3) Goal:
// - `Sub(&'a u32 <: &'b u32)`
//   - `Outlives('a: 'b)``
