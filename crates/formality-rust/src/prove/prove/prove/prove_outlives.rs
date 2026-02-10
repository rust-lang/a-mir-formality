use formality_core::judgment_fn;
use crate::types::grammar::{LtData, Parameter, Relation, RigidTy, Wcs};

use crate::prove::prove::{decls::Decls, prove};

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
        _decls: Decls,
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
            (prove_outlives(_decls, _env, _assumptions, LtData::Static, _b) => Constraints::none(env))
        )

        // A rigid type `r` outlives `b` if all of `r`'s parameters outlive `b`
        (
            (prove(decls, env, assumptions, Wcs::all_outlives(parameters, b)) => c)
            ----------------------------- ("rigid types")
            (prove_outlives(decls, env, assumptions, RigidTy { name: _, parameters }, b) => c)
        )

        // Rather than proving `'a: 'b` locally, we can add it to the environment
        // as a "pending obligation" and leave it to the caller to prove.
        // This is only allowed when `allow_pending_outlives` is set on the environment.
        (
            (if env.allow_pending_outlives())!
            ----------------------------- ("anything can be pending")
            (prove_outlives(_decls, env, _assumptions, a, b) => Constraints::none(
                env.with_pending(Relation::outlives(a, b))
            ))
        )
    }
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
