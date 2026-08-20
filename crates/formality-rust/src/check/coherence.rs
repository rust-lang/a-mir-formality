use crate::grammar::{Crate, Goals, NegTraitImpl, Predicate, TraitImpl};
use crate::prove::{Env, Program};

use super::{prove_goal, prove_not_goal};
use formality_core::judgment_fn;

judgment_fn! {
    /// Runs coherence checking (orphan, overlap, duplicates) for the current crate.
    /// Returns ProvenSet<()> caller.
    pub(crate) fn check_coherence(program: Program, current_crate: Crate) => () {
        debug(program, current_crate)

        (
            (let current_crate_impls = program.trait_impls_in_crate(&current_crate))
            (let all_crate_impls = program.trait_impls())
            (for_all(impl_a in current_crate_impls)
                (for_all(impl_b in all_crate_impls)
                    (overlap_check_impl(program, impl_a, impl_b) => ())))
            (for_all(trait_impl in current_crate_impls)
                (orphan_check(program, trait_impl) => ()))
            (for_all(neg_trait_impl in program.neg_trait_impls_in_crate(&current_crate))
                (orphan_check_neg(program, neg_trait_impl) => ()))
            --- ("check_coherence")
            (check_coherence(program, current_crate) => ())
        )
    }
}

// Orphan rule (RFC 2451): prove the trait ref is local under the impl's where-clauses.
judgment_fn! {
    fn orphan_check(program: Program, impl_a: TraitImpl) => () {
        debug(program, impl_a)

        (
            (let (env, a) = Env::default().instantiate_universally(&impl_a.binder))
            (let trait_ref = a.trait_ref())
            (prove_goal(program, env, &a.where_clauses, Predicate::is_local(trait_ref)) => ())
            --- ("orphan_check")
            (orphan_check(program, impl_a) => ())
        )
    }
}

judgment_fn! {
    fn orphan_check_neg(program: Program, impl_a: NegTraitImpl) => () {
        debug(program, impl_a)

        // The orphan check passes if
        // ∀P. ⌐ (coherence_mode => (wf(Ts) && cannot_be_proven(is_local_trait_ref)))
        //
        // TODO: feels like we do want a general "not goal", flipping existentials
        // and universals and the coherence mode
        // self.prove_not_goal(&env, &(Goals::wf)) // ??
        (
            (let (env, a) = Env::default().instantiate_universally(&impl_a.binder))
            (let trait_ref = a.trait_ref())
            (prove_goal(program, env, &a.where_clauses, Predicate::is_local(trait_ref)) => ())
            --- ("orphan_check_neg")
            (orphan_check_neg(program, impl_a) => ())
        )
    }
}

judgment_fn! {
    /// Holds if `impl_a` and `impl_b` cannot apply to the same types.
    /// Trivially holds for the same impl or for impls of different traits.
    fn overlap_check_impl(program: Program, impl_a: TraitImpl, impl_b: TraitImpl) => () {
        debug(program, impl_a, impl_b)

        // An impl cannot overlap with itself
        (
            (if impl_a == impl_b)
            --- ("same impl")
            (overlap_check_impl(_program, impl_a, impl_b) => ())
        )

        // Impls of two distinct traits cannot overlap
        (
            (if impl_a.trait_id() != impl_b.trait_id())
            --- ("different trait")
            (overlap_check_impl(_program, impl_a, impl_b) => ())
        )

        // Example:
        //
        // Given two impls...
        //
        //   impl<P_a..> SomeTrait<T_a...> for T_a0 where Wc_a { }
        //   impl<P_b..> SomeTrait<T_b...> for T_b0 where Wc_b { }
        //
        // We want to prove that ∀P_a, ∀P_b ...
        // ... ¬(coherence_mode => (Ts_a = Ts_b ∧ Wc_a ∧ Wc_b))
        //
        // i.e., there is no overlap if, in coherence mode, if we can prove that either
        // * the parameters cannot be equated ¬(Ts_a = Ts_b)
        // * or the where-clauses don't hold (¬Wc_a || ¬Wc_b).
        //
        // TODO: feels like we do want a general "not goal", flipping existentials
        // and universals and the coherence mode.
        // self.prove_not_goal(&env, &(Goals::wf))
        (
            (if impl_a != impl_b)
            (if impl_a.trait_id() == impl_b.trait_id())
            (let (env, a) = Env::default().instantiate_universally(&impl_a.binder))
            (let (env, b) = env.instantiate_universally(&impl_b.binder))
            (prove_not_goal(program, env, (), (Goals::all_eq(&a.trait_ref().parameters, &b.trait_ref().parameters), &a.where_clauses, &b.where_clauses)) => ())
            --- ("not goal")
            (overlap_check_impl(program, impl_a, impl_b) => ())
        )

        // try inverted where-clauses from Wc_a / Wc_b (e.g. T: Debug => T: !Debug).
        // If (equal params ∧ Wc_a ∧ Wc_b) => Wc_i is provable the two impls cannot both apply.
        (
            (if impl_a != impl_b)
            (if impl_a.trait_id() == impl_b.trait_id())
            (let (env, a) = Env::default().instantiate_universally(&impl_a.binder))
            (let (env, b) = env.instantiate_universally(&impl_b.binder))
            (wc in a.where_clauses.iter().chain(&b.where_clauses).flat_map(|wc| wc.invert()))
            (prove_goal(program, env, (Goals::all_eq(&a.trait_ref().parameters, &b.trait_ref().parameters), &a.where_clauses, &b.where_clauses), wc) => ())
            --- ("inverted")
            (overlap_check_impl(program, impl_a, impl_b) => ())
        )
    }
}
