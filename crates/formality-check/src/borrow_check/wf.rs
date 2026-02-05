//! Checks for simple syntactic criteria like "all identifiers are valid"
//! that the rest of the code would prefer to just assume hold.

use formality_core::judgment_fn;
use formality_rust::grammar::minirust::Terminator;

use crate::mini_rust_check::TypeckEnv;

judgment_fn! {
    /// Prove that any loans issued in this statement are respected.
    pub fn terminator_wf(
        env: TypeckEnv,
        terminator: Terminator,
    ) => () {
        debug(terminator, env)

        (
            (for_all(bb_id in &bb_ids)
             (if env.basic_block(bb_id).is_ok()))
            --- ("goto")
            (terminator_wf(env, Terminator::Goto(bb_ids)) => ())
        )

        (
            (for_all(switch_target in &switch_targets)
             (if env.basic_block(&switch_target.target).is_ok()))
            (if env.basic_block(&fallback).is_ok())
            --- ("switch")
            (terminator_wf(env, Terminator::Switch { switch_value: _, switch_targets, fallback }) => ())
        )

        (
            --- ("return")
            (terminator_wf(_env, Terminator::Return) => ())
        )

        (
            (if next_block.map(|b| env.basic_block(&b).is_ok()).unwrap_or(true))
            --- ("call")
            (terminator_wf(env, Terminator::Call { callee: _, generic_arguments: _, arguments: _, ret: _, next_block }) => ())
        )
    }
}