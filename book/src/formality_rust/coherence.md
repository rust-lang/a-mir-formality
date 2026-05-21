# Coherence checking

Coherence checking lives in `check/coherence.rs` and runs as part of crate checking.

## `check_coherence`

{judgment}`check_coherence`

At a high level, `check_coherence`:

* rejects duplicate impls in the current crate
* checks the current crate's impls for overlap against all impls in the program
* runs orphan checking for positive impls in the current crate
* runs orphan checking for negative impls in the current crate

## Orphan checking

{judgment}`orphan_check`

`orphan_check` instantiates the impl binder universally and proves that the trait reference is local under the impl's where-clauses.

Negative impls have the same structure:

{judgment}`orphan_check_neg`

## Overlap checking

`overlap_check_impl` compares two impls of the same trait. It first skips identical impls and impls for different traits. For matching trait ids, it tries to prove that the impls cannot both apply. If that fails, it reports the impls as overlapping.

## Proving locality

The `is_local` module determines whether a trait reference could have been defined locally, which is the key predicate for orphan checking:

{judgment}`is_local_trait_ref`

A parameter is considered local if it is a local type or a fundamental rigid type wrapping a local parameter:

{judgment}`is_local_parameter`

The complement — determining whether a trait reference *may* come from a remote crate — is used in coherence mode:

{judgment}`may_be_remote`
