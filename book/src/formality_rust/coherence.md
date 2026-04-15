# Coherence checking

Coherence checking lives in `check/coherence.rs` and runs as part of crate checking.

At a high level, `check_coherence`:

* rejects duplicate impls in the current crate
* checks the current crate's impls for overlap against all impls in the program
* runs orphan checking for positive impls in the current crate
* runs orphan checking for negative impls in the current crate

## Orphan checking

`orphan_check` and `orphan_check_neg` instantiate the impl binder universally and prove that the trait reference is local under the impl's where-clauses.

## Duplicate impls

`ensure_no_duplicate_impls` rejects duplicate positive impls in the current crate before overlap checking runs.

## Overlap checking

`overlap_check_impl` compares two impls of the same trait.

It first skips identical impls and impls for different traits. For matching trait ids, it tries to prove that the impls cannot both apply. If that fails, it reports the impls as overlapping.

## Negative impls

Negative impls participate in coherence through orphan checking.