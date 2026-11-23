# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.3.0](https://github.com/rust-lang/a-mir-formality/compare/formality-core-v0.2.0...formality-core-v0.3.0) - 2026-11-21

### Other

- Match cycle edge nodes in judgment conclusions
- Assert the visit order in the cycle fixed-point tests
- Add fixed-point test for a cycle with a tail (scratch)
- Remove explicit clones from judgment rules
- Add proven_results to extract values without proof trees
- Clarify what the for_all tests expect
- Test that for_all merges duplicate states
- Keep all possible states in for_all
- Test for_all with more than one possible state
- Document the `scope` condition
- Merge pull request #439 from nikomatsakis/fixed-point-2.0
- Add support for 128 bit integers
- Document judgment fixed points and share immutable proof nodes
- Exhaustively validate root-scoped judgment memoization
- Memoize completed results within one root call
- Remove obsolete generic fixed-point support
- Compute recursive judgments to a value fixed point
- Normalize rustc ':::' location lines in test snapshots
- Add a `scope` condition to judgment_fn!
- Merge pull request #480 from nikomatsakis/codex/judgment-collection-comprehensions
- Document comprehension desugaring strategy
- Simplify collection comprehensions with arrow syntax
- Add collection-producing for_all to judgment premises
