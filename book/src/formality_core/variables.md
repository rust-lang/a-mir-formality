# Variables

As in [Defining your lang](./lang.md), `declare_language!` implements [`Language`][language] by setting the associated types `Kind` and `Parameter`. They must satisfy the bounds on that trait (including `HasKind` for `Parameter`); in `formality_rust` both are enums:

* **`Kind`** — the *sorts* of parameter (type, lifetime, const, …). Classifies variables and binders.
* **`Parameter`** — the *values* a parameter can be instantiated to.

The generated language module also defines `pub type` aliases in `grammar` (`Variable`, `Binder`, `Substitution`, …) that specialize the `formality_core` types to `FormalityLang`.

Internally, variables use [`CoreVariable<L>`][core_variable]:

* **`UniversalVariables`** — treated as an unknown assumed in the environment.
* **`ExistentialVariables`** — a metavariable the rules will eventually constrain or solve.
* **`BoundVariables`** — tied to an enclosing binder (de Bruijn level and slot index). `Binder::open` introduces fresh bound variables with `debruijn: None` (see `CoreBoundVar::fresh`) `Binder::new` closes the binder again.

`CoreVariable::is_free`: universals and existentials are always free. A bound variable is free iff `debruijn` is `None` (including after `Binder::open`).

In `#[term]` definitions, use `#[variable]` on variable variants so folding applies substitutions and parsing resolves names against in-scope bindings (see [Defining terms with the `term` macro](./terms.md)).

[language]: https://github.com/rust-lang/a-mir-formality/blob/20ed035b96bf11f154f6816be85a3b042d6dcc16/crates/formality-core/src/language.rs
[core_variable]: https://github.com/rust-lang/a-mir-formality/blob/20ed035b96bf11f154f6816be85a3b042d6dcc16/crates/formality-core/src/variable.rs
