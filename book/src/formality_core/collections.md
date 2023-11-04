# Collections

When using formality, it's best to use the following collection types:

* for sequences, use the standard `Vec` type
* `formality_core::Set<T>` -- equivalent to `BTreeSet` but shorter. We use a `BTreeSet` because it has deterministic ordering for all operations.
* `formality_core::Map<K, V>` -- equivalent to `BTreeMap` but shorter. We use a `BTreeMap` because it has deterministic ordering for all operations.

## Macros

We also define macros:

* `seq![...]` -- equivalent to `vec![]` but permits flattening with `..` notation, as described below
* `set![...]` -- like `seq!`, but produces a `Set`

In these macros you can "flatten" things that support `IntoIterator`, so `set![..a, ..b]` will effectively perform a set union of `a` and `b`.

## Casting between collections and tuples

It is possible to upcast from variable tuple types to produce collections:

* A `Vec<E1>` can be upcast to a `Vec<E2>` if `E1: Upcast<E2>`.
* A `Set<E1>` can be upcast to a `Set<E2>` if `E1: Upcast<E2>`.
* Tuples of elements (e.g., `(E1, E2)` or `(E1, E2, E3)`) can be **upcast** to sets up to a fixed arity.
* Sets and vectors can be **downcast** to `()` and `(E, C)`, where `()` succeeds only for empty collections, and `(E, C)` extracts the first element `E` and a collection `C` with all remaining elements (note that elements in sets are always ordered, so the first element is well defined there). This is useful when writing judgment rules that operate over sequences and sets.