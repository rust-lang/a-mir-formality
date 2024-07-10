# Fuzzing

Formality terms come with support for fuzzing, though you do need to write some of the top-level code yourself.
The fuzzing system is designed to produce 'mostly valid' programs that are e.g. well-kinded and do not have references to random identifiers.
This is meant to concentrate efforts on fuzzing interesting programs that are "almost correct".

The fuzzing strategy is independent from the fuzzer, but we ship integration with [bolero] by default. 
[bolero] is a meta-fuzzing crate that lets you switch between fuzzers.

[bolero]: https://crates.io/crates/bolero

## Formality's `fuzz` module

The `formality_core::fuzz` module defines a few key types for fuzzing:

* The `FuzzDriver` trait defines the core operations we need from the fuzzer (basically: pick integers and floats from a range). If you want to use something other than [bolero], you can implement this trait to do so.
* The `FuzzConfig` struct which defines various kinds of fuzzing configuration. This, combined with a `&dyn FuzzDriver`, is used to produce a `FuzzCx` that will let you actually do fuzzing.
* The `FuzzCx` fuzzing context. This carries around state (including the `&dyn FuzzDriver`) used when fuzzing. 
* The `Fuzzable` trait is implemented by anything that is fuzzable. It defines two main operations
    * `estimate_cardinality(cx: &mut FuzzCx) -> f64`, which produces an estimate of number of instances of `Self` that could be produced by the fuzzer. This is used to ensure the fuzzer evenly covers the state space of programs, as described in detail below, but also to avoid invalid programs.
    * `fuzz() -> Option<Self>`, which produces a single value (or `None` if it runs out of bytes or otherwise fails).

All formality terms must implement `Fuzzable` and the `#[term]` macro generates a default `Fuzzable` implementation. You can add `#[customize(fuzz)]` if you want to write the impl an impl yourself, but this is rarely required.

## Ensuring "nearly valid" programs

The `FuzzConfig` struct lets you define various parameters that control the range of programs that get generated.
This configuration is stored is accessible from the `FuzzCx` during fuzzing.
It includes some basics, such as the expected length of collections (0..3 elements by default) or the maximum recursion depth (3 by default), but it also has two customizable maps that you can use to store information about own types (these are implemented using `TypeId`, which allows them to store types defined outside of `formality-core`):

* `fuzz_config.with_values::<T>(some_vec)` will set the value set of values for `T` to `some_vec`. This can be retrieved during fuzzing by calling `pick_value`, which will select one of those values at random and return it.
* `fuzz_config.with_key_values::<K, V>(some_map)` will store a map from `K -> V`. You can then supply a `K` value and get back a `V` value in your code.
* `fuzz_config.with_free_variables(free_vars)` will set the initial range of free variables used when generating a type variable in your language. This defaults to empty, but when you fuzz a `Binder<T>`, it will add more variables in the context that are free only within the binder.

Here are some of the ways the above methods are used in formality-core and also in formality-rust.

### Identifiers

The identifier types created with `formality_core::id!` expect to have their valid values configured in advance.
The `Fuzzable` implementation for some identifier type `I` therefore checks the configured values for `I`.
You will need to configure those in advance.

### Variables in scope

Each formality language defines various variable types (universal, existential, bound).
The fuzzing context tracks a set of "free variables" and fuzzing a variable clones one of those values.
This set begins as empty so that only closed terms are generated.
When you fuzz a `Binder<T>`, it first generates some number (configurable with `binder_range`) of variables for the binder and then brings those into scope when fuzzing the bound `T` value.

If you don't want to fuzz a closed term, you can introduce free variables yourself with `with_key_values` (the example of how we fuzz Rust, described below, demonstrates one reason to do that).

## Estimating cardinality to fuzz the search space evenly

A naive approach to fuzzing an enum would be to pick one of the variants at random with equal weight. The problem with doing that is that often the amount of search space covered by different variants can be quite different. Consider a simple tree:

```rust
enum List {
    Nil,
    Cons((), Box<List>)
}
```

You can see that if we pick `List::Nil` 50% of the time, we are going to spend half our time fuzzing with an empty list -- but that is a small fraction of the total search space. You can read more about this problem on [John Regehr's blog](https://blog.regehr.org/archives/1700).

Our approach to resolving it is to estimate the **cardinality** of each type we can fuzz, which roughly means "the number of values we might conceivably generate".
This is only possible because we bound the maximum recursion depth or else the search space would be infinite.
This bound is carried on `FuzzCx` and can be configured with `with_max_depth` (it defaults to 3 as of this writing).

Using the cardinality, we can pick between the variants more intelligently. 
For example, with `List`, the cardinality of `List::Nil` is just 1. 
But the cardinality of `List::Cons` would be 3. 
Therefore, at the root, we would pick `List::Cons` 75% of the time and `List::Nil` only 25% of the time.

Note that the cardinality is actually a function not only of the type but of the depth. 
In fact, in `FuzzCx`, we compute the cardinality of a type `T` under two bits of context: 
first, the recursion depth, second, the number of open binders (which influences the cardinality of type variables).

**Subtle point on cardinality of variables:** It is not possible to precisely compute the cardinality for variables. This is because fuzzing a `Binder<T>` will introduce some set of variables into scope but we cannot know in advance how many it will be. In this case, we track the number of binder scopes and use the expected value from `binder_range` to compute an estimate of the number of free variables that will be in scope at the time. Good enough.

## Writing your own `Fuzzable` impls

Most of your `Fuzzable` impls will be auto-generated by `#[term]`.
However, you will likely find you wish to implement a few of them by hand.
The key criteria is when you wish to "correlate" the fuzzing of two fields within a struct
or to only fuzz values brought into scope from outer context in some way.

Some examples of custom fuzzable impls:

* Every identifier type provides a custom impl so that it can pick a valid value from the context (versus generating arbitrary strings).
* In formality Rust, the `RigidTy` type defines a custom impl so that it can produce the correct number of generic arguments (e.g., we don't want to produce `Option<u32, i32>`, as that is ill-kinded).

Customizing a `Fuzzable` impl for a `#[term]` is done by adding `#[customize(fuzz)]` to the struct definition.

### Writing a `Fuzzable` impl

To properly track depth and avoid infinite recursion, fuzzable impls are written in a stylized fashion.
To show you how it works in a simple case, consider this struct:

```rust
#[term]
struct Foo {
    bar: Bar,
    count: u32,
}
```

Here is the code that `#[term]` would produce to fuzz this struct:

```rust
use crate::Formality:Lang;

impl Fuzzable<FormalityLang> for Foo {
    fn estimate_cardinality(cx: &mut FuzzCx<'_, FormalityLang>) -> f64 {
        cx.enter_estimate_cardinality::<Self>(|guard| {
            guard.estimate_cardinality::<Bar>() *
            guard.estimate_cardinality::<u32>()
        })
    }

    fn fuzz(cx: &mut FuzzCx<'_, FormalityLang>) -> Option<Self> {
        cx.enter_fuzz(|guard| {
            Some(Self {
                bar: guard.fuzz::<Bar>()?,
                count: guard.fuzz::<u32>()?
            })
        })
    }
}
```

Let's walk through it piece by piece.

* `impl Fuzzable<FormalityLang> for Foo`
    * The `Fuzzable` trait takes the language as a parameter. In this case you just want to use `crate::FormalityLang`, the language you are defining.
* `fn estimate_cardinality(cx: &mut FuzzCx<', FormalityLang>)`
    * The `estimate_cardinality` function always begins with a call to `cx.enter_estimate_cardinality::<Self>`, which will track the recursion depth.
    * This function takes a `|guard| ...` closure that is given a `guard` that can be used to perform recursive operations.
    * For a "product" type like a struct, you typically just want to multiple the cardinality of all the fields to get the total cardinality. This assumes the set of values for each field are independent.
* `fn fuzz(cx: &mut FuzzCx<'_, FormalityLang>)`
    * Again the `fuzz` function begins by calling an `enter` method, this time `cx.enter_fuzz::<Self>`. This method again takes a closure that gets a `guard` parameter.
    * The `guard.fuzz:<T>()` method will generate an instance of `Option<T>`, so it is typically discharged with `?`.

Some example impls you can look at:

* the impl for `Option<T>` contains a very simple "sum" type
* the impl for `Vec<T>` shows how to pick the length of a collection, taking into account the cardinality of its elements
* the impl in `formality-types` for `RigidTy` shows how to generate correlated cases where the number of parameters is dependent on the kind of type being crated

### Fuzzing your overall program

The fuzzable types will generate terms but to really use them you have to put them in context.
You can look at the `formality_rust::fuzz` module for an example of how to do that:

* It implements a `bolero::ValueGenerator` with the basic parameters of the Rust program (e.g., how many structs, etc).
* It begins by generating names for those structs (in our case, `Struct0`, `Struct1`, etc) as well as kinds and then installs those into the fuzzing context.
* It then generates the definition for each struct, adding free variables to cover the generic arguments we know the struct should have.
* It can then generate impls and function bodies freely against those struct definitions. 

### Example: Rust's `RustTypesFuzzConfig` trait

The `formality_types` crate (built on `formality_core`) shows how these methods can be used to generate well-kinded types.
It defines a `RustTypesFuzzConfig` trait with methods to set up and query the required context:

```rust
{{#include ../../../crates/formality-types/src/fuzz.rs:RustTypesFuzzConfigTrait}}
```

The `with_rust_types` method, for example, takes as argument a map `adt_kinds` that defines the set of valid `AdtId` values and the kinds of their generic parameters.
The implementation of this method processes that map to set the valid values for `AdtId` and also store the parameter kinds attached to the value of the `AdtId`:

```rust
{{#include ../../../crates/formality-types/src/fuzz.rs:RustTypesFuzzConfigImpl::with_rust_types}}
```

During fuzzing, the `adt_kinds` method will fetch the kinds for 

```rust
{{#include ../../../crates/formality-types/src/fuzz.rs:RustTypesFuzzConfigImpl::adt_kinds}}
```

This method is then used from the `Fuzzable` impl for `RigidTy`, which can

* fuzz an `AdtId` for the name
* lookup the kinds from the context
* and then fuzz an appropriate number of types, lifetimes, etc