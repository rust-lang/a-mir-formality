# Fuzzing

Formality programs can be fuzzed with [bolero](https://crates.io/crates/bolero) but as of now the integration is fairly weak and requires some manual intervention. One of the challenges is that bolero's traits do not allow us to easily thread context through, so we must rely on static variables. The other challenge is that we wish to steer the fuzzer to generate "mostly valid" programs, and that means that the integration cannot be fully auto-generated.

## Auto-derive and when to use it

Enabling most types for use with bolero is as simple as annotating types with `#[derive(bolero::TypeGenerator)]`.
This works great as long as any value for the fields is potentialy valid.
But when you have fields that are meant to be references to other items in the program, you are likely to get nonsense:
not necessarily a *problem*, but likely a waste of fuzzing time and effort.
Navigating this requires writing custom fuzzing implementations.

## Common reference types

The two most common "references" are identifiers (e.g., names of structs) and type variables.
As these are part of formality-core we have some built-in support for generating them.

### FuzzSingleton

The `formality_core::fuzz::FuzzSingleton` is a useful type for setting up global context that can be accessed from `TypeGenerator` implementations. To use it, declare a static like 

```rust
static F: FuzzSingleton<Vec<u32>> = F::new();
```

you can then use `F.get()` to read the current value (initialized with `Default::default`).

You can modify the value with methods like `F.set()` and `F.push()`. These will return a "guard" value that resets the singleton back to its initial state. Make sure you drop that guard appropriately, typically at the end of the block:

```rust
let _guard = F.set(new_value);
```

The `set` method is intended to be used once per fuzzing session and hence has an assertion that the value is currently at its default value.

### Identifiers

When you declare an identifier type `SomeId` with the `id!` macro,
it also creates an associated "fuzzing pool" accessible via `SomeId::fuzz_pool()`.
This is a static vector of identifiers you can use.
When an identifier is fuzzed, it will pick a value from the fuzzing pool.

The fuzzing pool starts empty. You can add entries to it by invoking `push` on the fuzzing pool.

A common pattern is to generate a set of identifiers early on (e.g., a set of struct names your program will have) and then set the fuzzing pool to contain those identifiers.

### Bound variables

The built-in variable types (universal, existential, etc) have fuzzing implementations.
The intention is to only have the fuzzer generate closed terms with no free variables.

Every formality language `L` has a fuzzing pool of variables in scope that can be referenced.
This begins as empty.
You can push new entries onto it with `L::open_fuzz_binder(kinds)`, which will create a set of variables `V` with the given kinds.
It returns a guard that will remove those variables from scope; the guard also has a method `into_binder`
that can be used to close over the variables `V` and create a `Binder<T>`.

Fuzzing a variable generates a reference to one of the variables pushed by `open_fuzz_binder`.
These are `BoundVariable` elements with those kinds and with depth set to `None`, just as you get with `Binder::open`. 
They are meant to be enclosed later in a binder with the `into_binder` method.

When you fuzz a `Binder<T>`, it follows this sequence:

* Fuzz some set of kinds `K`
* Invoke `guard = L::open_fuzz_binder(K)` to push a set of variables `V` in scope 
* Fuzz a `T` that will reference variables in `V` (and possibly others that are already in scope)
* Close over the `T` with `guard.into_binder(T)`, returning a `Binder<T>` where each reference variable in `V` now refers to an element in the binder. `V` are also removed from scope.

If you wish to fuzz a value that references a known set of variables, you can do so by invoking `L::open_fuzz_binder` yourself.

## How formality-Rust fuzzing works

The formality Rust fuzzer tries to generate "mostly well-kinded" programs. The pattern for generating structs is as follows:

* Generate a set of struct names and their generic parameters, effeciively a set of `(String, Vec<ParameterKind>)` tuples.
    * Note that a **string** is used to let the fuzzer generate fresh names.
* Push each of the struct names into `AdtId::fuzz_pool()` as an available name for reference.
    * Also store the kinds into a `FuzzSingleton<Map<AdtId, Vec<ParameterKind>>>`
* Generate struct definitions as follows. For each defined struct `(S, K)` with name `S` and kinds `K`:
    * Manually invoke `let guard = L::open_fuzz_binder(K)` to bring those parameters into scope
    * Generate the "body" of the struct (`StructBoundData`)
    * Close over `guard.into_binder(body)` to get a `Binder<StructBoundData>` that can be used in the struct definition
* Generate references to structs as follows. Whenever we fuzz a type, provide a custom `TypeGenerator` impl that will
    * Fuzz an `AdtId`, picking a struct name from the availabel list
    * Look up its kinds from the map
    * Generate a suitable set of parameters that match those kinds





