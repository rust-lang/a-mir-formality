# Defining your language

The very first thing you do to define your own language is to use the `formality_core::declare_language!` macro.
You use it like so:

```rust
{{#include ../../../crates/formality-types/src/lib.rs:declare_rust_language}}
```

The `declare_language` macro will create a module with the name you specify (here, `rust`).
You have to tell it a few things:

* The `NAME` of your language, a string for debugging.
* An enum defining the *kinds* of variables in your language; see the [variables](./variables.md) chapter for more information. For Rust, the kinds are types, lifetimes, and constants.
* An enum defining a *parameter*, which is the terms that can be used to represent the *value* of a variable; see the [variables](./variables.md) chapter for more information.
* Two characters `BINDER_OPEN` and `BINDER_CLOSED` defining the opening and closing characters for binders, e.g., `<` and `>`, to use when parsing.

## Contents of the language module

The language module you create has various items in it:

* A `struct FormalityLang` that defines your language. Some of the contains of `formality_core` (notably the traits that involve bound variables) are 

## Specifying the language for a crate

That module will contain a language struct named `FormalityLang`. It 
Other parts of the formality system (e.g., autoderives and the like) 
will need to know the current language you are defining,
and they expect to find it at `crate::FormalityLang`. 
Best practice is to add a `use` at the top of your crate defining your language.
For example, the `formality_types` crate has:

```rust
{{#include ../../../crates/formality-types/src/lib.rs:use_rust_language}}
```

and other crates like `formality_rust` have:

```rust
{{#include ../../../crates/formality-rust/src/lib.rs:use_rust_language}}
```
