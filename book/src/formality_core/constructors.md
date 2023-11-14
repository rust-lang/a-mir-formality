# Constructors

Unless you include `#[customize(constructors)]`, the `#[term]` macro automatically creates constructors as follows:

- For a `struct`, defines a `new` method that takes an `impl Upcast<T>` for each of your fields.
- For an `enum`, defines a method per variant (converted to snake-case).
  - If the name of the variant is a Rust keyword like `Struct`, the method will be called `struct_`.
