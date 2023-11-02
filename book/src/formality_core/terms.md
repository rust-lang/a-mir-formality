# Defining terms with the `term` macro

There are two or three key things to understand. The first is the [`#[term]`][term] macro. This is a procedural macro that you can attach to a `struct` or `enum` declaration that represents a piece of Rust syntax or part of the trait checking rules. It auto-derives a bunch of traits and functionality...

[term]: https://github.com/rust-lang/a-mir-formality/blob/bca36ecd069d6bdff77bffbb628ae3b2ef4f8ef7/crates/formality-macros/src/term.rs#L14-L36

* rules for parsing from a string
* a `Debug` impl to serialize back out
* folding and substitution
* upcasting and downcasting impls for convenient type conversion

For some types, we opt not to use `#[term]`, and instead implement the traits by hand. There are also derives so you can derive some of the traits but not all.

### Using `#[term]`

Let's do a simple example. If we had a really simple language, we might define expressions like this:

```rust
#[term]
enum Expr {
    #[cast]
    Variable(Variable),
    
    #[grammar($v0 + $v1)]
    Add(Box<Expr>, Box<Expr>),
}

#[term($name)]
struct Variable {
    name: String
}
```

The `#[term]` macro says that these are terms and we should generate all the boilerplate automatically. Note that it will generate code that references `crate::FormalityLang` so be sure to [define your language appropriately](./lang.md).

The `#[term]` also accepts some internal annotations:

* `#[cast]` can be applied on an enum variant with a single argument. It says that this variant represents an "is-a" relationship and hence we should generate upcast/downcast impls to allow conversion. In this case, a variable is a kind of expression -- i.e,. wrapping a variable up into an expression doesn't carry any semantic meaning -- so we want variables to be upcastable to expressions (and expressions to be downcast to variables). The `#[cast]` attribute will therefore generate an impl `Variable: Upcast<Expr>` that lets you convert a variable to an expression, and a downcast impl `Expr: Downcast<Variable>` that lets you try to convert from an expression to a variable. Downcasting is *fallible*, which means that the downcast will only succeed if this is an `Expr::Variable`. If this is a `Expr::Add`, the downcast would return `None`.
    * There is a special case version of `#[cast]` called `#[variable]`. It indicates that the variant represents a (type) variable -- we are not using it here because this an expression variable. Variables are rather specially in folding/parsing to allow for substitution, binders, etc.
* `#[grammar]` tells the parser and pretty printer how to parse this variant. The `$v0` and `$v1` mean "recursively parse the first and second arguments". This will parse a `Box<Expr>`, which of course is implemented to just parse an `Expr`.

If you are annotating a struct, the `#[term]` just accepts the grammar directly, so `#[term($name)] struct Variable`  means "to parse a variable, just parse the name field (a string)".

We could also define types and an environment, perhaps something like

```rust
#[term]
enum Type {
    Integer, // Default grammar is just the word `integer`
    String // Default grammar is just the word `string`
}

#[term] // Default grammar is just `env($bindings)`
struct Env {
    bindings: Set<(Variable, Type)>
}
```

You can see that the `#[term]` macro will generate some default parsing rules if you don't say anything. 

We can then write code like

```rust
let env: Env = term("env({(x, integer)})");
```

This will parse the string, panicking if either the string cannot be parsed or or if it is ambiguous (can be parsing in mutiple ways). This is super useful in tests.

These terms are just Rust types, so you can define methods in the usual way, e.g. this `Env::get` method will search for a variable named `v`:

```rust
impl Env {
    pub fn get(&self, v: &Variable) -> Option<&Type> {
        self.bindings.iter()
            .filter(|b| &b.0 == v)
            .map(|b| &b.1)
            .next()
    }
}
```
