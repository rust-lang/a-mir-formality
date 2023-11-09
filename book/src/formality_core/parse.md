# Parsing

Formality's `#[term]` and `#[grammar]` attributes let you specify the grammar that will be used to parse your structs/enums.

For structs, there is a single grammar, specified as part of the term:

```rust
#[term( /* grammar here */ )]
struct MyStruct { }
```

For enums, the grammar is placed in `#[grammar]` attributes on each variant:

```rust
#[term]
struct MyEnum {
    #[grammar( /* grammar here */ )]
    Foo(...),
}
```

### Ambiguity and greedy parsing

When parsing an enum there will be multiple possibilities. We will attempt to parse them all. If more than one succeeds, the parser will attempt to resolve the ambiguity by looking for the **longest match**. However, we don't just consider the number of characters, we look for a **reduction prefix**:

* When parsing, we track the list of things we had to parse. If there are two variants at the same precedence level, but one of them had to parse strictly more things than the other and in the same way, we'll prefer the longer one. So for example if one variant parsed a `Ty` and the other parsed a `Ty Ty`, we'd take the `Ty Ty`.
    * When considering whether a reduction is "significant", we take casts into account. See `ActiveVariant::mark_as_cast_variant` for a more detailed explanation and set of examples.

### Precedence and left-recursive grammars

We support left-recursive grammars like this one from the `parse-torture-tests`:

```rust
{{#include ../../../tests/parser-torture-tests/path.rs:path}}
```

We also support ambiguous grammars. For example, you can code up arithmetic expressions like this:


```rust
{{#include ../../../tests/parser-torture-tests/left_associative.rs:Expr}}
```

When specifying the `#[precedence]` of a variant, the default is left-associativity, which can be written more explicitly as `#[precedence(L, left)]`. If you prefer, you can specify right-associativity (`#[precedence(L, right)]`) or non-associativity `#[precedence(L, none)]`. This affects how things of the same level are parsed:

* `1 + 1 + 1` when left-associative is `(1 + 1) + 1`
* `1 + 1 + 1` when right-associative is `1 + (1 + 1)`
* `1 + 1 + 1` when none-associative is an error.

### Symbols

A grammar consists of a series of *symbols*. Each symbol matches some text in the input string. Symbols come in two varieties:

* Most things are *terminals* or *tokens*: this means they just match themselves:
    * For example, the `*` in `#[grammar($v0 * $v1)]` is a terminal, and it means to parse a `*` from the input.
    * Delimeters are accepted but must be matched, e.g., `( /* tokens */ )` or `[ /* tokens */ ]`.
* Things beginning with `$` are *nonterminals* -- they parse the contents of a field. The grammar for a field is generally determined from its type.
    * If fields have names, then `$field` should name the field.
    * For position fields (e.g., the T and U in `Mul(Expr, Expr)`), use `$v0`, `$v1`, etc.
    * Exception: `$$` is treated as the terminal `'$'`.
* Nonterminals have various modes:
    * `$field` -- just parse the field's type
    * `$*field` -- the field must be a `Vec<T>` -- parse any number of `T` instances. Something like `[ $*field ]` would parse `[f1 f2 f3]`, assuming `f1`, `f2`, and `f3` are valid values for `field`.
    * `$,field` -- similar to the above, but uses a comma separated list (with optional trailing comma). So `[ $,field ]` will parse something like `[f1, f2, f3]`.
    * `$?field` -- will parse `field` and use `Default::default()` value if not present.
    * `$<field>` -- parse `<E1, E2, E3>`, where `field: Vec<E>` 
    * `$<?field>` -- parse `<E1, E2, E3>`, where `field: Vec<E>`, but accept empty string as empty vector
    * `$(field)` -- parse `(E1, E2, E3)`, where `field: Vec<E>` 
    * `$(?field)` -- parse `(E1, E2, E3)`, where `field: Vec<E>`, but accept empty string as empty vector
    * `$[field]` -- parse `[E1, E2, E3]`, where `field: Vec<E>` 
    * `$[?field]` -- parse `[E1, E2, E3]`, where `field: Vec<E>`, but accept empty string as empty vector
    * `${field}` -- parse `{E1, E2, E3}`, where `field: Vec<E>` 
    * `${?field}` -- parse `{E1, E2, E3}`, where `field: Vec<E>`, but accept empty string as empty vector
    * `$:guard <nonterminal>` -- parses `<nonterminal>` but only if the keyword `guard` is present. For example, `$:where $,where_clauses` would parse `where WhereClause1, WhereClause2, WhereClause3` but would also accept nothing (in which case, you would get an empty vector).

### Greediness

Parsing is generally greedy. So `$*x` and `$,x`, for example, consume as many entries as they can. Typically this works best if `x` begins with some symbol that indicates whether it is present.

### Default grammar

If no grammar is supplied, the default grammar is determined as follows:

* If a `#[cast]` or `#[variable]` annotation is present, then the default grammar is just `$v0`.
* Otherwise, the default grammar is the name of the type (for structs) or variant (for enums), followed by `()`, with the values for the fields in order. So `Mul(Expr, Expr)` would have a default grammar `mul($v0, $v1)`.

### Customizing the parse

If you prefer, you can customize the parse by annotating your term with `#[customize(parse)]`. In the Rust case, for example, the parsing of `RigidTy` is customized ([as is the debug impl](./debug.md)):

```rust
{{#include ../../../crates/formality-types/src/grammar/ty.rs:RigidTy_decl}}
```

You must then supply an impl of `Parse` yourself. Because `Parse` is a trait alias, you actually want to implement `CoreParse<L>` for your language type `L`. Inside the code you will want to instantiate a `Parser` and then invoke `parse_variant` for every variant, finally invoking `finish`.

In the Rust code, the impl for `RigidTy` looks as follows:


```rust
{{#include ../../../crates/formality-types/src/grammar/ty/parse_impls.rs:RigidTy_impl}}
```