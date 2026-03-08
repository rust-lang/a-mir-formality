# Parsing

Formality's `#[term]` and `#[grammar]` attributes let you specify a grammar for your structs and enums, and the parser will automatically parse strings into those types. Here is a simple example:

```rust
#[term]
pub enum Expr {
    #[grammar($v0)]
    LocalVariable(Id),

    #[grammar($v0 $(v1))]
    FnCall(Id, Vec<Id>),
}

formality_core::id!(Id);
```

This defines an `Expr` that is either a local variable (a single identifier like `x`) or a function call (like `f(x, y)`). The `#[grammar]` attribute on each variant describes how to parse it: `$v0` means "parse the first field" and `$(v1)` means "parse a parenthesized, comma-separated list for the second field." For structs, the grammar goes directly on `#[term]`:

```rust
#[term($name : $ty)]
pub struct TypedBinding {
    name: Id,
    ty: Ty,
}
```

This parses strings like `x : i32`.

### Symbols

A grammar consists of a series of _symbols_. Each symbol matches some text in the input string. Symbols come in two varieties:

- Most things are _terminals_ or _tokens_: this means they just match themselves:
  - For example, the `*` in `#[grammar($v0 * $v1)]` is a terminal, and it means to parse a `*` from the input.
  - Delimiters are accepted but must be matched, e.g., `( /* tokens */ )` or `[ /* tokens */ ]`.
- The `$` character is used to introduce special matches. Generally these are _nonterminals_, which means they parse the contents of a field, where the grammar for a field is determined by its type.
  - If fields have names, then `$field` should name the field.
  - For positional fields (e.g., the T and U in `Mul(Expr, Expr)`), use `$v0`, `$v1`, etc.
- Valid uses of `$` are as follows:
  - `$field` -- just parse the field's type
  - `$*field` -- the field must be a collection of `T` (e.g., `Vec<T>`, `Set<T>`) -- parse any number of `T` instances. Something like `[ $*field ]` would parse `[f1 f2 f3]`, assuming `f1`, `f2`, and `f3` are valid values for `field`.
  - `$,field` -- similar to the above, but uses a comma separated list (with optional trailing comma). So `[ $,field ]` will parse something like `[f1, f2, f3]`.
  - `$?field` -- will parse `field` and use `Default::default()` value if not present.
  - `$<field>` -- parse `<E1, E2, E3>`, where `field` is a collection of `E`
  - `$<?field>` -- parse `<E1, E2, E3>`, where `field` is a collection of `E`, but accept empty string as empty vector
  - `$(field)` -- parse `(E1, E2, E3)`, where `field` is a collection of `E`
  - `$(?field)` -- parse `(E1, E2, E3)`, where `field` is a collection of `E`, but accept empty string as empty vector
  - `$[field]` -- parse `[E1, E2, E3]`, where `field` is a collection of `E`
  - `$[?field]` -- parse `[E1, E2, E3]`, where `field` is a collection of `E`, but accept empty string as empty vector
  - `${field}` -- parse `{E1, E2, E3}`, where `field` is a collection of `E`
  - `${?field}` -- parse `{E1, E2, E3}`, where `field` is a collection of `E`, but accept empty string as empty vector
  - `$:guard <nonterminal>` -- parses `<nonterminal>` but only if the keyword `guard` is present. For example, `$:where $,where_clauses` would parse `where WhereClause1, WhereClause2, WhereClause3` but would also accept nothing (in which case, you would get an empty vector).
  - `$!` -- marks a commit point, see the section on [commit points](#commit-points) below
  - `$$` -- parse the terminal `$`

### Default grammar

If no grammar is supplied, the default grammar is determined as follows:

- If a `#[cast]` or `#[variable]` annotation is present, then the default grammar is just `$v0`.
- Otherwise, the default grammar is the name of the type (for structs) or variant (for enums), followed by `()`, with the values for the fields in order. So `Mul(Expr, Expr)` would have a default grammar `mul($v0, $v1)`.

### Ambiguity: parse all possibilities, disambiguate at the top

The parser takes a **parse-all-possibilities** approach. When parsing an enum, all variants are attempted, and *all* successful parses are returned — not just the "best" one. Ambiguity in a child nonterminal propagates upward through the parse tree, producing a cross-product of possibilities. For example, if a struct has two fields and each field's nonterminal is 2-ways ambiguous, the struct produces 2 × 2 = 4 successful parses.

Consider this example, which parses either a single identifier (e.g., `x`) or a "call" (e.g., `x(y, z)`):

```rust
#[term]
pub enum Expr {
    #[grammar($v0)]
    LocalVariable(Id),

    #[grammar($v0 $(v1))]
    FnCall(Id, Vec<Id>),
}
```

Given an input like `x(y, z)`, the result is actually *ambiguous*. It could be that you parsed as an expression `x` with remaining text `(y, z)` or as a call expression; therefore, the parser returns both.

Disambiguation happens once, at the **top level** (when you call `term("...")` or `core_term_with`). Here we would filter out the parse as `x` because it failed to consume all the input. That leaves exactly one parse, and so `term` succeeds. If multiple distinct parses remain, the result is an **ambiguity panic** — this means your grammar genuinely has an unresolvable ambiguity for that input.

Sometimes disambiguation happens during parsing. For example, if we add another layer the example, like `Sum`:

```rust
#[term]
pub enum Expr {
    #[grammar($v0)]
    LocalVariable(Id),

    #[grammar($v0 $(v1))]
    FnCall(Id, Vec<Id>),
}

#[term]
pub enum Sum {
    #[grammar($v0 + $v1)]
    Add(Expr, Expr),
}
```

Now when parsing `x(y) + z` as a `Sum`, we would recursively parse it as an `Expr` and encounter the same two results:

* parsed `x` as a `Expr::LocalVariable`, remaining text `(y) + z`
* parsed `x(y)` as a `Expr::FnCall`, remaining text `+ z`

However, only the second one can be parsed as a `Sum`, because the `Sum` requires the next character to be `+`.

### Failure, _almost_ succeeding, and commit points

When parsing fails, we distinguish two cases:

- **Failure** — the input clearly didn't match. Usually this means the first token wasn't a valid start for the nonterminal. You'll get an error like "expected an `Expr`".
- **_Almost_ succeeded** — we got part-way through parsing, consuming some tokens, but then hit an error. For example, parsing `"1 / / 3"` as an `Expr` might give "expected an `Expr`, found `/`".

The distinction matters when parsing optional (`$?field`) or repeated (`$*field`) nonterminals. If parsing `field` outright **fails**, we treat the field as absent and continue with its `Default::default()` value. If parsing `field` **almost succeeds**, we assume it was present but malformed, and report a syntax error.

The default rule is that parsing "almost" succeeds if it consumes at least one token. For example:

```rust
#[term]
enum Projection {
  #[grammar(. $v0)]
  Field(Id),
}
```

Parsing `".#"` as a `Projection` would "almost" succeed — it consumes the `.` but then fails to find an identifier.

Sometimes this default is too aggressive. Consider `Projection` embedded in another type:

```rust
#[term($*projections . #)]
struct ProjectionsThenHash {
  projections: Vec<Projection>,
}
```

We'd like `".#"` to be a valid `ProjectionsThenHash` — zero projections followed by `.#`. But the parser sees the `.` as an "almost success" of a `Projection`, so it reports a syntax error instead of treating the projections list as empty.

#### Commit points

You can control this with `$!`, which marks a **commit point**. With `$!`, a parse is only considered to have "almost succeeded" if it reached the commit point. Before the commit point, failure is just failure (the variant wasn't present).

Adding `$!` after the identifier in `Projection` fixes the problem:

```rust
#[term]
enum Projection {
  #[grammar(. $v0 $!)]
  Field(Id),
}
```

Now `.#` fails outright (the commit point after `$v0` was never reached), so `ProjectionsThenHash` correctly treats the projections list as empty and parses `.#` as the trailing literal tokens.

See the `parser_torture_tests::commit_points` code for a working example.

### Variables and scope

Most compilers parse source text into an ambiguous syntax tree first, then apply name resolution and semantics to figure out what things mean. Formality aims to parse more directly into a semantically meaningful representation — for example, distinguishing variables from identifiers *during* parsing rather than in a later pass.

Today this shows up in one place: **variable scope**. When you write `<X> X`, the binder `<X>` introduces `X` into scope, and the body `X` is parsed as a variable reference rather than an identifier. This is the one piece of context the parser carries, but we may expand context-sensitive parsing in the future.

To see why this matters, let's extend our `Expr` example with a `#[variable]` variant:

```rust
#[term]
pub enum Expr {
    #[variable(Kind::Expr)]
    Variable(Variable),

    #[grammar($v0)]
    Name(Id),

    #[grammar($v0 ( $,v1 ))]
    FnCall(Id, Vec<Expr>),
}
```

#### `#[variable]` variants get priority

When an enum has a `#[variable]` variant, the parser tries it **first**, before any other variants. If the name is found in scope with the correct kind, the variable parse succeeds and the remaining variants are skipped entirely.

With `X` in scope, consider parsing `X(y, z)`:

1. The `Variable` variant tries first, finds `X` in scope — succeeds with `Variable(X)`, remaining text `(y, z)`.
2. The `Name` and `FnCall` variants are never tried.

At the top level, `Variable(X)` didn't consume all the input, so this is a **parse error**. The `#[variable]` short-circuit prevented the `FnCall` parse from ever being attempted.

This is often the right behavior for a type grammar — if `T` is a type variable, then `T` is always a type variable, never a type name. But it's a tradeoff: the short-circuit is aggressive, and it means the parser commits to the variable interpretation without seeing the surrounding context. For our `Expr` grammar, we probably don't want `#[variable]` at all — function names are identifiers, not variables. We'd remove the `#[variable]` variant and let the parse-all-possibilities approach handle things naturally, as we saw in the [Ambiguity section](#ambiguity-parse-all-possibilities-disambiguate-at-the-top).

#### `match_var = false` on `id!` types

The `#[variable]` short-circuit resolves ambiguity *within* an enum — when the variable variant is present, it wins. But what about *indirect* ambiguity, where a variable and an identifier compete in different variants of a parent enum?

The `id!` macro generates identifier types. By default, `id!()` types will happily parse any valid identifier string, even if that string happens to be a variable in scope. To make an `id!` type reject variable names, declare it with `match_var = false`:

```rust
formality_core::id!(TraitId, match_var = false);
```

To see why this is useful, imagine a simplified type system where types are *only* type variables — no named types like `i32`. A bound like `T : U` could mean either "`T` implements trait `U`" or "`T` is a subtype of type `U`":

```rust
#[term]
pub enum Ty {
    #[variable(Kind::Ty)]
    Variable(Variable),
}

#[term]
enum Bound {
    #[grammar($v0 : $v1)]
    Impl(Ty, TraitId),

    #[grammar($v0 : $v1)]
    Subtype(Ty, Ty),
}

formality_core::id!(TraitId);  // default: accepts any identifier
```

With `T` and `U` in scope as type variables, parsing `T : U` is ambiguous. The `Subtype` variant parses `U` as a `Ty`, which succeeds via the `#[variable]` rule. The `Impl` variant parses `U` as a `TraitId` — and since `id!(TraitId)` accepts any identifier string (including variable names), it also succeeds. Two parses, ambiguity panic.

Adding `match_var = false` fixes this:

```rust
formality_core::id!(TraitId, match_var = false);  // rejects variables in scope
```

Now when parsing `T : U`, the `Impl` variant tries to parse `U` as a `TraitId`. Because `U` is a variable in scope, `match_var = false` rejects it, so only `Subtype` succeeds. Parsing `T : Debug` (where `Debug` is *not* a variable) would still succeed as `Impl`, because `match_var = false` only rejects names that are actually variables in scope.

Identifiers that appear in positions where they *should* accept variable names — like associated item names after `::` in `<T as Trait>::Item` — can use the default `id!(AssociatedItemId)` which accepts any valid identifier string.

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

- `1 + 1 + 1` when left-associative is `(1 + 1) + 1`
- `1 + 1 + 1` when right-associative is `1 + (1 + 1)`
- `1 + 1 + 1` when none-associative is an error.

### Customizing the parse

If you prefer, you can customize the parse by annotating your term with `#[customize(parse)]`. In the Rust case, for example, the parsing of `RigidTy` is customized ([as is the debug impl](./debug.md)):

```rust
{{#include ../../../crates/formality-rust/src/grammar/ty.rs:RigidTy_decl}}
```

You must then supply an impl of `CoreParse<L>` for your language type `L`. Inside the impl you will want to instantiate a `Parser` and then invoke `parse_variant` for every variant. The key methods on `ActiveVariant` are:

- **`each_nonterminal(|value: T, p| { ... })`** — the primary way to parse a child nonterminal. Instead of returning a single result, it calls the continuation for *each* successful parse of `T`, passing a forked `ActiveVariant` positioned at that parse's remaining text. This is how ambiguity propagates: if `T` has 2 parses, the continuation runs twice, and the results are collected.
- **`each_comma_nonterminal(|items: Vec<T>, p| { ... })`** — parse a comma-separated list, then run the continuation with the result.
- **`each_opt_nonterminal(|opt: Option<T>, p| { ... })`** — parse an optional nonterminal.
- **`each_many_nonterminal(|items: Vec<T>, p| { ... })`** — parse zero or more nonterminals.
- **`each_delimited_nonterminal(open, optional, close, |items: Vec<T>, p| { ... })`** — parse a delimited comma-separated list (e.g., `<T1, T2>`).
- **`p.ok(value)`** — wrap a value into a successful parse result. Use this instead of `Ok(value)` at the end of variant closures.
- **`expect_char(c)`**, **`expect_keyword(kw)`** — consume expected tokens.
- **`reject_nonterminal::<T>()`** — fail if the input starts with a `T` (useful for disambiguation).

The continuation-passing style means nonterminal parsing is always nested: you parse the first field, then in its continuation parse the second field, and so on. Each `each_*` call forks for each ambiguous child result, so you get the cross-product automatically.

In the Rust code, the impl for `RigidTy` looks as follows:

```rust
{{#include ../../../crates/formality-rust/src/grammar/ty/parse_impls.rs:RigidTy_impl}}
```
