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

#### How variable/identifier disambiguation works

The problem: when an enum has both a `#[variable]` variant and fields that use `id!` types, the same text could parse as either a variable or an identifier. Without disambiguation, this produces an ambiguity panic.

The solution: when the parser begins parsing an enum that has a `#[variable]` variant, it runs an upfront **probe** — "can this text be parsed as any in-scope variable?" If yes, it sets a flag on the parser stack. All `id!` types check this flag before accepting an identifier and reject the text if the flag is set.

To see how this works, consider a type system with types and permissions, where types can be variables, named types, or a permission applied to a type:

```rust
#[term]
pub enum Ty {
    #[variable(Kind::Ty)]
    Variable(Variable),

    #[cast]
    Id(Id),

    #[grammar($v0 $v1)]
    Apply(Perm, Arc<Ty>),

    #[grammar($v0 :: $v1)]
    Assoc(Arc<Ty>, AssocId),
}

#[term]
pub enum Perm {
    #[variable(Kind::Perm)]
    Variable(Variable),
}

formality_core::id!(Id);
formality_core::id!(AssocId);
```

**Same-type disambiguation.** With type variable `T` in scope, parse `T` as a `Ty`. Because `Ty` has a `#[variable]` variant, the parser probes: "can `T` be parsed as any in-scope variable?" Yes — `T` is a type variable. The flag is set. Now when the `Id` variant tries to parse `T`, `Id` (an `id!` type) checks the flag, sees it's set, and rejects. Only `Variable(T)` succeeds. Without the flag, both `Variable(T)` and `Id(T)` would succeed — ambiguity panic.

**Cross-type disambiguation.** With perm variable `P` in scope, parse `P i32` as a `Ty`. The parser begins parsing `Ty` at `P` and runs the probe. The probe asks "can `P` be parsed as any in-scope variable?" — and it can, because `P` is a perm variable. The probe checks for variables **of any kind**, not just the kind associated with this type's `#[variable]` variant. So the flag is set. `Id` rejects `P`. But `Perm`'s `#[variable]` variant *does* match `P`, so the `Apply` variant succeeds: `Apply(Variable(P), Id(i32))`.

This cross-kind behavior is what makes the mechanism work across type boundaries. `Ty`'s probe finds the perm variable `P` even though `Ty`'s own `#[variable]` variant only accepts type variables. This prevents `Id` from claiming a name that belongs to a sibling type's variable namespace.

**Positional scoping.** Parse `i32::T` as a `Ty`, with type variable `T` in scope. The outer `Ty` parse starts at `i32::T`. The probe asks "can `i32::T` be parsed as a variable?" No — `i32` isn't a variable name, so no flag is set. The `Assoc` variant parses `i32` as a `Ty`, consumes `::`, then parses `T` as an `AssocId`. At that text position, no type with a `#[variable]` variant is being parsed (only `AssocId`, an `id!` type, is being parsed here), so no probe has run. `AssocId` accepts `T` as a plain identifier. This is the right behavior — `T` after `::` is an associated item name, not a variable reference.

See `tests/parser_var_id_ambiguity.rs` for working tests of all three scenarios.

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
