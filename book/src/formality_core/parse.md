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
enum MyEnum {
    #[grammar( /* grammar here */ )]
    Foo(...),
}
```

### Succeeding, failing, and _almost_ succeeding

When you attempt to parse something, you'll get back a `Result`: either the parse succeeded (`Ok`), or it didn't (`Err`). But we actually distinguish three outcomes:

- Success: we parsed a value successfully. We generally implement a **greedy** parse, which means we will attempt to consume as many things we can. As a simple example, imagine you are parsing a list of numbers. If the input is `"1, 2, 3"`, we could choose to parse just `[1, 2]` (or indeed just `[1]`), but we will instead parse the full list.
  - For you parsing nerds, this is analogous to the commonly used rule to prefer shifts over reduces in LR parsers.
- Failure: we tried to parse the value, but it clearly did not correspond to the thing we are looking for. This usually means that the first token was not a valid first token. This will give a not-very-helpful error message like "expected an `Expr`" (assuming we are parsing an `Expr`).
- _Almost_ succeeded: this is a special case of failure where we got part-way through parsing, consuming some tokens, but then encountered an error. So for example if we had an input like `"1 / / 3"`, we might give an error like "expected an `Expr`, found `/`". Exactly how many tokens we have to consume before we consider something to have 'almost' succeeded depends on the thing we are parsing (see the discussion on _commit points_ below).

Both failure and 'almost' succeeding correspond to a return value of `Err`. The difference is in the errors contained in the result. If there is a single error and it occurs at the start of the input (possibly after skipping whitespace), that is considered **failure**. Otherwise the parse "almost" succeeded. The distinction between failure and "almost" succeeding helps us to give better error messages, but it is also important for "optional" parsing or when parsing repeated items.

### Ambiguity: parse all possibilities, disambiguate at the top

The parser takes a **parse-all-possibilities** approach. When parsing an enum, all variants are attempted, and *all* successful parses are returned — not just the "best" one. Ambiguity in a child nonterminal propagates upward through the parse tree, producing a cross-product of possibilities. For example, if a struct has two fields and each field's nonterminal is 2-ways ambiguous, the struct produces 2 × 2 = 4 successful parses.

Disambiguation happens once, at the **top level** (when you call `term("...")` or `core_term_with`). After filtering to parses that consumed all input, duplicate values are removed. If exactly one parse remains, it succeeds. If multiple distinct parses remain, the result is an **ambiguity panic** — this means your grammar genuinely has an unresolvable ambiguity for that input.

The key advantage of the parse-all-possibilities design is that disambiguation has full context: a child nonterminal doesn't need to resolve ambiguity on its own, because the parent (or grandparent, or top-level caller) can see the bigger picture.

### Variables vs identifiers

When variables are in scope, there is a natural tension between parsing a name as a *variable* (a bound name) and parsing it as a plain *identifier* (like a type name or trait name). Two mechanisms work together to resolve this.

**Variable-first short-circuit.** When an enum has a `#[variable]` variant, the generated parser tries that variant *first*. If it succeeds, all other variants are skipped — the variable interpretation wins unconditionally. For example, given:

```rust
#[term]
enum Ty {
    #[variable(Kind::Ty)]
    Variable(Variable),

    #[cast]
    Id(TyId),
}
```

If `T` is a type variable in scope, parsing `T` as a `Ty` will match the `Variable` variant and immediately return — the `Id` variant is never attempted. If the input is `i32` (not a variable), the `Variable` variant fails, and the parser falls through to try `Id`.

**Scoped `id!` types.** The `id!` macro generates identifier types that reject names which are in scope as variables. This handles *indirect* ambiguity — cases where a variable and an identifier compete in different variants of a parent enum, not in the same enum. Consider this grammar:

```rust
#[term]
enum WhereClauseData {
    #[grammar($v0 : $v1 $<?v2>)]
    IsImplemented(Ty, TraitId, Vec<Parameter>),

    #[grammar($v0 : $v1)]
    Outlives(Parameter, Lt),
}
```

When parsing `T : a` where `a` is a lifetime variable, both variants match the `$v0 : $v1` prefix. The `Outlives` variant parses `a` as an `Lt` (which succeeds via the variable-first rule on `LtData`). The `IsImplemented` variant tries to parse `a` as a `TraitId`. Because `TraitId` is declared with plain `id!(TraitId)`, it rejects `a` (a variable in scope), so `IsImplemented` fails. Only `Outlives` succeeds, resolving the ambiguity.

**Unscoped `id!` types.** Some identifiers appear in positions where they *should* accept variable names — for example, associated item names after `::` in `<T as Trait>::Item`. Here, `Item` is an `AssociatedItemId`, and it might happen to share a name with a variable in scope. Since there is no competing variable interpretation in that position, we declare it as `id!(AssociatedItemId, unscoped)` to opt out of variable rejection.

### Variables and scope: context-sensitive parsing

Formality's parser is *almost* context-free, with one important exception: **variable scope**. When you write `<T> T`, the binder `<T>` introduces `T` into scope, and the body `T` is parsed as a variable reference rather than an identifier. This is the one piece of context that the parser carries.

This matters because variable names (like `T`) look identical to identifier names (like `TraitId` or `AdtId`). Without scope awareness, parsing `T` is ambiguous: is it a variable or an identifier?

#### How it works: `#[variable]` variants get priority

When an enum has a `#[variable]` variant, the parser tries it **first**, before any other variants. If the name is found in scope with the correct kind, the variable parse succeeds and the remaining variants are skipped entirely.

```rust
#[term]
pub enum Ty {
    #[variable(Kind::Ty)]  // Tried first!
    Variable(Variable),

    #[cast]
    Id(Id),                // Only tried if Variable didn't match
}
```

When parsing `T` with `T` in scope as a type variable:
1. `Variable` variant: looks up `T` in scope, finds it with `Kind::Ty` — success, done.
2. `Id` variant: never tried.

When parsing `i32` (not in scope):
1. `Variable` variant: looks up `i32` in scope, not found — fails.
2. `Id` variant: parses `i32` as an identifier — success.

This is a **local** rule: each enum decides independently based on its own `#[variable]` variant. There is no global disambiguation.

#### The `match_var = false` option on `id!()`

By default, `id!()` types will happily parse any valid identifier string, even if that string happens to be a variable in scope. This is usually fine — in a grammar like `$v0 :: $v1`, the `$v1` position clearly expects an identifier (like a field or associated type name), even if the name coincidentally matches a variable.

However, if you have a grammar where an `id!()` type appears in a position that *could also* be a variable, you may get ambiguity. In that case, you can opt into scope checking:

```rust
formality_core::id!(Id, match_var = false);
```

With `match_var = false`, the identifier will reject any string that matches a variable currently in scope, eliminating the ambiguity. Use this when the `id!()` type competes directly with a `#[variable]` variant at the same level.

The default is `match_var = true` (i.e., no rejection) because identifiers often appear in positions where variable names are valid strings but not valid variables — like field names, associated type names, or other contexts where scope doesn't apply.

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

### Symbols

A grammar consists of a series of _symbols_. Each symbol matches some text in the input string. Symbols come in two varieties:

- Most things are _terminals_ or _tokens_: this means they just match themselves:
  - For example, the `*` in `#[grammar($v0 * $v1)]` is a terminal, and it means to parse a `*` from the input.
  - Delimeters are accepted but must be matched, e.g., `( /* tokens */ )` or `[ /* tokens */ ]`.
- The `$` character is used to introduce special matches. Generally these are _nonterminals_, which means they parse the contents of a field, where the grammar for a field is determined by its type.
  - If fields have names, then `$field` should name the field.
  - For position fields (e.g., the T and U in `Mul(Expr, Expr)`), use `$v0`, `$v1`, etc.
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
  - `$!` -- marks a commit point, see the section on greediness below
  - `$$` -- parse the terminal `$`

### Commit points and greedy parsing

When you parse an optional (e.g., `$?field`) or repeated (e.g., `$*field`) nonterminal, it raises an interesting question. We will attempt to parse the given field, but how do we treat an error? It could mean that the field is not present, but it also could mean a syntax error on the part of the user. To resolve this, we make use of the distinction between failure and _almost_ succeeding that we introduced earlier:

- If parsing `field` outright **fails**, that means that the field was not present, and so the parse can continue with the field having its `Default::default()` value.
- If parsing `field` **almost succeeds**, then we assume it was present, but there is a syntax error, and so parsing fails.

The default rule is that parsing "almost" succeeds if it consumes at least one token. So e.g. if you had...

```rust
#[term]
enum Projection {
  #[grammar(. $v0)]
  Field(Id),
}
```

...and you tried to parse `".#"`, that would "almost" succeed, because it would consume the `.` but then fail to find an identifier.

Sometimes this rule is not quite right. For example, maybe the `Projection` type is embedded in another type like

```rust
#[term($*projections . #)]
struct ProjectionsThenHash {
  projections: Vec<Projection>,
}
```

For `ProjectionsThenHash`, we would consider `".#"` to be a valid parse -- it starts out with no projections and then parses `.#`. But if you try this, you will get an error because the `.#` is considered to be an "almost success" of a projection.

You can control this by indicating a "commit point" with `$!`. If `$!` is present, the parse is failure unless the commit point has been reached. For our grammar above, modifying `Projection` to have a commit point _after_ the identifier will let `ProjectionsThenHash` parse as expected:

```rust
#[term]
enum Projection {
  #[grammar(. $v0 $!)]
  Field(Id),
}
```

See the `parser_torture_tests::commit_points` code for an example of this in action.

### Default grammar

If no grammar is supplied, the default grammar is determined as follows:

- If a `#[cast]` or `#[variable]` annotation is present, then the default grammar is just `$v0`.
- Otherwise, the default grammar is the name of the type (for structs) or variant (for enums), followed by `()`, with the values for the fields in order. So `Mul(Expr, Expr)` would have a default grammar `mul($v0, $v1)`.

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
