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

### Resolving ambiguity, greedy parsing

When parsing an enum there will be multiple possibilities. We will attempt to parse them all. If more than one succeeds, the parser will attempt to resolve the ambiguity by looking for the **longest match**. However, we don't just consider the number of characters, we look for a **reduction prefix**:

- When parsing, we track the list of things we had to parse. If there are two variants at the same precedence level, but one of them had to parse strictly more things than the other and in the same way, we'll prefer the longer one. So for example if one variant parsed a `Ty` and the other parsed a `Ty Ty`, we'd take the `Ty Ty`.
  - When considering whether a reduction is "significant", we take casts into account. See `ActiveVariant::mark_as_cast_variant` for a more detailed explanation and set of examples.

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
{{#include ../../../crates/formality-types/src/grammar/ty.rs:RigidTy_decl}}
```

You must then supply an impl of `Parse` yourself. Because `Parse` is a trait alias, you actually want to implement `CoreParse<L>` for your language type `L`. Inside the code you will want to instantiate a `Parser` and then invoke `parse_variant` for every variant, finally invoking `finish`.

In the Rust code, the impl for `RigidTy` looks as follows:

```rust
{{#include ../../../crates/formality-types/src/grammar/ty/parse_impls.rs:RigidTy_impl}}
```
