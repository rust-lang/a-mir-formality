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

### Ambiguity,

When parsing an enum there will be multiple possibilities. We will attempt to parse them all. If more than one succeeds, the parse is deemed ambiguous and an error is reported. If zero succeed, we will report back a parsing error, attempting to localize the problem as best we can.

### Symbols

A grammar consists of a series of *symbols*. Each symbol matches some text in the input string. Symbols come in two varieties:

* Most things are *terminals* or *tokens*: this means they just match themselves:
    * For example, the `*` in `#[grammar($v0 * $v1)]` is a terminal, and it means to parse a `*` from the input.
    * Delimeters are accepted but must be matched, e.g., `( /* tokens */ )` or `[ /* tokens */ ]`.
* Things beginning with `$` are *nonterminals* -- they parse the contents of a field. The grammar for a field is generally determined from its type.
    * If fields have names, then `$field` should name the field.
    * For position fields (e.g., the T and U in `Mul(Expr, Expr)`), use `$v0`, `$v1`, etc.
    * Exception: `$$` is treated as the terminal `'$'`.
* Nonterminals can also accept modes:
    * `$field` -- just parse the field's type
    * `$*field` -- the field must be a `Vec<T>` -- parse any number of `T` instances. Something like `[ $*field ]` would parse `[f1 f2 f3]`, assuming `f1`, `f2`, and `f3` are valid values for `field`.
        * If the next token after `$*field` is a terminal, `$*` uses  it as lookahead. The grammar `[ $*field ]`, for example, would stop parsing once a `]` is observed. 
        * If the `$*field` appears at the end of the grammar or the next symbol is a nonterminal, then the type of `field` must define a grammar that begins with some terminal or else your parsing will likely be ambiguous or infinite.
    * `$,field` -- similar to the above, but uses a comma separated list (with optional trailing comma). So `[ $,field ]` will parse something like `[f1, f2, f3]`.
        * Lookahead is required for `$,field` (this could be changed).
    * `$?field` -- will parse `field` and use `Default::default()` value if not present.

### Greediness

Parsing is generally greedy. So `$*x` and `$,x`, for example, consume as many entries as they can, subject to lookahead. In a case like `$*f $*g`, the parser would parse as many `f` values as it could before parsing `g`. If some entry can be parsed as either `f` or `g`, the parser will not explore all possibilities. The caveat here is lookahead: given `$*f #`, for example, parsing will stop when `#` is observed, even if `f` could parse `#`.

### Default grammar

If no grammar is supplied, the default grammar is determined as follows:

* If a `#[cast]` or `#[variable]` annotation is present, then the default grammar is just `$v0`.
* Otherwise, the default grammar is the name of the type (for structs) or variant (for enums), followed by `()`, with the values for the fields in order. So `Mul(Expr, Expr)` would have a default grammar `mul($v0, $v1)`.
