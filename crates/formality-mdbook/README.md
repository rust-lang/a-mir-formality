# formality-mdbook

An [mdbook](https://rust-lang.github.io/mdBook/) preprocessor that extracts `judgment_fn!` definitions and anchored code snippets from Rust source files, rendering them as figures in your book.

## Installation

```sh
cargo install --path crates/formality-mdbook
```

Or, if published:

```sh
cargo install formality-mdbook
```

## Configuration

Add the preprocessor to your `book.toml`:

```toml
[preprocessor.judgment]
command = "formality-mdbook"

# Directory containing Rust source files to scan (relative to book root).
# Default: "src"
source-dir = "crates"

# Base URL for [src] links. Optional — if omitted, falls back to
# output.html.git-repository-url (appending /blob/main).
# If neither is set, no source links are generated.
# github-base = "https://github.com/rust-lang/a-mir-formality/blob/main"
```

## Usage

The preprocessor recognizes three reference types in your markdown:

### Judgments

Renders the signature and doc comment of a `judgment_fn!`:

```markdown
{judgment}`move_place`
```

This finds the function named `move_place` inside a `judgment_fn! { ... }` block and renders its signature.

### Judgment rules

Renders a single inference rule from within a judgment:

```markdown
{judgment-rule}`move_place, copy`
```

This renders the rule named `"copy"` from the `move_place` judgment. Rules are identified by the separator line pattern:

```rust
----------------------------------- ("copy")
```

### Anchors

Renders a named code snippet delimited by `// ANCHOR:` comments:

```markdown
{anchor}`Env`
```

This finds and renders the code between matching markers in your source:

```rust
// ANCHOR: Env
pub struct Env {
    program: Arc<Program>,
    local_variables: Map<Var, Ty>,
}
// ANCHOR_END: Env
```

## Output

Each reference is rendered as an HTML `<figure>` with:

- A permalink heading
- A `[src]` link to the source on GitHub (if `github-base` is configured)
- The code in a `rust,ignore` fenced block
- For judgments: the doc comment rendered below the code
