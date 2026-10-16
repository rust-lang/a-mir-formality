# formality-mdbook-skills

An independent mdBook 0.5 preprocessor for including agent skill documentation
without its YAML frontmatter. It has no dependencies on the formality crates.

Install from this checkout with `cargo install --path crates/formality-mdbook-skills`.
Once published, install with `cargo install formality-mdbook-skills`.

Add this to your book's `book.toml`:

```toml
[preprocessor.skills]
command = "formality-mdbook-skills"
before = ["links"]
```

Then include a skill in a chapter:

```markdown
{{#skill ../../skills/my-skill/SKILL.md}}
```

Paths are relative to the chapter's source file, may contain spaces, and may
point outside the book directory. The file's leading YAML frontmatter is parsed
and removed; everything after it is preserved. Files without frontmatter are
included in full. Frontmatter must be a YAML mapping with a closing `---` or
`...` delimiter. Missing files and malformed frontmatter fail the build with
both chapter and skill paths in the error.

Skill insertions are a single pass: nested `#skill` directives are not expanded.
Links, images, and ordinary mdBook includes in the inserted body resolve in the
including chapter's context; they are not rebased to the skill directory.
Like mdBook's built-in includes, directives also expand inside code fences.

For development in this workspace, set
`command = "cargo run --quiet -p formality-mdbook-skills --"` under `[preprocessor.skills]`.
The binary implements mdBook's JSON stdin/stdout protocol and supports all
renderers. The library also exports `SkillsPreprocessor` for embedding.
