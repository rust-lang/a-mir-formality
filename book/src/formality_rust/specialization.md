# Branch specialization

`#![feature(branch_specialization)]`: a generic function branches on whether
a trait bound holds, without the soundness hole of trait specialization.

## The problem

rustc selects impls at codegen, after erasing lifetimes, so an impl that
applies only for some lifetimes is selected for types it does not apply to:

```rust
trait Static {}
impl Static for &'static u32 {}

fn spec<'a>(x: &'a u32) {
    // Decided after erasure, `&'erased u32: Static` holds for every `'a`.
    if impls &'a u32: Static { .. } else { .. }
}
```

Any design that *re-derives* the decision after erasure has this problem.

## The design

Decide during type checking, where region information is available; let
codegen re-evaluate the bound on erased types only where type checking has
settled the answer.

| Syntax | Meaning |
|---|---|
| `if impls WC { .. } else { .. }` | The then-branch assumes `WC`; the else-branch assumes nothing new. Only the branch taken is compiled. |
| `where may_spec(WC)` | The function needs to know whether `WC` holds. Every caller must *decide* it. |

Every `if impls WC`, and every call to a function declaring `may_spec(WC)`,
proves the obligation `may_spec(WC)`: "`WC` is decided". Nothing is passed
to codegen; these obligations are what make codegen's re-evaluation agree
(see "Deciding at codegen").

## Deciding a bound

{judgment}`decide`

`may_spec(WC)` holds by any of these rules:

| Rule | When | Answer |
|---|---|---|
| by bound | a `may_spec` bound in scope names `WC` (see below) | the caller's |
| holds | `WC` provable, with region constraints as the mode allows | yes |
| does not hold | `WC` mentions no type or const parameter and is unprovable even with every region constraint deferred | no |

Otherwise `WC` is *undecided*, an error:

```rust
trait Bar {}
impl Bar for u32 {}

fn undecided<T>() { if impls T: Bar { .. } }        // error: a downstream crate may impl Bar
fn spec<T>() where may_spec(T: Bar) { if impls T: Bar { .. } }
fn main() { spec::<u32>(); spec::<i32>() }          // closed bounds: decided by search
```

The "does not hold" rule is the only negative reasoning: explicit negative
impls are never consulted, and nothing negative is ever assumed, in the
else-branch included.

### What a `may_spec` bound decides

{judgment}`decide_by_bound`

Only the bound it names, structurally:

| In scope | Goal | Decided? |
|---|---|---|
| `may_spec(T: Sub)` | `T: Sub` | yes |
| `may_spec(T: Sub)` | `T: Super`, given `Sub: Super` | no |
| `may_spec(T: Super)` | `T: Sub` | no |
| `may_spec(for<'a> T: Tr<'a>)` | `T: Tr<'x>` | yes: `'a := 'x` |
| `may_spec(T: Tr<'x>)` | `for<'a> T: Tr<'a>` | no |

Deciding `T: Super` from `may_spec(T: Sub)` would be unsound, because the
local proof it skips is where region constraints come from:

```rust
trait Super {}
trait Sub: Super {}
impl Super for u32 {}
impl Sub for u32 {}
impl Super for &'static u32 {}

fn needs_super<T: Super>(t: T) {}
fn caller<'a>(x: &'a u32) where may_spec(&'a u32: Sub) {
    if impls &'a u32: Super { needs_super(x) }   // rejected: undecided
}
fn main() { let local = 0; caller(&local) }      // decides &'x u32: Sub as "no"
```

With the shortcut, `caller` type-checks with no `'a: 'static` anywhere,
`main` passes a local, and codegen evaluates `&'erased u32: Super` as true
through the `'static` impl: `needs_super` runs on a type that does not
implement `Super`.

A `for<'a>` bound decides its instances because its answer transfers: a
"yes" covers every `'x`, and a "no" means no impl matches at any lifetime,
which the erased instance cannot contradict. A family that fails only for
some lifetimes is never decided "no":

```rust
trait Tr<'a> {}
impl Tr<'static> for u32 {}

fn spec<'x, T>() where may_spec(for<'a> T: Tr<'a>) {
    if impls T: Tr<'x> { .. }           // decided by the bound
}
fn main() { spec::<'x, u32>() }         // rejected: for<'a> u32: Tr<'a> is undecided
```

Matching the impl leaves `'a: 'static` on the placeholder `'a`, which the
borrow checker verifies like any constraint and nothing discharges. Were
that a "no", codegen would evaluate `u32: Tr<'erased>`, which holds through
the `'static` impl, and run the then-branch for a `'x` that is not
`'static`. The reverse would take a "yes" for one `'x` as a "yes" for
every lifetime.

## Deciding at codegen

Codegen erases every lifetime (of the generic arguments, in `MonoKey`; of
the bound, in `decide_at_codegen`) and proves the bound on the concrete
types, a region constraint on an erased lifetime counting as satisfied.
Sound because of the obligations type checking proved:

| Decided at type checking as | After erasure |
|---|---|
| holds | the region constraints were checked where it was decided: signature or borrow checker |
| does not hold | unprovable even with constraints deferred, so unprovable after erasure |
| lifetime-dependent | never "no": a caller that cannot show the constraint is rejected |
| by bound | decided by a caller further up, for the same types |

So in no monomorphization type checking admits does codegen say "yes" where
the truth is "no".

## Lifetimes: the modes

`&'a u32: Static` (with `impl Static for &'static u32`) holds only under
`'a: 'static`. The modes differ in what a decision may do with that
constraint (`allowed_by_mode`):

| Mode | Feature gate | A decision may leave a region constraint? |
|---|---|---|
| strict (default) | — | No. The bound must follow from the signature (`where 'a: 'static`). |
| commit and verify | `spec_commit_and_verify` | Yes: registered with the borrow checker, like any goal's. |
| always applicable | `spec_always_applicable` | No, and the bound must hold for every choice of its lifetimes. |
| bail on regions | `spec_bail_on_regions` | No `may_spec` at all: codegen decides per monomorphization, "yes" only if the bound holds for every lifetime, else the else-branch, silently. Modeled on `try_as_dyn`. |

### Local regions

Strict and commit-and-verify agree on constraints between universal
regions (the borrow checker proves them from the same assumptions). They
differ on regions local to the body, which region inference solves for:

```rust
fn spec<'a>(x: &'a u32) {
    let y: &'x u32 = x;                 // 'x local, 'a: 'x
    if impls &'x u32: Static { .. }     // needs 'x: 'static
}
```

| In scope | strict | commit and verify |
|---|---|---|
| nothing | rejected: `'x: 'static` left behind | `'x: 'static` registered; inference reduces it to `'a: 'static` on the signature; borrowck rejects |
| `where 'a: 'static` | holds outright | holds outright |

The type checker hands an equation of a body region variable to the borrow
checker as two outlives constraints rather than substituting: regions are
inference's to solve. Strict cannot be stated in rustc, where every body
lifetime is an inference variable and even `'x == 'a` is a constraint;
commit-and-verify is the natural mode there.

## Out of scope

Impl specialization (`default fn`, `default type`, `default impl`). Impl
specialization of fns is branch specialization plus a hook trait, and
modeling it needs trait fn calls, which formality does not have yet.
