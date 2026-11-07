# Branch specialization

`#![feature(branch_specialization)]`: a generic function branches on whether
a trait bound holds, without the soundness hole of trait specialization.

## The problem

Selecting impls after lifetime erasure can select an impl for types it does
not apply to:

```rust
trait Static {}
impl Static for &'static u32 {}

fn spec<'a>(x: &'a u32) {
    // Decided after erasure, `&'erased u32: Static` holds for every `'a`.
    if impls &'a u32: Static { .. } else { .. }
}
```

Re-deriving the answer after erasure is therefore insufficient on its own.

## The design

Decide during type checking, where region information is available; let
codegen re-evaluate the bound on erased types only where type checking has
settled the answer. The [lifetime modes](#lifetimes-the-modes) below vary
when decisions are required and how they use region information.

| Syntax | Meaning |
|---|---|
| `if impls WC { .. } else { .. }` | The then-branch assumes `WC`; the else-branch assumes nothing new. Only the branch taken is compiled. |
| `where may_spec(WC)` | The function needs to know whether `WC` holds. Every caller must *decide* it. |

`WC` is a trait bound; a higher-ranked one is written like any where-clause,
`for<'a> may_spec(T: Trait<'a>)`.

Every call to a function declaring `may_spec(WC)` proves that obligation:
"`WC` is decided". Every `if impls WC` does too, except in bail-on-regions
mode. Nothing is passed to codegen; these obligations are what make
codegen's re-evaluation agree (see "Deciding at codegen").

## Deciding a bound

{judgment}`decide`

`may_spec(WC)` holds by any of these rules (of several answers, the one
with the weakest region constraints is taken; see "Several answers"):

| Rule | When | Answer |
|---|---|---|
| always applicable trait | `WC` is a bound on an `always_applicable` trait (see below) | codegen's, exactly |
| by bound | a `may_spec` bound in scope names `WC` (see below) | the caller's |
| holds | `WC` provable, with region constraints as the mode allows | yes |
| does not hold | `WC` mentions no type or const parameter and is unprovable, even with every region constraint deferred | no |
| does not hold, by coherence | `WC` mentions a type parameter; no impl applies to any instantiation and none can be added, neither downstream (orphan rules) nor upstream in a minor release | no |

A higher-ranked goal (`for<'a> may_spec(T: Tr<'a>)` at a call, or
`if impls for<'a> T: Tr<'a>`) is decided for a fresh universal `'a`, like
any `for<..>` goal: "does not hold" then means no choice of `'a` makes the
bound provable, and a decision may not constrain `'a`.

Otherwise `WC` is *undecided*, an error:

```rust
trait Bar {}
impl Bar for u32 {}

fn undecided<T>() { if impls T: Bar { .. } }        // error: a downstream crate may impl Bar
fn spec<T>() where may_spec(T: Bar) { if impls T: Bar { .. } }
fn main() { spec::<u32>(); spec::<i32>() }          // closed bounds: decided by search
```

The "does not hold" rules are the only negative reasoning: explicit negative
impls are never consulted, and nothing negative is ever assumed, in the
else-branch included.

### What a `may_spec` bound decides

{judgment}`decide_by_bound`

Only the bound it names, up to unification of the parameters. The mode
determines whether that unification may leave region constraints:

| In scope | Goal | Decided? |
|---|---|---|
| `may_spec(T: Sub)` | `T: Sub` | yes |
| `may_spec(&'a u32: Tr)` | `&'x u32: Tr` | yes, with `'x == 'a` (a region constraint; see the modes) |
| `may_spec(T: Sub)` | `T: Super`, given `Sub: Super` | no |
| `may_spec(T: Super)` | `T: Sub` | no |
| `for<'a> may_spec(T: Tr<'a>)` | `T: Tr<'x>` | yes: `'a := 'x` |
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

fn spec<'x, T>() where for<'a> may_spec(T: Tr<'a>) {
    if impls T: Tr<'x> { .. }           // decided by the bound
}
fn main() { spec::<'x, u32>() }         // rejected: for<'a> u32: Tr<'a> is undecided
```

Matching the impl needs `'a: 'static`, which does not hold for every `'a`,
so the bound is not a "yes". An instance (`u32: Tr<'static>`) does hold, so
it is not a "no" either. Were it a "no", codegen would evaluate
`u32: Tr<'erased>`, which holds through the `'static` impl, and run the
then-branch for a `'x` that is not `'static`. The reverse would take a "yes"
for one `'x` as a "yes" for every lifetime.

### Several answers

A bound can be decided in more than one way: `&'x u32: Static` by a
`may_spec(&'a u32: Static)` in scope, with `'x == 'a`, and by the impl for
`&'static u32`, with `'x: 'static`. The type checker takes the answer whose
region constraints are the weakest: those that each of the others entails,
given what is already known.

* An answer that leaves no region constraint is always the weakest,
  wherever it comes from. This keeps commit-and-verify a superset of
  strict.
* `'x: 'static` entails `'x: 'a`, so with `'a: 'x` known (`'x` reborrows
  `'a`) the bound's `'x == 'a` is the weaker, and the caller's answer
  decides.
* Answers that cannot be compared are an error: two bounds giving
  `'x == 'a` and `'x == 'b`, or the two above when `'x` is unrelated to
  `'a`.

## Deciding at codegen

Codegen erases lifetimes in both generic arguments and trait bounds.
Except in bail-on-regions mode, it then proves the bound on the concrete
types, treating constraints on erased lifetimes as satisfied. The
obligations proved during type checking make this sound:

| Decided at type checking as | After erasure |
|---|---|
| holds | the region constraints were checked where it was decided: signature or borrow checker |
| does not hold | no instance provable even with constraints deferred, so unprovable after erasure |
| lifetime-dependent | never "no": a caller that cannot show the constraint is rejected |
| by bound | decided by a caller further up, for the same types |

Thus codegen never selects the then-branch for a bound that does not hold.

## Lifetimes: the modes

The four modes differ in when an `if impls` must be decided and whether
its decision can impose lifetime requirements:

| Mode | Feature gate | How `if impls` is decided |
|---|---|---|
| strict (default) | — | At type checking, without leaving region constraints. Otherwise, error. |
| commit and verify | `spec_commit_and_verify` | At type checking, allowing region constraints for borrow checking to verify. Failed verification is an error. |
| bail on regions | `spec_bail_on_regions` | At codegen: then-branch only if the bound holds for every choice of lifetimes; else-branch otherwise. No `may_spec` obligation at the `if impls`. |
| always applicable | `spec_always_applicable` | As strict, but a "holds" proof must also work for every choice of lifetimes. `always_applicable` traits allow decisions at codegen (see below). |

Bail on regions still checks explicit `may_spec` bounds at calls, using
strict's constraint policy.

Examples from `tests/may_spec.rs`, with `impl Static for Tag<'static>`:

| Program | strict | commit and verify | bail on regions | always applicable |
|---|---|---|---|---|
| `if impls Tag<'a>: Static`, nothing in scope | error | error (borrowck) | else-branch | error |
| ... with `where 'a: 'static` | then | then | else-branch | error |
| ... with `may_spec`, caller passes `'static` | then | then | else-branch | error |
| ... with `may_spec`, caller passes a free local region | error | then (inference picks `'static`) | error | error |
| `if impls Tag<'x>: Static`, `'x` a free local region | error | then | else-branch | error |
| `where T: Static` in scope, `if impls T: Static` | then | then | else-branch | then |
| `impl<'a> AnyLt for Tag<'a>`, `if impls Tag<'a>: AnyLt` | then | then | then | then |
| `impl<'b> Tr<'b> for Tag<'b>`, `if impls Tag<'a>: Tr<'a>` | then | then | else-branch | error |
| `if impls T: Bar`, no `may_spec` | error | error | per monomorphization | error |
| `always_applicable trait Foo`, `if impls T: Foo` | error (the gate) | error | error | per monomorphization |

### Strict versus commit-and-verify

#### Local regions

Signature lifetimes are universal; `exists<'x>` introduces a body-local
inference variable. Strict rejects answers that leave a new outlives
constraint or an equation for a body region. Commit-and-verify passes
those constraints to borrow checking.

For universal lifetimes, both modes require the same relationships to
follow from the signature. For local lifetimes, commit-and-verify can
infer a solution that strict rejects. With `impl Static for &'static u32`:

```rust
fn spec<'a>(x: &'a u32) {
    exists<'x> {
        let y: &'x u32 = x;                 // 'a: 'x
        if impls &'x u32: Static { .. }     // needs 'x: 'static
    }
}
```

| In scope | strict | commit and verify |
|---|---|---|
| nothing | error: leaves `'x: 'static` | error: implies `'a: 'static`, which the signature does not guarantee |
| `may_spec(&'a u32: Static)` | error: leaves `'x == 'a` | accepts: infer `'x == 'a`; use the caller's answer |
| `where 'a: 'static` | error: leaves `'x == 'static` | accepts: infer `'x == 'static` |

Bail on regions accepts all three examples and takes the else-branch;
always applicable rejects all three. Strict rejects even if other
statements already force `'x == 'a`: its filter does not consult the
body's accumulated constraints.

### Always applicable

The always-applicable criterion has two forms:

* At the use site: a bound is decided as holding only if it holds for
  every choice of its lifetimes: never a "yes" under region constraints.
  Neither `where 'a: 'static` nor a caller passing `'static` helps. A
  where-clause in scope that gives the bound does (`where T: Static`): its
  callers proved it, region constraints included.
  Each occurrence of a lifetime is a choice of its own, as after erasure:
  `Tag<'a>: Tr<'a>` is not decided given `impl<'b> Tr<'b> for Tag<'b>`,
  because that impl requires the two lifetimes to be equal.
* `always_applicable trait Foo`: every
  impl of `Foo` must apply regardless of lifetimes, so `T: Foo` can be
  decided after erasure exactly. It needs no `may_spec`; codegen decides
  per monomorphization. An impl is checked to have:
  * no `'static` in its header (`impl Foo for Tag<'static>`);
  * no parameter twice in its header (`impl<'a> Foo for Pair<'a, 'a>`,
    and `impl<T> Foo for Pair<T, T>`: type equality depends on lifetimes);
  * only where-clauses that are bounds on `always_applicable` traits, or
    mention no parameter.

## Out of scope

Impl specialization (`default fn`, `default type`, `default impl`). Impl
specialization of fns is branch specialization plus a hook trait, and
modeling it needs trait fn calls, which formality does not have yet.
