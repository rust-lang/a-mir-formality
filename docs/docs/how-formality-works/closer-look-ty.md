---
sidebar_position: 2
---
# A closer look at `formality-ty`

Let's take a closer look at the `formality-ty` layer.

### Defining Rust types

The current definition of types looks like this:

```scheme
(define-language formality-ty
  ...
  (Ty :=
      (TyApply TyName Parameters) ; Application type
      VarId                       ; Bound or existential (inference) variable
      (! VarId)                   ; Universal (placeholder) variable
      )
  (TyName :=
          AdtId           ; enum/struct/union
          TraitId         ; trait
          AssociatedTy    ; Associated type
          ScalarId        ; Something like i32, u32, etc
          (Ref MaybeMut)  ; `&mut` or `&`, expects a lifetime + type parameter
          (Tuple number)  ; tuple of given arity
          )
   ...
   (ScalarId := i8 u8 i16 u16 i32 u32 i64 u64 i128 u128 bool)
   ...
   (AssociatedTy := (TraitId AssociatedTyId))
   ...
   (Parameters := (Parameter ...))
   (Parameter := Ty Lt)
   ...
   ((AdtId VarId TraitId AssociatedTyId AnyId) := variable-not-otherwise-mentioned)
)
```
<span class="caption">[Source](https://github.com/nikomatsakis/a-mir-formality/blob/47eceea34b5f56a55d781acc73dca86c996b15c5/src/ty/grammar.rkt#L25-L37)</span>

This definition is far from complete,
but it should give you some idea for the level of abstraction we are shooting for
and also how PLT Redex works.
The idea here is that a type is either a variable,
a placeholder that represents a generic type,
or an "application" of a type name to some parameters.
Let's see some examples:

* A generic type like `T` could either be `T` or `(! T)`:
    * `T` is used when the generic has yet to be substituted, e.g., as part of a declaration.
    * `(! T)` is used as a placeholder to "any type `T`".
* A type like `Vec<T>` in Rust would be represented therefore as:
    * `(TyApply Vec (T))`, in a declaration; or,
    * `(TyApply Vec ((! T)))` when checking it.
* A scalar type like `i32` is represented as `(TyApply i32 ())`.

As I said, this defintion of types is woefully incomplete. I expect it to eventually include:

* "alias types" like associated types and type aliases
* "existential" types like `dyn`
* "forall" quantifies to cover `for<'a> ...`
* "function" types `fn(A1...An) -> R`
* "implication" types `where(...) T` (these don't exist in Rust—yet)

You can also see that the definition of types is aligned to highlight their "essential" characteristics
and not necessarily for convenience elsewhere.
Almost every Rust type, for example, boils down to *some* kind of "application"
(it's likely that we can even represent `fn` types this way).

### Type unification

A key part of the type layer is that it includes *type unification*.
That is, it defines the rules for making types equal.
This will eventually have to be extended to cover subtyping (more on that a bit later)
so that we can properly handle variance.

Unification is accomplished in PLT Redex with a "[metafunction][]",
which just means a function that operates on terms
(versus a function in the Rust program being analyzed):

```scheme
(define-metafunction formality-ty
  most-general-unifier : Env TermPairs -> EnvSubstitution or Error
```

This function takes an environment `Env` and a list of pairs of terms that should be equated and gives back either:

* a new environment and substitution from inference variables to values that will make the two terms syntactically equal; or,
* the term `Error`, if they cannot be unified.

The unifier is a bit smarter than the traditional unification
in that it knows about *universes* and so can handle "forall" proofs and things
(that's what is found in the environment).
This is the same as chalk and rustc.

I won't cover the details but I'll just give an example.
This is actually modified from a unit test from the code
([source](https://github.com/nikomatsakis/a-mir-formality/blob/47eceea34b5f56a55d781acc73dca86c996b15c5/src/ty/unify.rkt#L254-L269)).
Invoking `most-general-unifier` like so:

```scheme
(most-general-unifier Env_2 ((A X)
                             (X (TyApply Vec (Y)))
                             (Y (TyApply i32 ()))))
```

<!-- maybe: `A == X`,  `X == Vec<Y>`, and `Y == i32` -->
corresponds to saying that `[A = X, X = Vec<Y>, Y = i32]` must all be true,
where `A`, `X` and `Y` are inference variables.
The resulting output is a substitution that maps `A`, `X`, and `Y` to the following values:

* `A -> Vec<i32>`
* `X -> Vec<i32>`
* `Y -> i32`

### Predicates

`formality-ty` also defines the core predicates used to define Rust semantics.
The current definition is as follows:

```scheme,ignore
{{#include ../../src/ty/grammar.rkt:Predicates}}
```

These core predicates are then used to define a richer vocabulary of goals (things that can be proven)
and various kinds of "clauses" (things that are assumed to be true, axioms):

```scheme,ignore
{{#include ../../src/logic/grammar.rkt:GoalsAndHypotheses}}
```

Importantly, the *types layer* defines a solver that gives semantics to all the "meta" parts of goals and clauses -- e.g., it defines what it means to prove `(All (G1 G2))` (prove both `G1` and `G2`, duh). But it doesn't have any rules for what it means to prove the *core* predicates true -- so it could never prove `(is-implemented (Debug ((! T))))`. Those rules all come from the declaration layer and are given to the types layer as part of the "environment".

#### Goals versus clauses

You might be curious about the distinction between goal and clause
and why there are so many names for clauses (hypothesis, clause, invariant, etc).
Let's talk briefly about that.

The role of `∀` in goals and clauses is different.
Proving \\( \forall X. G \\) requires proving that `G` is true for any value of `X`
(i.e., for a placeholder `(! X)`, in our setup).
By contrast, if you know \\( \forall X. G \\) as an axiom,
it means that you can give `X` any value `X1` you want.
Clauses have a limited structure between that keeps the solver tractable.
The idea is that they are always "ways to prove a single predicate" true;
we don't allow a clause like `(Any (A B))` as a clause,
since that would mean "A or B is true but you don't know which".
That would then be a second way to prove an `Any` goal like `(Any ...)`
and introduce lots of complications (we got enough already, thanks).

Further distinctions between "hypotheses", "clauses", and "invariants"
are used to express and capture implied bounds.
We'll defer a detailed analysis until the section below, but briefly:
* "Hypotheses" are where-clauses that are assumed to be true in this section of the code.
* "Clauses" are global rules that are always true (derived, e.g., from an impl).
* "Invariants" express implied bounds (e.g., supertrait relationships like "if `T: Eq`, then `T: PartialEq`").

### Solver

Putting all this together, the types layer currently includes a relatively simple solver called `cosld-solve`.
The name refers to the classic [SLD Resolution Algorithm][] that powers Prolog,
although the version of it that we've implemented is extended in two ways:

* It is coinductive (hence `cosld`) as [described by Luke Simon et al.](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.102.9618&rep=rep1&type=pdf).
  This means it permits cycles, roughly speaking.
* It covers hereditary Harrop predicates using the [techniques described by Gopalan Nadathur](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.107.2510&rep=rep1&type=pdf).

In terms of chalk solvers, it is "similar" to slg, but much simpler in its structure (it doesn't do any caching).

All those fancy words aside, it's really quite simple.
It's defined via induction rules, which PLT Redex lets us write in a natural style with [`define-judgment-form`](https://docs.racket-lang.org/redex/reference.html#%28form._%28%28lib._redex%2Freduction-semantics..rkt%29._define-judgment-form%29%29).
The definition begins like so:

```scheme
(define-judgment-form formality-ty
  #:mode (prove I I I O)
  #:contract (prove Env Predicates_stack Goal EnvSubstitution)
```

This says that we are trying to prove something written as `(prove Env Predciates Goal EnvSubstitution)`,
where the first three are 'inputs' and the final name is an 'output'
(the input vs output distinction is often left implicit in Prolog and other languages).
The idea is that we will prove that `Goal` is true in some environment `Env`;
the environment contains our hypotheses and clauses, as well as information about the variables in scope.
The `Predicates` list is the stack of things we are solving, it's used to detect cycles.
The `EnvSubstitution` is the *output*, it is a modified environment
paired with a substitution that potentially gives new values to inference variables found in `Goal`.

Here is a simple rule.
It defines the way we prove `Any` ([source](https://github.com/nikomatsakis/a-mir-formality/blob/main/src/ty/cosld-solve/prove.rkt#L62-L65)).
The notation is as follows.
The stuff "above the line" are the conditions that have to be proven;
the thing "under the line" is the conclusion that we can draw.

```scheme
  [(prove Env Predicates_stack Goal_1 EnvSubstitution_out)
   ------------------------------------------------------- "prove-any"
   (prove Env Predicates_stack (Any (Goal_0 ... Goal_1 Goal_2 ...)) EnvSubstitution_out)
   ]
```

This rule says:

* Given some goal `(Any (Goal_0 ... Goal_1 Goal_2 ...))` where `Goal_1` is found somewhere in that list...
    * if we can prove `Goal_1` to be true, then the `Any` goal is true.

Or read another way:

* If we can prove `Goal_1` to be true, then we can prove `(Any Goals)` to be true so long as `Goal_1` is somewhere in `Goals`.

It shows you a bit of the power of PLT Redex (and Racket's pattern matching), as well. We are able to write the rule in a "non-deterministic" way -- saying, "pick any goal from the list" and prove it. Redex will search all the possibilities.

[metafunction]: https://docs.racket-lang.org/redex/reference.html#%28form._%28%28lib._redex%2Freduction-semantics..rkt%29._define-metafunction%29%29
[SLD Resolution Algorithm]: https://en.wikipedia.org/wiki/SLD_resolution
