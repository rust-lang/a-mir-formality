---
sidebar_position: 3
---

# A closer look at `formality-decl`

Now that we've surveyed the type layer, let's look at the declaration layer.
It begins by defining an "extended" language `formality-decl`
that adds new stuff to `formality-ty`:

```scheme
(define-extended-language formality-decl formality-ty ...)
```
<span class="caption">[Source](https://github.com/nikomatsakis/a-mir-formality/blob/47eceea34b5f56a55d781acc73dca86c996b15c5/src/decl/grammar.rkt#L5)</span>

For example, a set of crates looks like this:

```scheme,ignore
{{#include ../../src/decl/grammar.rkt:Crates}}
```

Basically a crate `a` is represented as a list of items like `(a (crate (item1 item2 item3)))`
where `item{1,2,3}` are either structs/enums (`AdtDecl`), traits (`TraitDecl`), or impls (`TraitImplDecl`).

### Declaring traits in `FormalityDecl`

Let's look more closely at one of those kinds of items.
A trait declaration looks like this:

```scheme,ignore
{{#include ../../src/decl/grammar.rkt:Traits}}
```

Here:

* `TraitId` is the name of the trait
* `KindedVarIds` is a list of generic parameters like `((TyVar Self) (TyVar T))`.
  Note that the `Self` parameter is made explicit. 
* `WhereClauses` is a list of where-clauses, which are currently just `T: Foo` trait references
  (though potentially higher-ranked).
* `TraitItems` are the contents of the trait, currently not used for anything.
  <!-- What would go here? Methods? -->

So the following Rust trait

```rust
trait Foo<T: Ord>: Bar { }
```

would be represented as

```scheme
(Foo (trait ; KindedVarIds -- the generics:
            ((TyKind Self) (TyKind T))
            ; where clauses, including supertraits:
            ((Implemented (Ord (T))) (Implemented (Bar (Self))))
            ; trait items, empty list:
            ()
            ))
```

### Lowering crate items to clauses

The next part of `formality-decl` is the metafunction `env-with-crate-decls`:

```scheme
(define-metafunction formality-decl
  ;; Add the clauses/hypothesis from multiple crates
  ;; into the environment, where CrateId names the current crate.
  env-with-crate-decls : Env CrateDecls CrateId -> Env
  ; ...
  )
```
<span class="caption">[Source](https://github.com/nikomatsakis/a-mir-formality/blob/47eceea34b5f56a55d781acc73dca86c996b15c5/src/decl/decl-to-clause.rkt#L20-L23)</span>

This metafunction converts *Rust declarations* into the *environment* from the type layer.
Note that it takes:

* a base environment `Env` (typically just the constant `EmptyEnv`)
* a set of crates `CrateDecls` (this is meant to include the imports)
* the ID of the current crate `CrateId`.
  This is because the set of rules we generate for a particular item
  can be different depending on whether we are compiling the crate where it was declared or from some other crate
  (consider e.g. `#[non_exhaustive]`).

### Generating the clauses for a single crate item

The `env-with-crate-decls` function iterates over all the items in all the crates
and ultimately invokes this helper function, `crate-item-decl-rules`:

```scheme
(define-metafunction formality-decl
  ;; Given a crate item, return a tuple of:
  ;;
  ;; * The clauses that hold in all crates due to this item
  ;; * The invariants that hold in all crates due to this item
  ;; * The invariants that hold only in the crate that declared this item
  crate-item-decl-rules : CrateDecls CrateItemDecl -> (Clauses Invariants Invariants)
```
<span class="caption">[Source](https://github.com/nikomatsakis/a-mir-formality/blob/47eceea34b5f56a55d781acc73dca86c996b15c5/src/decl/decl-to-clause.rkt#L57-L63)</span>

`crate-item-decl-rules` takes 

* the full set of crates `CrateDecls` and
* the declaration of a single item `CrateItemDecl`

and it returns a 3-tuple.
As the comment says, this contains both clauses (rules that can be used to derive true facts)
along with two sets of invariants.
We'll cover the invariants later.

<!-- move/copy description of metafunctions to ty chapter -->
Metafunctions are basically a gigantic match statement.
They consist of a series of clauses,
each of which begins with a "pattern match" against the arguments. 

To see how it works, let's look at the case for an `impl` from that section.
To begin with, here is the comment explaining what we aim to do:

```scheme
 [;; For an trait impl declared in the crate C, like the following:
   ;;
   ;;     impl<'a, T> Foo<'a, T> for i32 where T: Ord { }
   ;;
   ;; We consider `HasImpl` to hold if (a) all inputs are well formed and (b) where
   ;; clauses are satisfied:
   ;;
   ;;     (∀ ((LtKind 'a) (TyKind T))
   ;;         (HasImpl (Foo (i32 'a u32))) :-
   ;;             (WellFormed (TyKind i32))
   ;;             (WellFormed (TyKind i32))
   ;;             (Implemented (Ord T)))
```

The actual code for this
begins by matching the item that we are generating rules for:

```scheme
(crate-item-decl-rules CrateDecls (impl KindedVarIds_impl TraitRef WhereClauses_impl ImplItems))
```
<span class="caption">[Source](https://github.com/nikomatsakis/a-mir-formality/blob/47eceea34b5f56a55d781acc73dca86c996b15c5/src/decl/decl-to-clause.rkt#L141-L166)</span>


The next line is the final result.
The function this is part of  -- this says we will produce one global clause
<!-- ??? -->

```scheme
((Clause) () ())
```

This final result is allowed to refer to variables defined on the following lines, shown below.
A `(where/error <pattern> <value>)` clause is effectively just `let <pattern> = <value>`, in Rust terms;
the `/error` part means "if this pattern doesn't match, generate an error".
You can also write `(where <pattern> <value>)`, but that means that if the pattern doesn't match, 
the metafunction should to see if the next rule works.

The impl code begins by pattern matching against the `TraitRef` that the impl is implementing
(in the example from the comment, that would be `(Foo (i32 'a T))`).
We extract out the `TraitId` and the self type `Parameter_trait`:

```scheme
   (where/error (TraitId (Parameter_trait ...)) TraitRef)
```

Next we look up the definition of the trait itself to find out its generics, using a helper function `item-with-id`.
This matches and extracts out the parameter kinds (lifetimes vs types, in this code):

```scheme
   (where/error (trait KindedVarIds_trait _ _) (item-with-id CrateDecls TraitId))
   (where/error ((ParameterKind_trait _) ...) KindedVarIds_trait)
```

Next we convert the where clauses (e,g, `T: Ord`) into goals, using a helper function `where-clauses->goals` (described below):

```scheme
   (where/error (Goal_wc ...) (where-clauses->goals WhereClauses_impl))
```

Finally we can generate that variable `Clause` that was referenced in the final result.
Note that we use the `...` notation to "flatten" together the list of goals from the where-clauses (`Goal_wc ...`)
and `WellFormed`-ness goals that we must prove
(i.e., to use the impl, we must show that its input types are well-formed):

```scheme
   (where/error Clause (∀ KindedVarIds_impl
                               (Implies
                                ((WellFormed (ParameterKind_trait Parameter_trait)) ...
                                 Goal_wc ...
                                 )
                                (HasImpl TraitRef))))
   ]
```

The `where-clause->goal` helper is fairly simple.
Here is the source, I'll leave puzzling it out as an exercise to the reader:

```scheme
(define-metafunction formality-decl
  ;; Convert a where clause `W` into a goal that proves `W` is true.
  where-clause->goal : WhereClause -> Goal

  ((where-clause->goal (Implemented TraitRef))
   (Implemented TraitRef)
   )

  ((where-clause->goal (∀ KindedVarIds WhereClause))
   (∀ KindedVarIds Goal)
   (where/error Goal (where-clause->goal WhereClause))
   )

  ; FIXME: Support lifetimes, projections
  )
```
<span class="caption">[Source](https://github.com/nikomatsakis/a-mir-formality/blob/47eceea34b5f56a55d781acc73dca86c996b15c5/src/decl/decl-to-clause.rkt#L197-L211)</span>

### Generating the "ok goals" for a crate

In addition to "clauses", there is also a function `crate-ok-goal` 
that generate goals for each crate item in a given crate:

```scheme
(define-metafunction formality-decl
  ;; Given a set of crates and the decl for the current crate,
  ;; generate the goal that proves all declarations in the current crate are
  ;; "ok". Other crates are assumed to be "ok".
  crate-ok-goal : CrateDecls CrateDecl -> Goal
```
<span class="caption">[Source](https://github.com/nikomatsakis/a-mir-formality/blob/47eceea34b5f56a55d781acc73dca86c996b15c5/src/decl/decl-ok.rkt#L7-L11)</span>

The idea is that the crate is "ok" (i.e., passes the type check) if these goals are satisfied.
For the declarations layer, these goals correspond roughly to rustc's `wfcheck`.
Here is the rule for impls:

```scheme
  [;; For a trait impl declared in the crate C, like the following:
   ;;
   ;;     impl<'a, T> Foo<'a, T> for u32 { }
   ;;
   ;; we require that the trait is implemented.
   (crate-item-ok-goal _ (impl KindedVarIds_impl TraitRef WhereClauses_impl ImplItems))
   (∀ KindedVarIds_impl
           (Implies ((WellFormed KindedVarId_impl) ... WhereClause_impl ...)
                    (All ((Implemented TraitRef)))))

   (where/error (KindedVarId_impl ...) KindedVarIds_impl)
   (where/error (WhereClause_impl ...) WhereClauses_impl)
   ]
  )
```
<span class="caption">[Source](https://github.com/nikomatsakis/a-mir-formality/blob/47eceea34b5f56a55d781acc73dca86c996b15c5/src/decl/decl-ok.rkt#L59-L71)</span>

In short, an `impl` is well-formed if the trait is fully implemented.
We'll look at the definition of *implemented* in more detail in [the next section](../what-formality-can-do/case-study),
but for now it suffices to say that a trait is implemented if
(a) it has an impl and 
(b) all of its where-clauses are satisfied.
Since we know there is an impl (we're checking it right now!) this is equivalent to saying 
"all of the trait's where clauses are satisfied".
