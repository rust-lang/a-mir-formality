#lang racket
(require redex/reduction-semantics
         "../../logic/substitution.rkt"
         "../../logic/grammar.rkt"
         "../grammar.rkt"
         )
(provide well-formed-goal-for-where-clause
         )

(define-metafunction formality-decl
  ;; Given a `Biformula`, returns a goal that defines when this where-clause is well-formed.
  ;;
  ;; * `CrateDecls` -- the crate decls in scope
  ;; * `KindedVarIds` -- the variable ids in scope
  ;; * `Biformula` -- the biformula created from the user's where clause (doesn't accept arbitrary
  ;;   biformulas)
  well-formed-goal-for-where-clause : CrateDecls KindedVarIds Biformula -> Goal

  [(well-formed-goal-for-where-clause CrateDecls KindedVarIds (∀ KindedVarIds Biformula))
   (∀ KindedVarIds (well-formed-goal-for-where-clause CrateDecls KindedVarIds Biformula))
   ]

  [; well-formedness for a goal like `T: a`
   ;
   ; FIXME -- the syntax of where clauses needs to be changed to give an
   ; explicit parameter kind for Ty, or else we need to thread in the variables
   ; in scope so that we can figure it out
   (well-formed-goal-for-where-clause CrateDecls KindedVarIds (Parameter_a -outlives- Parameter_b))
   (&& [(well-formed (kinded-parameter KindedVarIds Parameter_a))
        (well-formed (kinded-parameter KindedVarIds Parameter_b))])
   ]

  [; well-formedness for a goal like `A: Foo<B>` where
   ;
   ; ```
   ; trait Foo<T> where T: Bar { }
   ; ```
   ;
   ; would be that all of these conditions must be met:
   ;
   ; * well-formed (type A) -- the input types must be well-formed
   ; * well-formed (type B)
   ; * is-implemented (B: Bar) -- the where-clauses on the trait must be satisfied
   ;
   ; Tricky example. If you have `X: Foo` where `trait Foo where Self: Foo` you would get
   ; `((well-formed (type X)) (is-implemented (Foo (X))))`.
   (well-formed-goal-for-where-clause CrateDecls _ (is-implemented (TraitId [Parameter ...])))
   (&& ((well-formed (ParameterKind Parameter_value)) ...
        Biformula_substituted ...)
       )

   ; Find the trait declaration
   ;
   ; * in our example above `(ParameterKind VarId) ...` would match `((type Self) (type T))`
   ; * and `Biformulas` would be `(T : Bar[])`
   (where/error (trait TraitId ((ParameterKind VarId) ...) where Biformulas _) (trait-with-id CrateDecls TraitId))

   ; create a substitution ((Self => A) (T => B))
   (where/error (Parameter_value ...) (Parameter ...))
   (where/error Substitution ((VarId Parameter_value) ...))
   (where/error [Biformula_substituted ...] (apply-substitution Substitution Biformulas))
   ]

  [(well-formed-goal-for-where-clause CrateDecls _ (normalizes-to AliasTy Ty))
   ; fixme: we need to modify outlives to include a `ParameterKind`
   (&& ((well-formed (type AliasTy))
        (well-formed (type Ty))))
   ]

  )

(define-metafunction formality-decl
  ;; Figure out whether something is a type or a lifetime or what.
  ;;
  ;; For variables, this is ambiguous and we have to check what is in scope.
  kinded-parameter : KindedVarIds Parameter -> KindedParameter

  [(kinded-parameter [_ ... (ParameterKind VarId) _ ...] VarId)
   (ParameterKind VarId)
   ]

  [(kinded-parameter _ Ty)
   (type Ty)
   ]

  [(kinded-parameter _ Lt)
   (lifetime Lt)
   ]
  )
