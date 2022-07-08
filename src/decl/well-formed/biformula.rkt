#lang racket
(require redex/reduction-semantics
         "../../logic/substitution.rkt"
         "../../logic/grammar.rkt"
         "../grammar.rkt"
         "parameter.rkt"
         )
(provide well-formed-goal-for-biformula
         )

(define-metafunction formality-decl
  ;; Given a `Biformula`, returns a goal that defines when this where-clause is well-formed.
  ;;
  ;; * `CrateDecls` -- the crate decls in scope
  ;; * `KindedVarIds` -- the variable ids in scope
  ;; * `Biformula` -- the biformula created from the user's where clause (doesn't accept arbitrary
  ;;   biformulas)
  well-formed-goal-for-biformula : CrateDecls KindedVarIds Biformula -> Goal

  [(well-formed-goal-for-biformula CrateDecls KindedVarIds (∀ KindedVarIds Biformula))
   (∀ KindedVarIds (well-formed-goal-for-biformula CrateDecls KindedVarIds Biformula))
   ]

  [; well-formedness for a goal like `T: a`
   ;
   ; FIXME -- the syntax of where clauses needs to be changed to give an
   ; explicit parameter kind for Ty, or else we need to thread in the variables
   ; in scope so that we can figure it out
   (well-formed-goal-for-biformula CrateDecls KindedVarIds (Parameter_a -outlives- Parameter_b))
   (&& [(well-formed-goal-for-parameter CrateDecls KindedVarIds Parameter_a)
        (well-formed-goal-for-parameter CrateDecls KindedVarIds Parameter_b)
        ])
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
   (well-formed-goal-for-biformula CrateDecls KindedVarIds (is-implemented (TraitId [Parameter ...])))
   (&& [(well-formed-goal-for-parameter CrateDecls KindedVarIds Parameter_value) ...
        Biformula_substituted ...
        ]
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

  [(well-formed-goal-for-biformula CrateDecls _ (normalizes-to AliasTy Ty))
   (&& [(well-formed-goal-for-ty CrateDecls AliasTy)
        (well-formed-goal-for-ty Ty)])
   ]

  [; well-formed goals are added during lowering to represent default  bounds
   ;
   ; no WF requirements per se
   (well-formed-goal-for-biformula CrateDecls _ (well-formed _))
   true-goal
   ]

  [; well-formed trait-ref goals are added during lowering to represent default bounds
   ;
   ; no WF requirements per se
   (well-formed-goal-for-biformula CrateDecls _ (well-formed-trait-ref _))
   true-goal
   ]

  [; in-scope goals are added during lowering to represent default  bounds
   ;
   ; no WF requirements per se
   (well-formed-goal-for-biformula CrateDecls _ (in-scope _))
   true-goal
   ]

  )
