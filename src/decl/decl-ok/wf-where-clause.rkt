#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../where-clauses.rkt"
         "../../logic/substitution.rkt"
         "../../ty/user-ty.rkt"
         )
(provide well-formed-where-clause-goal
         )

(define-metafunction formality-decl
  ;; Given a `WhereClause`, returns a goal that defines when this where-clause is well-formed.
  well-formed-where-clause-goal : CrateDecls WhereClause -> Goal

  [(well-formed-where-clause-goal CrateDecls (∀ KindedVarIds WhereClause))
   (∀ KindedVarIds (well-formed-where-clause-goal WhereClause))
   ]

  [; well-formedness for a goal like `T: a`
   (well-formed-where-clause-goal CrateDecls (UserTy : Lt))
   (&& [(well-formed (type (user-ty UserTy)))
        (well-formed (lifetime Lt))])
   ]

  [; well-formedness for a goal like `a: b`
   (well-formed-where-clause-goal CrateDecls (Lt_a >= Lt_b))
   (&& [(well-formed (lifetime Lt_a))
        (well-formed (lifetime Lt_b))])
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
   (well-formed-where-clause-goal CrateDecls (UserTy : TraitId (UserParameter ...)))
   (&& ((well-formed (ParameterKind Parameter_value)) ...
        (where-clause->goal CrateDecls WhereClause_substituted) ...)
       )

   ; Find the trait declaration
   ;
   ; * in our example above `(ParameterKind VarId) ...` would match `((type Self) (type T))`
   ; * and `WhereClauses` would be `(T : Bar ())`
   (where/error (trait TraitId ((ParameterKind VarId) ...) where WhereClauses _) (trait-with-id CrateDecls TraitId))

   ; create a substitution ((Self => A) (T => B))
   (where/error (UserParameter_value ...) (UserTy UserParameter ...))
   (where/error Substitution ((VarId UserParameter_value) ...))
   (where/error (WhereClause_substituted ...) (apply-substitution Substitution WhereClauses))
   ]

  [(well-formed-where-clause-goal CrateDecls (UserAliasTy == UserTy))
   (&& ((well-formed (type (user-ty UserAliasTy)))
        (well-formed (type (user-ty UserTy)))))
   ]




  )