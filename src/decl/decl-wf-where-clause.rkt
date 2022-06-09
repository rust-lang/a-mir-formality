#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../logic/substitution.rkt"
         "../ty/where-clauses.rkt"
         )
(provide well-formed-where-clause-goal
         )

(define-metafunction formality-decl
  ;; Given a `WhereClause`, returns a goal that defines when this where-clause is well-formed.
  well-formed-where-clause-goal : CrateDecls WhereClause -> Goal

  [(well-formed-where-clause-goal CrateDecls (∀ KindedVarIds WhereClause))
   (∀ KindedVarIds (well-formed-where-clause-goal WhereClause))
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
   (well-formed-where-clause-goal CrateDecls (Ty : TraitId (Parameter ...)))
   (&& ((well-formed (ParameterKind Parameter_value)) ...
        (where-clause->goal WhereClause_substituted) ...)
       )

   ; Find the trait declaration
   ;
   ; * in our example above `(ParameterKind VarId) ...` would match `((type Self) (type T))`
   ; * and `WhereClauses` would be `(T : Bar ())`
   (where/error (trait TraitId ((ParameterKind VarId) ...) where WhereClauses _) (trait-with-id CrateDecls TraitId))

   ; create a substitution ((Self => A) (T => B))
   (where/error (Parameter_value ...) (Ty Parameter ...))
   (where/error Substitution ((VarId Parameter_value) ...))
   (where/error (WhereClause_substituted ...) (apply-substitution Substitution WhereClauses))
   ]

  [(well-formed-where-clause-goal CrateDecls (AliasTy == Ty))
   ; fixme: we need to modify outlives to include a `ParameterKind`
   (&& ((well-formed (type AliasTy))
        (well-formed (type Ty))))
   ]




  )