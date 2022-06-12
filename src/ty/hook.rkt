#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../logic/hook.rkt"
         "../logic/env.rkt"
         )
(provide formality-ty-hook
         env-adt-generics
         ty:is-predicate?
         ty:is-relation?
         where-clause->goal∧clause
         )

;; Extends the core logic hook with the ability to query variance:
;;
;; adt-generics : AdtId -> Generics
(struct formality-ty-hook formality-logic-hook (adt-generics))

(define-metafunction formality-ty
  ;; Returns the variance for each of the parameters to an Adt
  env-adt-generics : Env AdtId -> Generics

  [(env-adt-generics Env AdtId)
   ,((formality-ty-hook-adt-generics (term any)) (term AdtId))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(define-metafunction formality-ty
  ty:is-predicate? : Goal -> boolean

  [(ty:is-predicate? Predicate) #t]
  [(ty:is-predicate? _) #f]
  )

(define-metafunction formality-ty
  ty:is-relation? : Goal -> boolean

  [(ty:is-relation? Relation) #t]
  [(ty:is-relation? _) #f]
  )

(define-metafunction formality-ty
  ;; Helper function: Converts a where-clause into a `Goal∧Clause`; used to
  ;; define the functions in `where-clauses.rkt`, which are the ones you should
  ;; actually invoke.
  where-clause->goal∧clause : WhereClause -> Goal∧Clause

  [(where-clause->goal∧clause (∀ KindedVarIds WhereClause))
   (∀ KindedVarIds (where-clause->goal∧clause WhereClause))
   ]

  [(where-clause->goal∧clause (Ty_self : TraitId (Parameter ...)))
   (is-implemented (TraitId (Ty_self Parameter ...)))
   ]

  [(where-clause->goal∧clause (AliasTy == Ty))
   (normalizes-to AliasTy Ty)
   ]

  [(where-clause->goal∧clause (Parameter_a : Parameter_b))
   (Parameter_a -outlives- Parameter_b)
   ]

  )


(define-metafunction formality-ty
  where-clauses->goals∧clauses : WhereClauses -> Goals∧Clauses

  [(where-clauses->goals∧clauses (WhereClause ...))
   ((where-clause->goal∧clause WhereClause) ...)
   ]

  )