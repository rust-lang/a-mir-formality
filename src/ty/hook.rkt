#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../logic/hook.rkt"
         "../logic/env.rkt"
         )
(provide formality-ty-hook
         env-adt-generics
         ty:categorize-goal
         where-clause->biformula-from-env
         )

;; Extends the core logic hook with the ability to query variance:
;;
;; adt-generics : AdtId -> Generics
(struct formality-ty-hook formality-logic-hook (adt-generics
                                                where-clause->biformula-from-env
                                                ))

(define-metafunction formality-ty
  ;; Returns the variance for each of the parameters to an Adt
  env-adt-generics : Env AdtId -> Generics

  [(env-adt-generics Env AdtId)
   ,((formality-ty-hook-adt-generics (term any)) (term AdtId))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(define-metafunction formality-ty
  ty:categorize-goal : Goal -> Goal/Categorization

  [(ty:categorize-goal BuiltinGoal) builtin-goal]

  [(ty:categorize-goal Relation) builtin-relation]

  ; Normalization is inductive: this prevents a cycle between impls
  ; that would otherwise permit an associated type to be equal to anything.
  ;
  ; See #68.
  [(ty:categorize-goal (normalizes-to AliasTy Ty)) (user-predicate -)]

  [(ty:categorize-goal Predicate) (user-predicate +)]
  )

(define-metafunction formality-ty
  ;; Helper function: Converts a where-clause into a `Biformula`; used to
  ;; define the functions in `where-clauses.rkt`, which are the ones you should
  ;; actually invoke.
  where-clause->biformula-from-env : Env WhereClause -> Biformula

  [(where-clause->biformula-from-env Env WhereClause)
   ,((formality-ty-hook-where-clause->biformula-from-env (term any)) (term WhereClause))
   (where/error (Hook: any) (env-hook Env))
   ]
  )
