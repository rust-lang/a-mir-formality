#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../logic/hook.rkt"
         "../logic/env.rkt"
         )
(provide formality-ty-hook
         env-adt-generics
         ty:categorize-goal
         )

;; Extends the core logic hook with the ability to query variance:
;;
;; adt-generics : AdtId -> Generics
;; fn-generics : FnId -> Generics
(struct formality-ty-hook formality-logic-hook (adt-generics
                                                fn-generics
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
  ;; Returns the variance for each of the parameters to an function
  env-fn-generics : Env FnId -> Generics

  [(env-fn-generics Env FnId)
   ,((formality-ty-hook-fn-generics (term any)) (term FnId))
   (where/error (Hook: any) (env-hook Env))
   ]
  )

(define-metafunction formality-ty
  ty:categorize-goal : Env Goal -> Goal/Categorization

  [(ty:categorize-goal Env BuiltinGoal) builtin-goal]

  [(ty:categorize-goal Env Relation) builtin-relation]

  ; Normalization is inductive: this prevents a cycle between impls
  ; that would otherwise permit an associated type to be equal to anything.
  ;
  ; See #68.
  [(ty:categorize-goal Env (normalizes-to AliasTy Ty)) (user-predicate -)]

  [(ty:categorize-goal Env Predicate) (user-predicate +)]
  )
