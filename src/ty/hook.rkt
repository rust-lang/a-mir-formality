#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../logic/hook.rkt"
         "../logic/env.rkt"
         )
(provide formality-ty-hook
         env-adt-generics
         ty:is-predicate-goal?
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
  ty:is-predicate-goal? : Goal -> boolean

  [(ty:is-predicate-goal? Predicate) #t]
  [(ty:is-predicate-goal? _) #f]
  )
