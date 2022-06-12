#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../inequalities.rkt"
         "../where-clauses-from-env.rkt"
         "../parameters.rkt"
         "../extrude.rkt"
         "../../logic/substitution.rkt"
         "../../logic/env.rkt"
         )
(provide universe-check-ok?
         )


(define-metafunction formality-ty
  ;; Checks whether `VarId` is in a universe that can see all the values of `Parameter`.
  ;;
  ;; Fails if:
  ;;
  ;; * `Parameter` mentions a placeholder that `VarId` cannot name because of its universe
  ;;
  ;; Returns a fresh environment which contains adjust universes for each
  ;; variable `V` that occur in `Parameter`. The universe of `V` cannot
  ;; be greater than the universe of `VarId` (since whatever value `V` ultimately
  ;; takes on will become part of `VarId`'s value).
  universe-check-ok? : Env VarId Parameter -> boolean

  [(universe-check-ok? Env VarId Parameter)
   (all? (universe-includes Universe_VarId (universe-of-var-in-env Env VarId_free)) ...)

   (where/error Universe_VarId (universe-of-var-in-env Env VarId))
   (where/error (VarId_free ...) (free-variables Env Parameter))
   ]

  )
