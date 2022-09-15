#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../inequalities.rkt"
         "../parameters.rkt"
         "../extrude.rkt"
         "../../logic/substitution.rkt"
         "../../logic/env.rkt"
         "../../logic/env-inequalities.rkt"
         )
(provide occurs-check-ok?
         )

(define-metafunction formality-ty
  ;; Checks whether `VarId` appears free in `Parameter`.
  occurs-check-ok? : Env VarId Parameter -> boolean

  [(occurs-check-ok? Env VarId Parameter)
   ; can't have X = Vec<X> or whatever, that would be infinite in size
   (not? (in?/id VarId (free-variables Env Parameter)))]

  )
