#lang racket
(require redex/reduction-semantics
         "../../logic/grammar.rkt"
         "../../logic/env.rkt"
         "../grammar.rkt"
         "../locations.rkt"
         )
(provide type-check-goal/Place
         )

(define-judgment-form
  formality-body
  #:mode (type-check-goal/Place I I O)
  #:contract (type-check-goal/Place Γ Place Goals)

  [; FIXME: type check places
   ----------------------------------------
   (type-check-goal/Place Γ Place [])
   ]

  )


