#lang racket
(require redex/reduction-semantics
         "grammar-extended.rkt"
         "type-check-goal.rkt"
         "well-formed-mir.rkt"
         )
(provide unsafe-check
)

(define-judgment-form
  formality-mir-extended
  #:mode (unsafe-check I)
  #:contract (unsafe-check Γ)

  [; FIXME
      ----------------------------------------
   (unsafe-check Γ)
   ]
  )
