#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         )
(provide unsafe-check
         )

(define-judgment-form
  formality-check
  #:mode (unsafe-check I)
  #:contract (unsafe-check Γ)

  [; FIXME
   ----------------------------------------
   (unsafe-check Γ)
   ]
  )
