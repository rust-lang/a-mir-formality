#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "borrow-check/initialization-check.rkt"
         "borrow-check/initialization-infer.rkt"
         )
(provide borrow-check
         )

(define-judgment-form
  formality-body
  #:mode (borrow-check I I)
  #:contract (borrow-check Γ GoalAtLocations)

  [(initialization-check Γ (infer-initialization Γ))
   ; FIXME: ...the rest of the check
   ----------------------------------------
   (borrow-check Γ GoalAtLocations)
   ]
  )
