#lang racket
(require redex/reduction-semantics
         "grammar-extended.rkt"
         )
(provide borrow-check
         )

(define-judgment-form
  formality-mir-extended
  #:mode (borrow-check I I)
  #:contract (borrow-check Γ GoalAtLocations)

  [; FIXME: Prove that GoalAtLocations are valid, yielding a set of constraints.
   ;
   ; FIXME: Feed those constraints into the borrow checker rules.
   ----------------------------------------
   (borrow-check Γ GoalAtLocations)
   ]
  )
