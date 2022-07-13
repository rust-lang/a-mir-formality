#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         )
(provide borrow-check
         )

(define-judgment-form
  formality-body
  #:mode (borrow-check I I)
  #:contract (borrow-check Γ GoalAtLocations)

  [; FIXME: Prove that GoalAtLocations are valid, yielding a set of constraints.
   ;
   ; FIXME: Feed those constraints into the borrow checker rules.
   ----------------------------------------
   (borrow-check Γ GoalAtLocations)
   ]
  )
