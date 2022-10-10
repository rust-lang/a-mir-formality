#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../check/prove-goal.rkt"
         "borrow-check/initialization-check.rkt"
         "borrow-check/initialization-infer.rkt"
         "borrow-check/loan-check.rkt"
         )
(provide borrow-check
         )

(define-judgment-form
  formality-body
  #:mode (borrow-check I I I)
  #:contract (borrow-check Γ Env GoalAtLocations)

  [(initialization-check Γ (infer-initialization Γ))
   (loan-check Γ Env GoalAtLocations)
   ----------------------------------------
   (borrow-check Γ Env GoalAtLocations)
   ]
  )
