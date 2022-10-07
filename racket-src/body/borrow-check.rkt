#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../check/prove-goal.rkt"
         "borrow-check/initialization-check.rkt"
         "borrow-check/initialization-infer.rkt"
         "borrow-check/liveness.rkt"
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

(define-judgment-form
  formality-body
  #:mode (loan-check I I I)
  #:contract (loan-check Γ Env GoalAtLocations)

  [(where/error [(Location Goal) ...] GoalAtLocations)
   (where/error Goal_master (&& [Goal ...]))
   (prove-goal-in-env Env Goal_master TrueSolution)
   ----------------------------------------
   (loan-check Γ Env GoalAtLocations)
   ]
  )

(define-judgment-form
  formality-body
  #:mode (snoop I)
  #:contract (snoop Term)

  [----------------------------------------
   (snoop Term)
   ]
  )
