#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "borrow-check/initialization-check.rkt"
         "borrow-check/initialization-infer.rkt"
         "../check/prove-goal.rkt"
         )
(provide borrow-check
         )

(define-judgment-form
  formality-body
  #:mode (borrow-check I I I)
  #:contract (borrow-check Γ Env GoalAtLocations)

  [(where/error [(Location Goal) ...] GoalAtLocations)
   (where/error Goal_master (&& [Goal ...]))
   (prove-goal-in-env Env_in Goal_master Env_out)
   ; FIXME: Feed those constraints into the borrow checker rules.

   (initialization-check Γ (infer-initialization Γ))
   ; FIXME: ...the rest of the check
   ----------------------------------------
   (borrow-check Γ Env_in GoalAtLocations)
   ]
  )
