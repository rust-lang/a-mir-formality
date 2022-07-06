#lang racket
(require redex/reduction-semantics
         "grammar-extended.rkt"
         "prove-goal.rkt"
         )
(provide borrow-check
         )

(define-judgment-form
  formality-mir-extended
  #:mode (borrow-check I I I)
  #:contract (borrow-check Γ Env GoalAtLocations)

  [(where/error [(Location Goal) ...] GoalAtLocations)
   (where/error Goal_master (&& [Goal ...]))
   (prove-goal-in-env Env_in Goal_master Env_out)

   ; FIXME: Feed those constraints into the borrow checker rules.
   ----------------------------------------
   (borrow-check Γ Env_in GoalAtLocations)
   ]
  )
