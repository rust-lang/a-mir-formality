#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         )
(provide type-check-goal/Constant
         )

;; Type check a constant and produce a set of goals that must be proven for it to be well-typed.

(define-judgment-form
  formality-body
  #:mode (type-check-goal/Constant I I O)
  #:contract (type-check-goal/Constant Γ Constant Goals)

  [; FIXME: type check constants
   ----------------------------------------
   (type-check-goal/Constant Γ Constant [])
   ]

  )

