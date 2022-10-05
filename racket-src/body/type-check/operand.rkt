#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "place.rkt"
         "constant.rkt"
         )
(provide type-check-goal/Operand
         )

(define-judgment-form
  formality-body
  #:mode (type-check-goal/Operand I I O)
  #:contract (type-check-goal/Operand Γ Operand Goals)

  [; FIXME: Place must be Copy
   (type-check-goal/Place Γ Place Goals)
   ----------------------------------------
   (type-check-goal/Operand Γ (copy Place) Goals)
   ]

  [(type-check-goal/Place Γ Place Goals)
   ----------------------------------------
   (type-check-goal/Operand Γ (move Place) Goals)
   ]

  [(type-check-goal/Constant Γ Constant Goals)
   ----------------------------------------
   (type-check-goal/Operand Γ (const Constant) Goals)
   ]

  )