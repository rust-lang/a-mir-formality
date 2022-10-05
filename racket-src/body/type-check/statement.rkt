#lang racket
(require redex/reduction-semantics
         "../../logic/grammar.rkt"
         "../grammar.rkt"
         "type-of.rkt"
         "rvalue.rkt"
         "place.rkt"
         )
(provide type-check-goal/Statement
         )

(define-judgment-form
  formality-body
  #:mode (type-check-goal/Statement I I O)
  #:contract (type-check-goal/Statement Γ Statement Goals)

  [(type-of/Place Γ Place Ty_place)
   (type-of/Rvalue Γ Rvalue Ty_rvalue)
   #;(mutability/Place Γ Place mut)
   (type-check-goal/Rvalue Γ Rvalue [Goal_rvalue ...])
   ----------------------------------------
   (type-check-goal/Statement Γ
                              (Place = Rvalue)
                              [Goal_rvalue ... (Ty_rvalue <= Ty_place)])
   ]

  [----------------------------------------
   (type-check-goal/Statement Γ (storage-live LocalId) [])
   ]

  [----------------------------------------
   (type-check-goal/Statement Γ (storage-dead LocalId) [])
   ]

  [----------------------------------------
   (type-check-goal/Statement Γ noop [])
   ]

  [(type-check-goal/Place Γ Place Goals)
   ----------------------------------------
   (type-check-goal/Statement Γ (fake-read Place) Goals)
   ]

  )