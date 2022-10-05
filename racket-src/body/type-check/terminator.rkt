#lang racket
(require redex/reduction-semantics
         "../../logic/grammar.rkt"
         "../grammar.rkt"
         "type-of.rkt"
         "fn-ty-signature.rkt"
         )
(provide type-check-goal/Terminator
         )

(define-judgment-form
  formality-body
  #:mode (type-check-goal/Terminator I I O)
  #:contract (type-check-goal/Terminator Γ Terminator Goals)

  [----------------------------------------
   (type-check-goal/Terminator Γ (goto BasicBlockId) [])
   ]

  [----------------------------------------
   (type-check-goal/Terminator Γ resume [])
   ]

  [----------------------------------------
   (type-check-goal/Terminator Γ abort [])
   ]

  [----------------------------------------
   (type-check-goal/Terminator Γ return [])
   ]

  [----------------------------------------
   (type-check-goal/Terminator Γ unreachable [])
   ]

  [(type-of/Operand Γ Operand_fn Ty_fn)
   (type-of/Operand Γ Operand_arg Ty_arg) ...
   (type-of/Place Γ Place_dest Ty_dest)
   (fn-ty-signature Γ Ty_fn (∀ KindedVarIds (implies (Biformula ...) ((Ty_formal ...) -> Ty_ret))))
   (where Goal (∃ KindedVarIds (&& ((Ty_arg <= Ty_formal) ...
                                    (Ty_ret <= Ty_dest)
                                    Biformula ...
                                    ))))
   ----------------------------------------
   (type-check-goal/Terminator Γ (call Operand_fn (Operand_arg ...) Place_dest TargetIds) [Goal])
   ]

  )