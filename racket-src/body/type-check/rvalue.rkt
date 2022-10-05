#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "type-of.rkt"
         "operand.rkt"
         "place.rkt"
         "fn-ty-signature.rkt"
         )
(provide type-check-goal/Rvalue
         )

;; Type-check an rvalue and produce a set of goals that must be proven for it to be well-formed.


(define-judgment-form
  formality-body
  #:mode (type-check-goal/Rvalue I I O)
  #:contract (type-check-goal/Rvalue Γ Rvalue Goals)

  [(type-of/Operands Γ Operands (Ty_op ...))
   (field-tys Γ AdtId Parameters VariantId ((_ Ty_field) ...))
   ----------------------------------------
   (type-check-goal/Rvalue Γ
                           ((adt AdtId VariantId Parameters) Operands)
                           [(Ty_op <= Ty_field) ...])
   ]

  [(type-check-goal/Operand Γ Operand Goals)
   ----------------------------------------
   (type-check-goal/Rvalue Γ (use Operand) Goals)
   ]

  [; FIXME: Operand must be Copy in some circumstances
   (type-check-goal/Operand Γ Operand Goals)
   ----------------------------------------
   (type-check-goal/Rvalue Γ (repeat Operand Constant) Goals)
   ]

  [(type-check-goal/Place Γ Place [Goal_0 ...])
   (type-of/Place Γ Place Ty_Place)
   (reborrow-goals Γ Place Lt [Goal_1 ...])
   ----------------------------------------
   (type-check-goal/Rvalue Γ (ref Lt MaybeMut Place) [Goal_0 ... Goal_1 ...])
   ]

  [(type-check-goal/Place Γ Place Goals)
   ----------------------------------------
   (type-check-goal/Rvalue Γ (addr-of MaybeMut Place) Goals)
   ]

  [(type-check-goal/Place Γ Place Goals)
   ----------------------------------------
   (type-check-goal/Rvalue Γ (len Place) Goals)
   ]

  [(type-check-goal/Operand Γ Operand_l [Goal_l ...])
   (type-check-goal/Operand Γ Operand_r [Goal_r ...])
   ----------------------------------------
   (type-check-goal/Rvalue Γ (BinaryOp Operand_l Operand_r) [Goal_l ... Goal_r ...])
   ]

  [; type-check ReifyFnPointer casts
   (type-of/Operand Γ Operand Ty_fn)
   (fn-ty-signature Γ Ty_fn (∀ KindedVarIds (implies Biformulas ((Ty_arg ...) -> Ty_ret))))
   (where/error number_args ,(length (term (Ty_arg ...))))
   (where/error Ty_fnptr (rigid-ty (fn-ptr "Rust" number_args) (Ty_arg ... Ty_ret)))
   (where/error Ty_reified (∀ KindedVarIds (implies Biformulas Ty_fnptr)))
   ----------------------------------------
   (type-check-goal/Rvalue Γ
                           (cast Operand as Ty_target)
                           [(Ty_reified <= Ty_target)])
   ]

  [(type-check-goal/Operand Γ Operand Goals)
   ----------------------------------------
   (type-check-goal/Rvalue Γ (cast Operand as Ty) Goals)
   ]
  )


(define-judgment-form
  formality-body
  #:mode (reborrow-goals I I I O)
  #:contract (reborrow-goals Γ Place Lt Goals)

  [----------------------------------------
   (reborrow-goals Γ LocalId Lt [])
   ]

  [(type-of/Place Γ Place (rigid-ty (ref ()) [Lt_r _]))
   ----------------------------------------
   (reborrow-goals Γ (* Place) Lt [(Lt_r -outlives- Lt)])
   ]

  [(type-of/Place Γ Place (rigid-ty (ref mut) [Lt_r _]))
   (reborrow-goals Γ Place Lt [Goal ...])
   ----------------------------------------
   (reborrow-goals Γ (* Place) Lt [Goal ... (Lt_r -outlives- Lt)])
   ]

  ; FIXME: Raw pointer derefs, box derefs

  [(reborrow-goals Γ Place Lt Goals)
   ----------------------------------------
   (reborrow-goals Γ (field Place _) Lt Goals)
   ]

  [(reborrow-goals Γ Place Lt Goals)
   ----------------------------------------
   (reborrow-goals Γ (index Place _) Lt Goals)
   ]

  [(reborrow-goals Γ Place Lt Goals)
   ----------------------------------------
   (reborrow-goals Γ (downcast Place _) Lt Goals)
   ]
  )