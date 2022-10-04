#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "type-of.rkt"
         "operand.rkt"
         "place.rkt"
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

  [; FIXME:
   (type-check-goal/Place Γ Place Goals)
   ----------------------------------------
   (type-check-goal/Rvalue Γ (ref Lt MaybeMut Place) Goals)
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
  #:mode (fn-ty-signature I I O)
  #:contract (fn-ty-signature Γ Ty Signature)

  [(fn-ty-signature Γ Ty (∀ [KindedVarId_2 ...] (implies Biformulas (Tys_args -> Ty_ret))))
   ----------------------------------------
   (fn-ty-signature Γ
                    (∀ [KindedVarId_1 ...] Ty)
                    (∀ [KindedVarId_1 ... KindedVarId_2 ...] (implies Biformulas (Tys_args -> Ty_ret))))
   ]

  [(fn-ty-signature Γ Ty (∀ () (implies [Biformula_2 ...] (Tys_args -> Ty_ret))))
   ----------------------------------------
   (fn-ty-signature Γ
                    (implies [Biformula_1 ...] Ty)
                    (∀ () (implies [Biformula_1 ... Biformula_2 ...] (Tys_args -> Ty_ret))))
   ]

  [----------------------------------------
   (fn-ty-signature Γ
                    (rigid-ty (fn-ptr _ _) (Ty_arg ... Ty_ret))
                    (∀ () (implies () ((Ty_arg ...) -> Ty_ret))))
   ]

  [(where/error (fn _ KindedVarIds_fn (Ty_arg ...) -> Ty_ret _ _ _) (find-fn Γ FnId))
   (where/error Substitution (create-substitution KindedVarIds_fn Parameters))
   (where/error (Ty_argsubst ...) ((apply-substitution Substitution Ty_arg) ...))
   (where/error Ty_retsubst (apply-substitution Substitution Ty_ret))
   ----------------------------------------
   (fn-ty-signature Γ
                    (rigid-ty (fn-def FnId) Parameters)
                    (∀ () (implies () ((Ty_argsubst ...) -> Ty_retsubst))))
   ]

  )