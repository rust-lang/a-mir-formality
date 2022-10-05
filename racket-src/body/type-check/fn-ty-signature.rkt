#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "type-of.rkt"
         "operand.rkt"
         "place.rkt"
         )
(provide fn-ty-signature
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