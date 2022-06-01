#lang racket
(require redex
         "grammar-extended.rkt"
         "../logic/substitution.rkt"
         )
(provide type-of/Place
         type-of/Rvalue
         type-of/Operands
         type-of/Operand
         )

;; type-of judgments:
;;
;; * Output the type of a place, rvalue, etc

(define-judgment-form
  formality-mir-extended
  #:mode (type-of/Place I I O)
  #:contract (type-of/Place Γ Place Ty)

  [(place-type-of Γ Place Ty ())
   ----------------------------------------
   (type-of/Place Γ Place Ty)
   ]

  )

(define-judgment-form
  formality-mir-extended
  #:mode (place-type-of I I O O)
  #:contract (place-type-of Γ Place Ty MaybeVariantId)

  [(type-of/LocalId Γ LocalId Ty)
   ----------------------------------
   (place-type-of Γ LocalId Ty ())
   ]

  [(place-type-of Γ Place (rigid-ty (ref _) (_ Ty)) ())
   ----------------------------------
   (place-type-of Γ (* Place) Ty ())
   ]

  [; field of a struct
   ;
   ; extract the name of the singular variant
   (place-type-of Γ Place (rigid-ty AdtId Parameters) ())
   (where (struct _ _ ((VariantId _))) (decl-of-adt Γ AdtId))
   (field-ty Γ AdtId Parameters VariantId FieldId Ty_field)
   ----------------------------------
   (place-type-of Γ (field Place FieldId) Ty_field ())
   ]

  [; field of an enum
   ;
   ; must have been downcast to a particular variant
   (place-type-of Γ Place (rigid-ty AdtId Parameters) (VariantId))
   (where (enum _ _ _) (decl-of-adt Γ AdtId))
   (field-ty Γ AdtId Parameters VariantId FieldId Ty_field)
   ----------------------------------
   (place-type-of Γ (field Place FieldId) Ty_field ())

   ]

  [; downcast to an enum variant
   (place-type-of Γ Place (rigid-ty AdtId Parameters) ())
   (where (enum _ _ (_ ... (VariantId _) _ ...)) (decl-of-adt Γ AdtId))
   ----------------------------------
   (place-type-of Γ (downcast Place VariantId) (rigid-ty AdtId Parameters) (VariantId))
   ]

  ; FIXME: indexing, unions
  )

(define-judgment-form
  formality-mir-extended
  #:mode (type-of/LocalId I I O)
  #:contract (type-of/LocalId Γ LocalId Ty)

  [(where/error LocalDecls (local-decls-of-Γ Γ))
   (where (_ ... (LocalId Ty _) _ ...) LocalDecls)
   ----------------------------------------
   (type-of/LocalId Γ LocalId Ty)
   ]

  )

(define-judgment-form
  formality-mir-extended

  ;; Get the type of a field from a given variant of a given ADT,
  ;; substituting type parameters.

  #:mode (field-ty I I I I I O)
  #:contract (field-ty Γ AdtId Parameters VariantId FieldId Ty)

  [(where (_ KindedVarIds _ (_ ... (VariantId FieldDecls) _ ...)) (decl-of-adt Γ AdtId))
   (where/error Substitution (create-substitution KindedVarIds Parameters))
   (where (_ ... (FieldId Ty) _ ...) FieldDecls)
   (where/error Ty_substituted (apply-substitution Substitution Ty))
   ----------------------------------
   (field-ty Γ AdtId Parameters VariantId FieldId Ty_substituted)
   ]

  )


(define-judgment-form
  formality-mir-extended

  #:mode (type-of/Operand I I O)
  #:contract (type-of/Operand Γ Operand Ty)

  [; copy
   (type-of/Place Γ Place Ty)
   ------------------------------------------
   (type-of/Operand Γ (copy Place) Ty)]

  [; move
   (type-of/Place Γ Place Ty)
   ------------------------------------------
   (type-of/Operand Γ (move Place) Ty)
   ]

  [; constant
   ;
   ; FIXME
   ------------------------------------------
   (type-of/Operand Γ (const _) (scalar-ty i32))
   ]
  )

(define-judgment-form formality-mir-extended

  ;; Apply type-of/Operand to a list of operands

  #:mode (type-of/Operands I I O)
  #:contract (type-of/Operands Γ Operands Tys)

  [(type-of/Operand Γ Operand Ty) ...
   ------------------------------------------
   (type-of/Operands Γ (Operand ...) (Ty ...))
   ]
  )

(define-judgment-form formality-mir-extended
  ;; Computes the `Ty` of a `Rvalue`

  #:mode (type-of/Rvalue I I O)
  #:contract (type-of/Rvalue Γ Rvalue Ty)

  [; use
   (type-of/Operand Γ Operand Ty)
   ------------------------------------------
   (type-of/Rvalue Γ (use Operand) Ty)
   ]

  [; ref
   (type-of/Place Γ Place Ty)
   ------------------------------------------
   (type-of/Rvalue Γ (ref Lt () Place) (& Lt Ty))
   ]

  [; ref-mut
   (type-of/Place Γ Place Ty)
   ------------------------------------------
   (type-of/Rvalue Γ (ref Lt (mut) Place) (&mut Lt Ty))
   ]

  [; binop
   (type-of/Operand Γ Operand_rhs (rigid-ty ScalarId_ty ()))
   (type-of/Operand Γ Operand_lhs (rigid-ty ScalarId_ty ()))
   ------------------------------------------
   (type-of/Rvalue Γ (BinaryOp Operand_rhs Operand_lhs) (rigid-ty ScalarId_ty ()))
   ]
  )