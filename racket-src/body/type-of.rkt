#lang racket
(require redex/reduction-semantics
         "../logic/substitution.rkt"
         "../ty/user-ty.rkt"
         "grammar.rkt"
         )
(provide type-of/Place
         type-of/Rvalue
         type-of/Operands
         type-of/Operand
         field-tys
         )

;; type-of judgments:
;;
;; * Output the type of a place, rvalue, etc

(define-judgment-form
  formality-body
  #:mode (type-of/Place I I O)
  #:contract (type-of/Place Γ Place Ty)

  [(place-type-of Γ Place Ty ())
   ----------------------------------------
   (type-of/Place Γ Place Ty)
   ]

  )

(define-judgment-form
  formality-body
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
   ;s
   ; extract the name of the singular variant
   (place-type-of Γ Place (rigid-ty AdtId Parameters) ())
   (where (struct AdtId _ where _ ((VariantId _))) (find-adt Γ AdtId))
   (field-tys Γ AdtId Parameters VariantId (_ ... (FieldId Ty_field) _ ...))
   ----------------------------------
   (place-type-of Γ (field Place FieldId) Ty_field ())
   ]

  [; field of an enum
   ;
   ; must have been downcast to a particular variant
   (place-type-of Γ Place (rigid-ty AdtId Parameters) (VariantId))
   (where (enum AdtId _ _ where _) (find-adt Γ AdtId))
   (field-tys Γ AdtId Parameters VariantId (_ ... (FieldId Ty_field) _ ...))
   ----------------------------------
   (place-type-of Γ (field Place FieldId) Ty_field ())

   ]

  [; downcast to an enum variant
   (place-type-of Γ Place (rigid-ty AdtId Parameters) ())
   (where (enum AdtId _ where _ (_ ... (VariantId _) _ ...)) (find-adt Γ AdtId))
   ----------------------------------
   (place-type-of Γ (downcast Place VariantId) (rigid-ty AdtId Parameters) (VariantId))
   ]

  ; FIXME: indexing, unions
  )

(define-judgment-form
  formality-body
  #:mode (type-of/LocalId I I O)
  #:contract (type-of/LocalId Γ LocalId Ty)

  [(where/error LocalDecls (local-decls-of-Γ Γ))
   (where (_ ... (LocalId Ty _) _ ...) LocalDecls)
   ----------------------------------------
   (type-of/LocalId Γ LocalId Ty)
   ]

  )

(define-judgment-form
  formality-body

  ;; Get the type of a field from a given variant of a given ADT,
  ;; substituting type parameters.

  #:mode (field-tys I I I I O)
  #:contract (field-tys Γ AdtId Parameters VariantId ((FieldId Ty) ...))

  [(where (_ AdtId KindedVarIds where _ (_ ... (VariantId FieldDecls) _ ...)) (find-adt Γ AdtId))
   (where/error Substitution (create-substitution KindedVarIds Parameters))
   (where ((FieldId Ty) ...) FieldDecls)
   (where/error (Ty_substituted ...) ((apply-substitution Substitution Ty) ...))
   ----------------------------------
   (field-tys Γ AdtId Parameters VariantId ((FieldId Ty_substituted) ...))
   ]

  )

(define-judgment-form
  formality-body

  #:mode (type-of/Constant I I O)
  #:contract (type-of/Constant Γ Constant Ty)

  [; integer
   ; FIXME: choose proper int type
   ------------------------------------------
   (type-of/Constant Γ number (user-ty i32))]

  [; true
   ------------------------------------------
   (type-of/Constant Γ true (user-ty bool))]

  [; false
   ------------------------------------------
   (type-of/Constant Γ false (user-ty bool))]

  [; function
   (where/error (fn _ (KindedVarId ..._n _ ...) (Ty_arg ...) -> Ty_ret _ _ _) (find-fn Γ FnId))
   (where/error Substitution (create-substitution (KindedVarId ...) (Parameter ...)))
   (where/error (Ty_argsubst ...) ((apply-substitution Substitution Ty_arg) ...))
   (where/error Ty_retsubst (apply-substitution Substitution Ty_ret))
   (where/error number_args ,(length (term (Ty_arg ...))))
   (where/error Ty_fnptr (rigid-ty (fn-ptr "Rust" number_args) (Ty_argsubst ... Ty_retsubst)))
   ------------------------------------------
   (type-of/Constant Γ (fn-ptr FnId (Parameter ..._n)) Ty_fnptr)]

  [; static
   (where/error (static _ _ _ _ : Ty = _) (find-static Γ StaticId))
   ------------------------------------------
   (type-of/Constant Γ (static StaticId) Ty)]

  [; tuple
   (type-of/Constant Γ Constant Ty) ...
   (where/error number_args ,(length (term (Ty ...))))
   ------------------------------------------
   (type-of/Constant Γ (tuple [Constant ...]) (rigid-ty (tuple number_args) (Ty ...)))]
  )

(define-judgment-form
  formality-body

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
   (type-of/Constant Γ Constant Ty)
   ------------------------------------------
   (type-of/Operand Γ (const Constant) Ty)
   ]
  )

(define-judgment-form formality-body

  ;; Apply type-of/Operand to a list of operands

  #:mode (type-of/Operands I I O)
  #:contract (type-of/Operands Γ Operands Tys)

  [(type-of/Operand Γ Operand Ty) ...
   ------------------------------------------
   (type-of/Operands Γ (Operand ...) (Ty ...))
   ]
  )

(define-judgment-form formality-body
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
   (type-of/Rvalue Γ (ref Lt () Place) (rigid-ty (ref ()) (Lt Ty)))
   ]

  [; ref-mut
   (type-of/Place Γ Place Ty)
   ------------------------------------------
   (type-of/Rvalue Γ (ref Lt mut Place) (rigid-ty (ref mut) (Lt Ty)))
   ]

  [; binop
   (type-of/Operand Γ Operand_rhs (rigid-ty ScalarId_ty ()))
   (type-of/Operand Γ Operand_lhs (rigid-ty ScalarId_ty ()))
   ------------------------------------------
   (type-of/Rvalue Γ (BinaryOp Operand_rhs Operand_lhs) (rigid-ty ScalarId_ty ()))
   ]

  [; aggregate tuple
   (type-of/Operands Γ Operands Tys)
   ------------------------------------------
   (type-of/Rvalue Γ (tuple Operands) (rigid-ty (tuple ,(length (term Operands))) Tys))
   ]

  [; aggregate adt
   (where/error Ty_adt (rigid-ty AdtId Parameters))
   ------------------------------------------
   (type-of/Rvalue Γ ((adt AdtId VariantId Parameters) Operands) Ty_adt)
   ]
  )

(define-metafunction formality-body
  ;; Returns the `AdtDecl` of the ADT with the given `AdtId`.
  find-adt : Γ AdtId -> AdtDecl

  [(find-adt Γ AdtId)
   (adt-with-id (crate-decls-of-Γ Γ) AdtId)
   ]
  )

(define-metafunction formality-body
  ;; Returns the `FnDecl` of the function with the given `FnId`.
  find-fn : Γ FnId -> FnDecl

  [(find-fn Γ FnId)
   (fn-with-id (crate-decls-of-Γ Γ) FnId)
   ]
  )

(define-metafunction formality-body
  ;; Returns the `StaticDecl` of the function with the given `StaticId`.
  find-static : Γ StaticId -> StaticDecl

  [(find-static Γ StaticId)
   (static-with-id (crate-decls-of-Γ Γ) StaticId)
   ]
  )
