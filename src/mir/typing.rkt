#lang racket
(require redex
         "grammar.rkt"
         "../ty/grammar.rkt")
(provide (all-defined-out))

(define-extended-language formality-mir+Γ formality-mir
  [PlaceTy ::= (TyPlace Ty) (TyPlaceVariant Ty VariantId)]
  [Γ-ty ::= ((local-variable-id Ty) ...)]
  [Γ ::= (Γ-ty CrateDecls)])

(define-judgment-form
  formality-mir+Γ
  #:mode (types/place I I O)
  #:contract (types/place Γ place PlaceTy)

  [(where (_ ... (local-variable-id Ty_var) _ ...) Γ-ty)
   (where PlaceTy (apply-projections CrateDecls (TyPlace Ty_var) projections))
   ---------------------------------------------------------------------------
   (types/place (Γ-ty CrateDecls) (local-variable-id projections) PlaceTy)]
)

(define-judgment-form
  formality-mir+Γ
  #:mode (types/operand I I O)
  #:contract (types/operand Γ operand Ty)

  [(types/place Γ place (TyPlace Ty))
   -----------------------------------------
   (types/operand Γ (operand-copy place) Ty)]

  [(types/place Γ place (TyPlace Ty))
   -----------------------------------------
   (types/operand Γ (operand-move place) Ty)]

  [------------------------------------------------------
   (types/operand Γ (operand-constant _) (scalar-ty i32))]
)

(define-judgment-form
  formality-mir+Γ
  #:mode (types/rvalue I I O)
  #:contract (types/rvalue Γ rvalue Ty)

  [-----------------------------------
   (types/rvalue Γ (nullary-op Ty) Ty)]

  [(types/operand Γ operand Ty)
   ----------------------------------------
   (types/rvalue Γ (rvalue-use operand) Ty)]

  [(types/place Γ place (TyPlace Ty))
   ------------------------------------------------------------------------------------------------
   (types/rvalue Γ (rvalue-ref lifetime mutability place) (TyRigid (Ref mutability) (lifetime Ty)))]

  [(types/operand Γ operand_a (TyRigid ScalarId_ty ()))
   (types/operand Γ operand_b (TyRigid ScalarId_ty ()))
   ----------------------------------------------------------------------------------
   (types/rvalue Γ (rvalue-binary-op _ operand_a operand_b) (TyRigid ScalarId_ty ()))]
)

(define-judgment-form
  formality-mir+Γ
  #:mode (types-ok I I O)
  #:contract (types-ok Γ any Γ)

  ; blocks
  [(types-ok Γ_0 statements Γ_1)
   (types-ok Γ_1 terminator Γ_2)
   -------------------------------------------------------------
   (types-ok Γ_0 (basic-block-data (statements terminator)) Γ_2)]

  [------------------------------
   (types-ok Γ (statements ()) Γ)]

  [(types-ok Γ_0 statement_hd Γ_1)
   (types-ok Γ_1 (statements (statement_tl ...)) Γ_2)
   ---------------------------------------------------------------
   (types-ok Γ_0 (statements (statement_hd statement_tl ...)) Γ_2)]

  ; statements
  [------------------------------
   (types-ok Γ (statement-nop) Γ)]

  [(types/rvalue Γ rvalue Ty)
   (types/place Γ place (TyPlace Ty))
   ----------------------------------------------
   (types-ok Γ (statement-assign place rvalue) Γ)]

  ;[(types Γ rvalue Ty)
  ;-------------------------------------------------------------------------------------------------
  ; (types-ok Γ (statement-assign (place (local-variable-id ())) rvalue) (local-variable-id : Ty Γ))]

   ; terminators
  [(types/operand Γ operand_fn (TyRigid (Fn _ _) (Ty_arg ..._n Ty_ret)))
   (types/operand Γ operand_arg Ty_arg) ...
   (types/place Γ place (TyPlace Ty_ret))
    ----------------------------------------------------------------------
   (types-ok Γ (terminator-call operand_fn (operand_arg ..._n) place _) Γ)]
)

(define-metafunction formality-mir+Γ
  apply-projections : CrateDecls PlaceTy projections -> PlaceTy

  [(apply-projections _ PlaceTy ()) PlaceTy]

  [(apply-projections CrateDecls
                      (TyPlace (TyRigid (Ref _) (_ Ty)))
                      (projection-deref projection ...))
   (apply-projections CrateDecls (TyPlace Ty) (projection ...))]

  [(apply-projections CrateDecls
                      (TyPlace Ty_adt)
                      ((projection-field FieldId) projection ...))
   (apply-projections CrateDecls (TyPlace Ty_field) (projection ...))
   
   (where (TyRigid AdtId Parameters) Ty_adt)
   (where (struct _ _ ((VariantId FieldDecls))) (item-with-id CrateDecls AdtId))
   (where (_ ... (FieldId Ty_field) _ ...) FieldDecls)]

  [(apply-projections CrateDecls
                      (TyPlace Ty)
                      ((projection-downcast VariantId) projection ...))
   (apply-projections CrateDecls (TyPlaceVariant Ty VariantId) (projection ...))
   
   (where (TyRigid AdtId Parameters) Ty)
   (where (enum _ _ AdtVariants) (item-with-id CrateDecls AdtId))
   (where (_ ... (VariantId _) _ ...) AdtVariants)]

  [(apply-projections CrateDecls
                      (TyPlaceVariant Ty_adt VaraintId)
                      ((projection-field FieldId) projection ...))
   (apply-projections CrateDecls (TyPlace Ty_field) (projection ...))
   
   (where (TyRigid AdtId Parameters) Ty_adt)
   (where (enum _ _ AdtVariants) (item-with-id CrateDecls AdtId))
   (where (_ ... (VariantId FieldDecls) _ ...) AdtVariants)
   (where (_ ... (FieldId Ty_field) _ ...) FieldDecls)]
  )

(module+ test
  (redex-let*
   formality-mir+Γ

   ((CrateDecl (term (TheCrate (crate ()))))
    (Γ (term (((foo (scalar-ty i32))) (CrateDecl))))
    )

    (test-equal
      (judgment-holds
        (types/operand Γ
                       (operand-constant 42)
                       Ty)
        Ty)
      (list (term (scalar-ty i32))))

    (test-equal
      (judgment-holds
        (types/place Γ
                     (foo ())
                     (TyPlace Ty))
        Ty)
      (list (term (scalar-ty i32))))
  )
)
