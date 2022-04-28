#lang racket
(require redex
         "grammar.rkt"
         "../decl/grammar.rkt")
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

  [--------------------------------------------------
   (types/rvalue Γ (nullary-op Ty) (scalar-ty usize))]

  [(types/operand Γ operand Ty)
   ----------------------------------------
   (types/rvalue Γ (rvalue-use operand) Ty)]

  [(types/place Γ place (TyPlace Ty))
   --------------------------------------------------------------------------------
   (types/rvalue Γ (rvalue-ref Lt MaybeMut place) (TyRigid (Ref MaybeMut) (Lt Ty)))]

  [(types/operand Γ operand_a (TyRigid ScalarId_ty ()))
   (types/operand Γ operand_b (TyRigid ScalarId_ty ()))
   ----------------------------------------------------------------------------------
   (types/rvalue Γ (rvalue-binary-op _ operand_a operand_b) (TyRigid ScalarId_ty ()))]
)

(define-judgment-form
  formality-mir+Γ
  #:mode (types/block I I O)
  #:contract (types/block Γ basic-block-data Γ)

  [(types/term Γ_0 terminator Γ_1)
   -------------------------------------
   (types/block Γ_0 (() terminator) Γ_1)]

  [(types/stmt Γ_0 statement_hd Γ_1)
   (types/block Γ_1 ((statement_tl ...) terminator) Γ_2)
   ------------------------------------------------------------------
   (types/block Γ_0 ((statement_hd statement_tl ...) terminator) Γ_2)]
)

(define-judgment-form
  formality-mir+Γ
  #:mode (types/stmt I I O)
  #:contract (types/stmt Γ statement Γ)

  [--------------------------------
   (types/stmt Γ (statement-nop) Γ)]

  [(types/rvalue Γ rvalue Ty)
   (types/place Γ place (TyPlace Ty))
   ------------------------------------------------
   (types/stmt Γ (statement-assign place rvalue) Γ)]
)

(define-judgment-form
  formality-mir+Γ
  #:mode (types/term I I O)
  #:contract (types/term Γ terminator Γ)

  [(types/operand Γ operand_fn (TyRigid (Fn _ _) (Ty_arg ..._n Ty_ret)))
   (types/operand Γ operand_arg Ty_arg) ...
   (types/place Γ place (TyPlace Ty_ret))
   -------------------------------------------------------------------------
   (types/term Γ (terminator-call operand_fn (operand_arg ..._n) place _) Γ)]
)

(define-metafunction formality-mir+Γ
  apply-projections : CrateDecls PlaceTy projections -> PlaceTy

  [(apply-projections _ PlaceTy ()) PlaceTy]

  [(apply-projections CrateDecls
                      (TyPlace (TyRigid (Ref _) (_ Ty)))
                      (projection-deref projection ...))
   (apply-projections CrateDecls (TyPlace Ty) (projection ...))]

  [(apply-projections CrateDecls
                      (TyPlace (TyRigid AdtId _))
                      ((projection-field FieldId) projection ...))
   (apply-projections CrateDecls (TyPlace Ty_field) (projection ...))
   
   (where (struct _ _ ((_ FieldDecls))) (item-with-id CrateDecls AdtId))
   (where (_ ... (FieldId Ty_field) _ ...) FieldDecls)]

  [(apply-projections CrateDecls
                      (TyPlace Ty_adt)
                      ((projection-downcast VariantId) projection ...))
   (apply-projections CrateDecls (TyPlaceVariant Ty_adt VariantId) (projection ...))
   
   (where (TyRigid AdtId _) Ty_adt)
   (where (enum _ _ AdtVariants) (item-with-id CrateDecls AdtId))
   (where (_ ... (VariantId _) _ ...) AdtVariants)]

  [(apply-projections CrateDecls
                      (TyPlaceVariant (TyRigid AdtId _) VariantId)
                      ((projection-field FieldId) projection ...))
   (apply-projections CrateDecls (TyPlace Ty_field) (projection ...))
   
   (where (enum _ _ AdtVariants) (item-with-id CrateDecls AdtId))
   (where (_ ... (VariantId FieldDecls) _ ...) AdtVariants)
   (where (_ ... (FieldId Ty_field) _ ...) FieldDecls)]
  )

(module+ test
  (redex-let*
   formality-mir+Γ

   ((; struct Foo { counter: i32 }
     AdtDecl_Foo (term (Foo (struct () () ((struct-variant ((counter (scalar-ty i32)))))))))

    (; enum Bar { Baz { counter: i32 } }
     AdtDecl_Bar (term (Bar (enum () () ((Baz ((counter (scalar-ty i32)))))))))

    (; crate TheCrate { ... }
     CrateDecl (term (TheCrate (crate (AdtDecl_Foo AdtDecl_Bar)))))
    
    ; let foo: Foo
    (Γ (term (((foo (TyRigid Foo ())) (bar (TyRigid Bar ()))) (CrateDecl))))
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
      (list (term (TyRigid Foo ()))))

    (test-equal
      (judgment-holds
        (types/place Γ
                    (foo ((projection-field counter)))
                    (TyPlace Ty))
        Ty)
      (list (term (scalar-ty i32))))

    (test-equal
      (judgment-holds
        (types/place Γ
                    (bar ((projection-downcast Baz) (projection-field counter)))
                    (TyPlace Ty))
        Ty)
      (list (term (scalar-ty i32))))

    (test-equal
      (judgment-holds
        (types/rvalue Γ
                      (rvalue-binary-op + (operand-constant 1) (operand-constant 2))
                      Ty)
        Ty)
      (list (term (scalar-ty i32))))
  )
)
