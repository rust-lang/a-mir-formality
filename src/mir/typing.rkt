#lang racket
(require redex
         "grammar.rkt"
         "../decl/grammar.rkt")
(provide (all-defined-out))

(define-extended-language formality-mir+Γ formality-mir
  ;; Extension of `Ty` to optionally store a `VariantId`.
  [PlaceTy ::= (TyPlace Ty) (TyPlaceVariant Ty VariantId)]
  ;; Typing context storing bindings from locals to types.
  [Γ-ty ::= ((local-variable-id Ty) ...)]
  ;; Extended typing context, additionally containing `CrateDecls`.
  [Γ ::= (Γ-ty CrateDecls)]
  )

(define-metafunction formality-mir+Γ
  ;; Returns the type of the local with the given `local-variable-id`.
  type-of-local : Γ local-variable-id -> Ty

  [(type-of-local Γ local-variable-id)
   Ty
   (where/error (Γ-ty _) Γ)
   (where (_ ... (local-variable-id Ty) _ ...) Γ-ty)]
  )

(define-metafunction formality-mir+Γ
  ;; Returns the `AdtContents` of the ADT with the given `AdtId`.
  decl-of-adt : Γ AdtId -> AdtContents

  [(decl-of-adt Γ AdtId)
   AdtContents
   (where/error (_ CrateDecls) Γ)
   (where AdtContents (item-with-id CrateDecls AdtId))]
  )

(define-metafunction formality-mir+Γ
  ;; Returns the derived type obtained by recursively applying `projections` to a given `PlaceTy`.
  apply-projections : Γ PlaceTy projections -> PlaceTy

  [(apply-projections _ PlaceTy ()) PlaceTy]

  [(apply-projections Γ
                      (TyPlace (TyRigid (Ref _) (_ Ty)))
                      (projection-deref projection ...))
   (apply-projections Γ (TyPlace Ty) (projection ...))]

  [(apply-projections Γ
                      (TyPlace (TyRigid AdtId _))
                      ((projection-field FieldId) projection ...))
   (apply-projections Γ (TyPlace Ty_field) (projection ...))
   (where (struct _ _ ((_ FieldDecls))) (decl-of-adt Γ AdtId))
   (where (_ ... (FieldId Ty_field) _ ...) FieldDecls)]

  [(apply-projections Γ
                      (TyPlace Ty_adt)
                      ((projection-downcast VariantId) projection ...))
   (apply-projections Γ (TyPlaceVariant Ty_adt VariantId) (projection ...))
   (where (TyRigid AdtId _) Ty_adt)
   (where (enum _ _ AdtVariants) (decl-of-adt Γ AdtId))
   (where (_ ... (VariantId _) _ ...) AdtVariants)]

  [(apply-projections Γ
                      (TyPlaceVariant (TyRigid AdtId _) VariantId)
                      ((projection-field FieldId) projection ...))
   (apply-projections Γ (TyPlace Ty_field) (projection ...))
   (where (enum _ _ AdtVariants) (decl-of-adt Γ AdtId))
   (where (_ ... (VariantId FieldDecls) _ ...) AdtVariants)
   (where (_ ... (FieldId Ty_field) _ ...) FieldDecls)]
  )

(define-judgment-form
  formality-mir+Γ
  #:mode (types/place I I O)
  #:contract (types/place Γ place PlaceTy)

  [(where Ty_var (type-of-local Γ local-variable-id))
   (where PlaceTy (apply-projections Γ (TyPlace Ty_var) projections))
   ------------------------------------------------------------------ "local"
   (types/place Γ (local-variable-id projections) PlaceTy)]
  )

(define-judgment-form
  formality-mir+Γ
  #:mode (types/operand I I O)
  #:contract (types/operand Γ operand Ty)

  [(types/place Γ place (TyPlace Ty))
   ----------------------------------------- "copy"
   (types/operand Γ (operand-copy place) Ty)]

  [(types/place Γ place (TyPlace Ty))
   ----------------------------------------- "move"
   (types/operand Γ (operand-move place) Ty)]

  [------------------------------------------------------ "const"
   (types/operand Γ (operand-constant _) (scalar-ty i32))]
  )

(define-judgment-form
  formality-mir+Γ
  #:mode (types/rvalue I I O)
  #:contract (types/rvalue Γ rvalue Ty)

  [-------------------------------------------------- "nullop"
   (types/rvalue Γ (nullary-op Ty) (scalar-ty usize))]

  [(types/operand Γ operand Ty)
   ---------------------------------------- "use"
   (types/rvalue Γ (rvalue-use operand) Ty)]

  [(types/place Γ place (TyPlace Ty))
   -------------------------------------------------------------------------------- "ref"
   (types/rvalue Γ (rvalue-ref Lt MaybeMut place) (TyRigid (Ref MaybeMut) (Lt Ty)))]

  [(types/operand Γ operand_a (TyRigid ScalarId_ty ()))
   (types/operand Γ operand_b (TyRigid ScalarId_ty ()))
   ---------------------------------------------------------------------------------- "binop"
   (types/rvalue Γ (rvalue-binary-op _ operand_a operand_b) (TyRigid ScalarId_ty ()))]
  )

(define-judgment-form
  formality-mir+Γ
  #:mode (types/stmt I I O)
  #:contract (types/stmt Γ statement Γ)

  [-------------------------------- "nop"
   (types/stmt Γ (statement-nop) Γ)]

  [----------------------------------------- "storage-live"
   (types/stmt Γ (statement-storage-live) Γ)]

  [----------------------------------------- "storage-dead"
   (types/stmt Γ (statement-storage-dead) Γ)]

  [(types/rvalue Γ rvalue Ty)
   (types/place Γ place (TyPlace Ty))
   ------------------------------------------------ "assign"
   (types/stmt Γ (statement-assign place rvalue) Γ)]

  [(types/place Γ place (TyPlace (TyRigid AdtId _)))
   (where (enum _ _ AdtVariants) (decl-of-adt Γ AdtId))
   (where (_ ... (VariantId _) _ ...) AdtVariants)
   -------------------------------------------------------------- "set-discr"
   (types/stmt Γ (statement-set-discriminant place VariantId) Γ)]
  )

(define-judgment-form
  formality-mir+Γ
  #:mode (types/term I I O)
  #:contract (types/term Γ terminator Γ)

  [------------------------------------ "goto"
   (types/term Γ (terminator-goto _) Γ)]

  [---------------------------------- "resume"
   (types/term Γ terminator-resume Γ)]

  [--------------------------------- "abort"
   (types/term Γ terminator-abort Γ)]

  [---------------------------------- "return"
   (types/term Γ terminator-return Γ)]

  [--------------------------------------- "unreachable"
   (types/term Γ terminator-unreachable Γ)]

  [(types/place Γ place _)
   ------------------------------------------ "drop"
   (types/term Γ (terminator-drop place _) Γ)]

  [(types/place Γ place (TyPlace Ty))
   (types/operand Γ operand Ty)
   -------------------------------------------------------------- "drop-replace"
   (types/term Γ (terminator-drop-and-replace place operand _) Γ)]

  [(types/operand Γ operand_fn (TyRigid (Fn _ _) (Ty_arg ..._n Ty_ret)))
   (types/operand Γ operand_arg Ty_arg) ...
   (types/place Γ place (TyPlace Ty_ret))
   ------------------------------------------------------------------------- "call"
   (types/term Γ (terminator-call operand_fn (operand_arg ..._n) place _) Γ)]
  )

(define-judgment-form
  formality-mir+Γ
  #:mode (types/block I I O)
  #:contract (types/block Γ basic-block-data Γ)

  [(types/term Γ_0 terminator Γ_1)
   ------------------------------------- "block-base"
   (types/block Γ_0 (() terminator) Γ_1)]

  [(types/stmt Γ_0 statement_hd Γ_1)
   (types/block Γ_1 ((statement_tl ...) terminator) Γ_2)
   ------------------------------------------------------------------ "block-step"
   (types/block Γ_0 ((statement_hd statement_tl ...) terminator) Γ_2)]
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
