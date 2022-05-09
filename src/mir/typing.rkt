#lang racket
(require redex
         "grammar.rkt"
         "../decl/grammar.rkt")
(provide (all-defined-out))

(define-extended-language formality-mir+Γ formality-mir
  ;; Extension of `Ty` to optionally store a `VariantId`.
  [PlaceTy ::= (TyPlace Ty MaybeMut) (TyPlaceVariant Ty MaybeMut VariantId)]
  ;; Typing context storing bindings from locals to types and `CrateDecls`.
  [Γ ::= (LocalDecls CrateDecls)]
  )

(define-metafunction formality-mir+Γ
  ;; Returns the type of the local with the given `LocalId`.
  type-of-local : Γ LocalId -> PlaceTy

  [(type-of-local Γ LocalId)
   (TyPlace Ty MaybeMut)
   (where/error (LocalDecls _) Γ)
   (where (_ ... (LocalId Ty MaybeMut) _ ...) LocalDecls)]
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
  ;; Returns the derived type obtained by recursively applying `Projections` to a given `PlaceTy`.
  apply-projections : Γ PlaceTy Projections -> PlaceTy

  [(apply-projections _ PlaceTy ()) PlaceTy]

  [(apply-projections Γ
                      (TyPlace (TyRigid (Ref _) (_ Ty)) MaybeMut)
                      (ProjectionDeref Projection ...))
   (apply-projections Γ (TyPlace Ty MaybeMut) (Projection ...))]

  [(apply-projections Γ
                      (TyPlace (TyRigid AdtId _) MaybeMut)
                      ((ProjectionField FieldId) Projection ...))
   (apply-projections Γ (TyPlace Ty_field MaybeMut) (Projection ...))
   (where (struct _ _ ((_ FieldDecls))) (decl-of-adt Γ AdtId))
   (where (_ ... (FieldId Ty_field) _ ...) FieldDecls)]

  [(apply-projections Γ
                      (TyPlace Ty_adt MaybeMut)
                      ((ProjectionDowncast VariantId) Projection ...))
   (apply-projections Γ (TyPlaceVariant Ty_adt MaybeMut VariantId) (Projection ...))
   (where (TyRigid AdtId _) Ty_adt)
   (where (enum _ _ AdtVariants) (decl-of-adt Γ AdtId))
   (where (_ ... (VariantId _) _ ...) AdtVariants)]

  [(apply-projections Γ
                      (TyPlaceVariant (TyRigid AdtId _) MaybeMut VariantId)
                      ((ProjectionField FieldId) Projection ...))
   (apply-projections Γ (TyPlace Ty_field MaybeMut) (Projection ...))
   (where (enum _ _ AdtVariants) (decl-of-adt Γ AdtId))
   (where (_ ... (VariantId FieldDecls) _ ...) AdtVariants)
   (where (_ ... (FieldId Ty_field) _ ...) FieldDecls)]
  )

(define-judgment-form
  formality-mir+Γ
  #:mode (types/place I I O)
  #:contract (types/place Γ Place PlaceTy)

  [(where PlaceTy_local (type-of-local Γ LocalId))
   (where PlaceTy_proj (apply-projections Γ PlaceTy_local Projections))
   -------------------------------------------------------------------- "local"
   (types/place Γ (LocalId Projections) PlaceTy_proj)]
  )

(define-judgment-form
  formality-mir+Γ
  #:mode (types/operand I I O)
  #:contract (types/operand Γ Operand Ty)

  [(types/place Γ Place (TyPlace Ty _))
   ---------------------------------------- "copy"
   (types/operand Γ (OperandCopy Place) Ty)]

  [(types/place Γ Place (TyPlace Ty _))
   ---------------------------------------- "move"
   (types/operand Γ (OperandMove Place) Ty)]

  [----------------------------------------------------- "const"
   (types/operand Γ (OperandConstant _) (scalar-ty i32))]
  )

(define-judgment-form
  formality-mir+Γ
  #:mode (types/rvalue I I O)
  #:contract (types/rvalue Γ Rvalue Ty)

  [------------------------------------------------------- "nullop"
   (types/rvalue Γ (RvalueNullaryOp Ty) (scalar-ty usize))]

  [(types/operand Γ Operand Ty)
   --------------------------------------- "use"
   (types/rvalue Γ (RvalueUse Operand) Ty)]

  [(types/place Γ Place (TyPlace Ty _))
   ------------------------------------------------------------------- "ref"
   (types/rvalue Γ (RvalueRef Lt () Place) (TyRigid (Ref ()) (Lt Ty)))]

  [(types/place Γ Place (TyPlace Ty (mut)))
   ------------------------------------------------------------------------- "ref-mut"
   (types/rvalue Γ (RvalueRef Lt (mut) Place) (TyRigid (Ref (mut)) (Lt Ty)))]

  [(types/operand Γ Operand_a (TyRigid ScalarId_ty ()))
   (types/operand Γ Operand_b (TyRigid ScalarId_ty ()))
   -------------------------------------------------------------------------------- "binop"
   (types/rvalue Γ (RvalueBinaryOp _ Operand_a Operand_b) (TyRigid ScalarId_ty ()))]
  )

(define-judgment-form
  formality-mir+Γ
  #:mode (types/stmt I I O)
  #:contract (types/stmt Γ Statement Γ)

  [----------------------------- "nop"
   (types/stmt Γ StatementNop Γ)]

  [------------------------------------- "storage-live"
   (types/stmt Γ StatementStorageLive Γ)]

  [------------------------------------- "storage-dead"
   (types/stmt Γ StatementStorageDead Γ)]

  [(types/rvalue Γ Rvalue Ty)
   (types/place Γ Place (TyPlace Ty (mut)))
   ----------------------------------------------- "assign"
   (types/stmt Γ (StatementAssign Place Rvalue) Γ)]

  [(types/place Γ Place (TyPlace (TyRigid AdtId _) (mut)))
   (where (enum _ _ AdtVariants) (decl-of-adt Γ AdtId))
   (where (_ ... (VariantId _) _ ...) AdtVariants)
   ----------------------------------------------------------- "set-discr"
   (types/stmt Γ (StatementSetDiscriminant Place VariantId) Γ)]
  )

(define-judgment-form
  formality-mir+Γ
  #:mode (types/term I I O)
  #:contract (types/term Γ Terminator Γ)

  [----------------------------------- "goto"
   (types/term Γ (TerminatorGoto _) Γ)]

  [--------------------------------- "resume"
   (types/term Γ TerminatorResume Γ)]

  [-------------------------------- "abort"
   (types/term Γ TerminatorAbort Γ)]

  [--------------------------------- "return"
   (types/term Γ TerminatorReturn Γ)]

  [-------------------------------------- "unreachable"
   (types/term Γ TerminatorUnreachable Γ)]

  [(types/place Γ Place _)
   ----------------------------------------- "drop"
   (types/term Γ (TerminatorDrop Place _) Γ)]

  [(types/place Γ Place (TyPlace Ty _))
   (types/operand Γ Operand Ty)
   ----------------------------------------------------------- "drop-replace"
   (types/term Γ (TerminatorDropAndReplace Place Operand _) Γ)]

  [(types/operand Γ Operand_fn (TyRigid (Fn _ _) (Ty_arg ..._n Ty_ret)))
   (types/operand Γ Operand_arg Ty_arg) ...
   (types/place Γ Place (TyPlace Ty_ret (mut)))
   ------------------------------------------------------------------------ "call"
   (types/term Γ (TerminatorCall Operand_fn (Operand_arg ..._n) Place _) Γ)]
  )

(define-judgment-form
  formality-mir+Γ
  #:mode (types/block I I O)
  #:contract (types/block Γ BasicBlockData Γ)

  [(types/term Γ_0 Terminator Γ_1)
   ------------------------------------- "block-base"
   (types/block Γ_0 (() Terminator) Γ_1)]

  [(types/stmt Γ_0 Statement_hd Γ_1)
   (types/block Γ_1 ((Statement_tl ...) Terminator) Γ_2)
   ------------------------------------------------------------------ "block-step"
   (types/block Γ_0 ((Statement_hd Statement_tl ...) Terminator) Γ_2)]
  )

(module+ test
  (redex-let*
   formality-mir+Γ

   ((; struct Foo { counter: i32 }
     AdtDecl_Foo (term (Foo (struct () () ((struct-variant ((counter (scalar-ty i32)))))))))
    (Ty_Foo (term (TyRigid Foo ())))

    (; enum Bar { Baz { counter: i32 } }
     AdtDecl_Bar (term (Bar (enum () () ((Baz ((counter (scalar-ty i32)))))))))
    (Ty_Bar (term (TyRigid Bar ())))

    (; crate TheCrate { ... }
     CrateDecl (term (TheCrate (crate (AdtDecl_Foo AdtDecl_Bar)))))
    
    ; let foo: Foo; let bar: Bar;
    (Γ (term (((foo Ty_Foo ()) (bar Ty_Bar ())) (CrateDecl))))
    )

    (test-equal
      (judgment-holds
        (types/operand Γ
                       (OperandConstant 42)
                       Ty)
        Ty)
      (list (term (scalar-ty i32))))

    (test-equal
      (judgment-holds
        (types/place Γ
                     (foo ())
                     (TyPlace Ty ()))
        Ty)
      (list (term (TyRigid Foo ()))))

    (test-equal
      (judgment-holds
        (types/place Γ
                    (foo ((ProjectionField counter)))
                    (TyPlace Ty ()))
        Ty)
      (list (term (scalar-ty i32))))

    (test-equal
      (judgment-holds
        (types/place Γ
                    (bar ((ProjectionDowncast Baz) (ProjectionField counter)))
                    (TyPlace Ty ()))
        Ty)
      (list (term (scalar-ty i32))))

    (test-equal
      (judgment-holds
        (types/rvalue Γ
                      (RvalueBinaryOp + (OperandConstant 1) (OperandConstant 2))
                      Ty)
        Ty)
      (list (term (scalar-ty i32))))
  )
)
