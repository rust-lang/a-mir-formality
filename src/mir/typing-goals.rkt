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

(define-metafunction formality-mir+Γ
  ;; Computes the `PlaceTy` of a `Place`
  typeof/place : Γ Place -> PlaceTy

  [; local
   (typeof/place Γ (LocalId Projections))
   PlaceTy_proj
   (where PlaceTy_local (type-of-local Γ LocalId))
   (where PlaceTy_proj (apply-projections Γ PlaceTy_local Projections))]
  )

(define-metafunction formality-mir+Γ
  ;; Computes the `Ty` of a `Operand`
  typeof/operand : Γ Operand -> Ty

  [; copy
   (typeof/operand Γ (OperandCopy Place))
   Ty
   (where (TyPlace Ty _) (typeof/place Γ Place))]

  [; move
   (typeof/operand Γ (OperandMove Place))
   Ty
   (where (TyPlace Ty _) (typeof/place Γ Place))]

  [; constant
   (typeof/operand Γ (OperandConstant _))
   (scalar-ty i32)]
  )
(define-metafunction formality-mir+Γ
  typeof/operands : Γ (Operand ...) -> (Ty ...)

  [(typeof/operands Γ ()) ()]
  [(typeof/operands Γ (Operand_hd Operand_tl ...))
   ((typeof/operand Γ Operand_hd) Ty_tl ...)
   (where (Ty_tl ...) (typeof/operands Γ (Operand_tl ...)))]
  )

(define-metafunction formality-mir+Γ
  ;; Computes the `Goals` that have to hold for a `Operand`
  goals/operand : Γ Operand -> Goals

  [; copy
   (goals/operand Γ (OperandCopy Place))
   ((Implemented (rust:Copy (Ty))))
   (where (TyPlace Ty _) (typeof/place Γ Place))]

  [; move
   (goals/operand Γ (OperandMove Place))
   ()]

  [; constant
   (goals/operand Γ (OperandConstant _))
   ()]
  )
(define-metafunction formality-mir+Γ
  goals/operands : Γ (Operand ...) -> Goals

  [(goals/operands Γ ()) ()]

  [(goals/operands Γ (Operand_hd Operand_tl ...))
   (Goal_hd ... Goal_tl ...)
   (where (Goal_hd ...) (goals/operand Γ Operand_hd))
   (where (Goal_tl ...) (goals/operands Γ (Operand_tl ...)))]
  )

(define-metafunction formality-mir+Γ
  ;; Computes the `Ty` of a `Rvalue`
  typeof/rvalue : Γ Rvalue -> Ty

  [; use
   (typeof/rvalue Γ (RvalueUse Operand))
   Ty
   (where Ty (typeof/operand Γ Operand))]

  [; ref
   (typeof/rvalue Γ (RvalueRef Lt () Place))
   (TyRigid (Ref ()) (Lt Ty))
   (where (TyPlace Ty _) (typeof/place Γ Place))]

  [; ref-mut
   (typeof/rvalue Γ (RvalueRef Lt (mut) Place))
   (TyRigid (Ref (mut)) (Lt Ty))
   (where (TyPlace Ty (mut)) (typeof/place Γ Place))]

  [; nullop
   (typeof/rvalue Γ (RvalueNullaryOp Ty))
   (scalar-ty usize)]

  [; binop
   (typeof/rvalue Γ (RvalueBinaryOp _ Operand_rhs _))
   (TyRigid ScalarId_ty ())
   (where (TyRigid ScalarId_ty ()) (typeof/operand Γ Operand_rhs))]
  )

(define-metafunction formality-mir+Γ
  ;; Computes the `Goals` that have to hold for a `Rvalue`
  goals/rvalue : Γ Rvalue -> Goals

  [; use
   (goals/rvalue Γ (RvalueUse Operand))
   (Goal_op ...)
   (where (Goal_op ...) (goals/operand Γ Operand))]

  [; ref
   (goals/rvalue Γ (RvalueRef Lt MaybeMut Place))
   ()]

  [; nullop
   (goals/rvalue Γ (RvalueNullaryOp Ty))
   ()]

  [; binop
   (goals/rvalue Γ (RvalueBinaryOp _ Operand_rhs Operand_lhs))
   ((Ty_rhs == Ty_lhs) Goal_rhs ... Goal_lhs ...)
   (where Ty_rhs (typeof/operand Γ Operand_rhs))
   (where Ty_lhs (typeof/operand Γ Operand_lhs))
   (where (Goal_rhs ...) (goals/operand Γ Operand_rhs))
   (where (Goal_lhs ...) (goals/operand Γ Operand_lhs))]
  )

(define-metafunction formality-mir+Γ
  ;; Computes the `Goals` that have to hold for a `Statement`
  goals/stmt : Γ Statement -> Goals

  [; nop
   (goals/stmt Γ StatementNop)
   ()]

  [; storage-live
   (goals/stmt Γ StatementStorageLive)
   ()]

  [; storage-dead
   (goals/stmt Γ StatementStorageLive)
   ()]

  [; assign
   (goals/stmt Γ (StatementAssign Place Rvalue))
   ((Ty_rvalue <= Ty_place) Goal_rvalue ...)
   (where Ty_rvalue (typeof/rvalue Γ Rvalue))
   (where (TyPlace Ty_place (mut)) (typeof/place Γ Place))
   (where (Goal_rvalue ...) (goals/rvalue Γ Rvalue))]

  ; TODO: set-discr
  )

(define-metafunction formality-mir+Γ
  ;; Computes the `Goals` that have to hold for a `Terminator`
  goals/term : Γ Terminator -> Goals

  [; goto
   (goals/term Γ (TerminatorGoto _))
   ()]

  [; resume
   (goals/term Γ TerminatorResume)
   ()]

  [; abort
   (goals/term Γ TerminatorAbort)
   ()]

  [; return
   (goals/term Γ TerminatorReturn)
   ()]

  [; unreachable
   (goals/term Γ TerminatorUnreachable)
   ()]

  [; drop
   (goals/term Γ (TerminatorDrop _ _))
   ()]

  [; drop-replace
   (goals/term Γ (TerminatorDropAndReplace Place Operand _))
   ((Ty_operand == Ty_place) Goal_op ...)
   (where Ty_operand (typeof/operand Γ Operand))
   (where (TyPlace Ty_place _) (typeof/place Γ Place))
   (where (Goal_op ...) (goals/operand Γ Operand))]

  [; call
   (goals/term Γ (TerminatorCall Operand_fn (Operand_arg ..._n) Place _))
   ((Ty_ret <= Ty_place) (Ty_oparg <= Ty_arg) ... Goal_fn ... Goal_arg ...)
   (where (TyRigid (Fn _ _) (Ty_arg ..._n Ty_ret)) (typeof/operand Γ Operand_fn))
   (where (Ty_oparg ...) (typeof/operands Γ (Operand_arg ...)))
   (where (TyPlace Ty_place (mut)) (typeof/place Γ Place))
   (where (Goal_fn ...) (goals/operand Γ Operand_fn))
   (where (Goal_arg ...) (goals/operands Γ (Operand_arg ...)))]
  )

(define-metafunction formality-mir+Γ
  ;; Computes the `Goals` that have to hold for a `BasicBlockData`
  goals/block : Γ BasicBlockData -> Goals

  [; block
   (goals/block Γ (() Terminator))
   (Goal_term ...)
   (where (Goal_term ...) (goals/term Γ Terminator))]

  [; block
   (goals/block Γ ((Statement_hd Statement_tl ...) Terminator))
   (Goal_hd ... Goal_tl ...)
   (where (Goal_hd ...) (goals/stmt Γ Statement_hd))
   (where (Goal_tl ...) (goals/block Γ ((Statement_tl ...) Terminator)))]
  )
