#lang racket
(require redex
         "grammar.rkt"
         "../decl/grammar.rkt")
(provide (all-defined-out))

(define-extended-language formality-mir+Γ formality-mir
  ;; Extension of `Ty` to optionally store a `VariantId`.
  [PlaceTy ::= (place-ty Ty MaybeMut) (place-ty-variant Ty MaybeMut VariantId)]
  ;; Typing context storing bindings from locals to types and `CrateDecls`.
  [Γ ::= (LocalDecls CrateDecls)]
  )

(define-metafunction formality-mir+Γ
  ;; Returns the type of the local with the given `LocalId`.
  type-of-local : Γ LocalId -> PlaceTy

  [(type-of-local Γ LocalId)
   (place-ty Ty MaybeMut)
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
  ;; Computes the `PlaceTy` of a `Place`
  typeof/place : Γ Place -> PlaceTy

  [; local
   (typeof/place Γ LocalId)
   (type-of-local Γ LocalId)]

  [; deref
   (typeof/place Γ (* Place))
   (place-ty Ty MaybeMut)
   (where (place-ty (rigid-ty (ref _) (_ Ty)) MaybeMut) (typeof/place Γ Place))]

  [; field-struct
   (typeof/place Γ (field Place FieldId))
   (place-ty Ty_field MaybeMut)
   (where (place-ty (rigid-ty AdtId _) MaybeMut) (typeof/place Γ Place))
   (where (struct _ _ ((_ FieldDecls))) (decl-of-adt Γ AdtId))
   (where (_ ... (FieldId Ty_field) _ ...) FieldDecls)]

  [; downcast
   (typeof/place Γ (downcast Place VariantId))
   (place-ty-variant Ty_adt MaybeMut VariantId)
   (where (place-ty Ty_adt MaybeMut) (typeof/place Γ Place))
   (where (rigid-ty AdtId _) Ty_adt)
   (where (enum _ _ AdtVariants) (decl-of-adt Γ AdtId))
   (where (_ ... (VariantId _) _ ...) AdtVariants)]

  [; field-enum
   (typeof/place Γ (field  Place FieldId))
   (place-ty Ty_field MaybeMut)
   (where (place-ty-variant (rigid-ty AdtId _) MaybeMut VariantId) (typeof/place Γ Place))
   (where (enum _ _ AdtVariants) (decl-of-adt Γ AdtId))
   (where (_ ... (VariantId FieldDecls) _ ...) AdtVariants)
   (where (_ ... (FieldId Ty_field) _ ...) FieldDecls)]
  )

(define-metafunction formality-mir+Γ
  ;; Computes the `Ty` of a `Operand`
  typeof/operand : Γ Operand -> Ty

  [; copy
   (typeof/operand Γ (copy Place))
   Ty
   (where (place-ty Ty _) (typeof/place Γ Place))]

  [; move
   (typeof/operand Γ (move Place))
   Ty
   (where (place-ty Ty _) (typeof/place Γ Place))]

  [; constant
   (typeof/operand Γ (const _))
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
   (goals/operand Γ (copy Place))
   ((is-implemented (rust:Copy (Ty))))
   (where (place-ty Ty _) (typeof/place Γ Place))]

  [; move
   (goals/operand Γ (move Place))
   ()]

  [; constant
   (goals/operand Γ (const _))
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
   (typeof/rvalue Γ (use Operand))
   Ty
   (where Ty (typeof/operand Γ Operand))]

  [; ref
   (typeof/rvalue Γ (ref Lt () Place))
   (rigid-ty (ref ()) (Lt Ty))
   (where (place-ty Ty _) (typeof/place Γ Place))]

  [; ref-mut
   (typeof/rvalue Γ (ref Lt (mut) Place))
   (rigid-ty (ref (mut)) (Lt Ty))
   (where (place-ty Ty (mut)) (typeof/place Γ Place))]

  [; binop
   (typeof/rvalue Γ (BinaryOp Operand_rhs _))
   (rigid-ty ScalarId_ty ())
   (where (rigid-ty ScalarId_ty ()) (typeof/operand Γ Operand_rhs))]
  )

(define-metafunction formality-mir+Γ
  ;; Computes the `Goals` that have to hold for a `Rvalue`
  goals/rvalue : Γ Rvalue -> Goals

  [; use
   (goals/rvalue Γ (use Operand))
   (Goal_op ...)
   (where (Goal_op ...) (goals/operand Γ Operand))]

  [; ref
   (goals/rvalue Γ (ref Lt MaybeMut Place))
   ()]

  [; binop
   (goals/rvalue Γ (BinaryOp Operand_rhs Operand_lhs))
   ((Ty_rhs == Ty_lhs) Goal_rhs ... Goal_lhs ...)
   (where Ty_rhs (typeof/operand Γ Operand_rhs))
   (where Ty_lhs (typeof/operand Γ Operand_lhs))
   (where (Goal_rhs ...) (goals/operand Γ Operand_rhs))
   (where (Goal_lhs ...) (goals/operand Γ Operand_lhs))]
  )

(define-metafunction formality-mir+Γ
  ;; Computes the `Goals` that have to hold for a `Statement`
  goals/stmt : Γ Statement -> Goals

  [; noop
   (goals/stmt Γ noop)
   ()]

  [; storage-live
   (goals/stmt Γ storage-live)
   ()]

  [; storage-dead
   (goals/stmt Γ storage-dead)
   ()]

  [; assign
   (goals/stmt Γ (assign Place Rvalue))
   ((Ty_rvalue <= Ty_place) Goal_rvalue ...)
   (where Ty_rvalue (typeof/rvalue Γ Rvalue))
   (where (place-ty Ty_place (mut)) (typeof/place Γ Place))
   (where (Goal_rvalue ...) (goals/rvalue Γ Rvalue))]

  ; TODO: set-discr
  )

(define-metafunction formality-mir+Γ
  ;; Computes the `Goals` that have to hold for a `Terminator`
  goals/term : Γ Terminator -> Goals

  [; goto
   (goals/term Γ (goto _))
   ()]

  [; resume
   (goals/term Γ resume)
   ()]

  [; abort
   (goals/term Γ abort)
   ()]

  [; return
   (goals/term Γ return)
   ()]

  [; unreachable
   (goals/term Γ unreachable)
   ()]

  [; drop
   (goals/term Γ (drop _ _))
   ()]

  [; drop-replace
   (goals/term Γ (drop-and-replace Place Operand _))
   ((Ty_operand == Ty_place) Goal_op ...)
   (where Ty_operand (typeof/operand Γ Operand))
   (where (place-ty Ty_place _) (typeof/place Γ Place))
   (where (Goal_op ...) (goals/operand Γ Operand))]

  [; call
   (goals/term Γ (call Operand_fn (Operand_arg ..._n) Place _))
   ((Ty_ret <= Ty_place) (Ty_oparg <= Ty_arg) ... Goal_fn ... Goal_arg ...)
   (where (rigid-ty (fn-ptr _ _) (Ty_arg ..._n Ty_ret)) (typeof/operand Γ Operand_fn))
   (where (Ty_oparg ...) (typeof/operands Γ (Operand_arg ...)))
   (where (place-ty Ty_place (mut)) (typeof/place Γ Place))
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
