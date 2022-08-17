#lang racket
(require redex/reduction-semantics
         "../decl/grammar.rkt"
         )
(provide (all-defined-out))

(define-extended-language formality-body formality-decl
  ; Overridden from formality-decl
  (FnBody ::=
          (∃ KindedVarIds LocalsAndBlocks)
          {trusted-fn-body} ; a special tag that is always considered to pass the type check
          )
  (LocalsAndBlocks ::= (LocalDecls BasicBlockDecls))

  ;; A `LocalDecl` indicates the type of a local variable.
  (LocalDecls ::= (LocalDecl ...))
  (LocalDecl ::= (LocalId Ty MaybeMut))

  ;; A `BasicBlockDecl` declares the data for a basic block: its statements, terminator, etc.
  (BasicBlockDecls ::= (BasicBlockDecl ...))
  (BasicBlockDecl ::= (BasicBlockId BasicBlockData))
  (BasicBlockData ::= (Statements Terminator))

  ;; A MIR statement is a single executiable unit within a basic block.
  (Statements ::= (Statement ...))
  (Statement ::=
             (Place = Rvalue)
             (set-discriminant Place VariantId)
             (storage-live LocalId)
             (storage-dead LocalId)
             noop
             )

  ;; A Rvalue indicates the set of expressions that can be evaluated into a place.
  (Rvalue ::=
          (use Operand)
          (repeat Operand Constant)
          (ref Lt MaybeMut Place)
          (addr-of MaybeMut Place)
          (len Place)
          (BinaryOp Operand Operand)
          )

  (BinaryOp ::= + - * /)

  ;; A `Terminator` ends a basic block and branches to other blocks.
  (Terminator ::=
              (goto BasicBlockId)
              resume
              abort
              return
              unreachable
              (drop Place TargetIds)
              (drop-and-replace Place Operand TargetIds)
              (call Operand Operands Place TargetIds)
              )
  (TargetIds ::=
             (BasicBlockId) ; unwind not possible
             (BasicBlockId BasicBlockId) ; unwind possible
             )

  ;; An `Operand` is the argument to an rvalue.
  (Operands ::= [Operand ...])
  (Operand ::=
           (CopyMove Place)
           (const Constant)
           )

  (CopyMove ::= copy move)

  (Constant ::= number)

  (Places ::= [Place ...])
  (Place ::=
         LocalId
         (* Place)
         (field Place FieldId)
         (index Place LocalId)
         (downcast Place VariantId)
         )

  (Projections ::= (Projection ...))
  (Projection ::=
              *
              (field FieldId)
              (index LocalId)
              (downcast VariantId)
              )

  ;; Internal to type check:
  ;;
  ;; Extension of `Ty` to optionally store a `VariantId`.
  (PlaceTy ::= (place-ty Ty MaybeMut) (place-ty-variant Ty MaybeMut VariantId))

  ;; Internal to type check:
  ;;
  ;; Typing context storing bindings from locals to types and `CrateDecls`.
  (Γ ::= (CrateDecls VarIds_∀ (Tys -> Ty where Biformulas) LocalsAndBlocks))

  ;; Internal to type check:
  ;;
  ;; MaybeVariantId -- either a variant-id or nothing
  (MaybeVariantId ::= () (VariantId))

  ;; Internal to type check:
  ;;
  ;; MirBodySig -- all the information we need when type-checking a MIR function body.
  ;;
  ;; Example, given this Rust function:
  ;;
  ;; ```rust
  ;; fn foo<T, U>(data1: T, data2: U) -> (T, U)
  ;; where
  ;;     T: Debug
  ;; {
  ;;     (data1, data2)
  ;; }
  ;; ```
  ;;
  ;; we would get a `MirBodySig` like
  ;;
  ;; ```
  ;;    (∀ ((type T) (type U)) ((T U) -> (ty-tuple (T U)) where ((T: Debug())) mir))
  ;;        -----------------    - -     ----------------        ------------
  ;;        type-parameters   arguments   return type           where-clauses
  ;; ```
  ;;
  (MirBodySig ::= (∀ KindedVarIds (Tys -> Ty where Biformulas FnBody)))

  ;; Internal to type check:
  ;;
  ;; Location --- identifies a particular statement or terminator within the MIR
  (Location ::= (BasicBlockId @ number))

  ;; Internal to type check:
  ;;
  ;; StatementAtLocation, TerminatorAtLocation -- pair of a statement/terminator with its location
  (StatementAtLocations ::= (StatementAtLocation ...))
  (StatementAtLocation ::= (Location Statement))
  (TerminatorAtLocation ::= (Location Terminator))

  ;; Internal to type check:
  ;;
  ;; GoalAtLocation -- a goal that must hold at a given program location; used by borrow checking
  ;; to implement flow sensitivity
  (GoalAtLocations ::= (GoalAtLocation ...))
  (GoalAtLocation ::= (Location Goal))

  ;; A move set stores a set of places that are moved
  (MoveSet ::= Places)

  ;; Maps from a basic block to a moveset on entry to that block.
  (MoveSetMap ::= [(BasicBlockId MoveSet) ...])

  ;; The move-sets before each statement in a block, along with the final move-set
  ;; (the state before the terminator is executed).
  (MoveSetsForStatements ::= ([(MoveSet Statement) ...] MoveSet))

  ;; Identifies the local variables live before entering a given basic block.
  (LiveVariablesBeforeBlocks ::= [LiveVariablesBeforeBlock ...])
  (LiveVariablesBeforeBlock ::= (BasicBlockId LiveVariables))
  (LiveVariables ::= (reads: LocalIds drops: LocalIds))
  (LivenessEffects ::= (reads: LocalIds drops: LocalIds writes: LocalIds))

  ;; Summarizes one possible effect of a terminator:
  ;;
  ;; If control-flow goes backwards from the given basic block,
  ;; then `LivenessEffects` are applied.
  (TerminatorLivenessEffects ::= (BasicBlockId LivenessEffects))

  ; identifiers of various kinds:
  (LocalIds ::= [LocalId ...])
  (MirId BasicBlockId LocalId ::= variable-not-otherwise-mentioned)
  )


(define-metafunction formality-body
  ;; Returns the `AdtContents` of the ADT with the given `AdtId`.
  decl-of-adt : Γ AdtId -> AdtDecl

  [(decl-of-adt Γ AdtId)
   (adt-with-id (crate-decls-of-Γ CrateDecls) AdtId)
   ]
  )

(define-metafunction formality-body
  ;; Returns the `MirBody` from the environment Γ
  locals-and-blocks-of-Γ : Γ -> LocalsAndBlocks

  [(locals-and-blocks-of-Γ (_ _ _ LocalsAndBlocks)) LocalsAndBlocks]
  )

(define-metafunction formality-body
  ;; Returns the `LocalDecls` from the MIR body of the environment Γ
  local-decls-of-Γ : Γ -> LocalDecls

  [(local-decls-of-Γ Γ)
   LocalDecls
   (where/error (LocalDecls _) (locals-and-blocks-of-Γ Γ))]
  )

(define-metafunction formality-body
  ;; Returns the types of the fn arguments from the environment Γ
  argument-types-of-Γ : Γ -> Tys

  [(argument-types-of-Γ Γ)
   Tys
   (where/error (_ _ (Tys -> Ty where Biformulas) _) Γ)]
  )

(define-metafunction formality-body
  ;; Returns  the fn return type from the environment Γ
  return-type-of-Γ : Γ -> Tys

  [(return-type-of-Γ Γ)
   Ty
   (where/error (_ _ (Tys -> Ty where Biformulas) _) Γ)]
  )

(define-metafunction formality-body
  ;; Returns the `LocalDecls` from the MIR body of the environment Γ
  basic-block-decls-of-Γ : Γ -> BasicBlockDecls

  [(basic-block-decls-of-Γ Γ)
   BasicBlockDecls
   (where/error (_ BasicBlockDecls) (locals-and-blocks-of-Γ Γ))]
  )

(define-metafunction formality-body
  ;; Returns the `LocalDecls` from the MIR body of the environment Γ
  entry-basic-block-id-of-Γ : Γ -> BasicBlockId

  [(entry-basic-block-id-of-Γ Γ)
   BasicBlockId_entry
   (where/error [(BasicBlockId_entry _) BasicBlockDecl ...] (basic-block-decls-of-Γ Γ))]
  )

(define-metafunction formality-body
  ;; Returns the `CrateDecls` from the environment Γ
  crate-decls-of-Γ : Γ -> MirBody

  [(crate-decls-of-Γ (CrateDecls _ _ _)) CrateDecls]
  )
