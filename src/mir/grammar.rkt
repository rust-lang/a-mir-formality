#lang racket
(require redex/reduction-semantics "../decl/grammar.rkt")
(provide (all-defined-out))

(define-extended-language formality-mir formality-decl
  ; Overridden from formality-decl
  (FnBody ::= (LocalDecls BasicBlockDecls))

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
             (assign Place Rvalue)
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
  (Operand ::=
           (copy Place)
           (move Place)
           (const Constant)
           )

  (Constant ::= number)

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

  ;; Interal to type check:
  ;;
  ;; Typing context storing bindings from locals to types and `CrateDecls`.
  (Γ ::= (LocalDecls CrateDecls))

  ; identifiers of various kinds:
  (MirId BasicBlockId LocalId ::= variable-not-otherwise-mentioned)
  )
