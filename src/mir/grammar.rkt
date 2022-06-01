#lang racket
(require redex/reduction-semantics "../decl/grammar.rkt")
(provide (all-defined-out))

(define-extended-language formality-mir formality-decl
  (Body ::= (LocalDecls BasicBlockMap))

  (LocalDecls ::= (LocalDecl ...))
  (LocalDecl ::= (LocalId Ty MaybeMut))

  (BasicBlockMap ::= ((BasicBlockId BasicBlockData) ...))
  (BasicBlockData ::= (Statements Terminator))

  (Statements ::= (Statement ...))
  (Statement ::=
             (assign Place Rvalue)
             (set-discriminant Place VariantId)
             (storage-live LocalId)
             (storage-dead LocalId)
             noop
             )

  (Rvalue ::=
          (use Operand)
          (repeat Operand Constant)
          (ref Lt MaybeMut Place)
          (addr-of MaybeMut Place)
          (len Place)
          (BinaryOp Operand Operand)
          )

  (BinaryOp ::= + - * /)

  (Terminator ::=
              (goto BasicBlockId)
              ;(TerminatorSwitchInt Operand SwitchTargets)
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

  ; identifiers of various kinds:
  (BasicBlockId LocalId ::= variable-not-otherwise-mentioned)
  )
