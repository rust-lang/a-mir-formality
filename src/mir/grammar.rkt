#lang racket
(require redex "../decl/grammar.rkt")
(provide (all-defined-out))

(define-extended-language formality-mir formality-decl
  (Body ::= (LocalDecls BasicBlockMap))

  (LocalDecls ::= (LocalDecl ...))
  (LocalDecl ::= (LocalId Ty MaybeMut))

  (BasicBlockMap ::= ((BasicBlockId BasicBlockData) ...))
  (BasicBlockData ::= (Statements Terminator))

  (Statements ::= (Statement ...))
  (Statement ::=
             (StatementAssign Place Rvalue)
             (StatementSetDiscriminant Place VariantId)
             (StatementStorageLive LocalId)
             (StatementStorageDead LocalId)
             StatementNop
             )

  (Rvalue ::=
          (RvalueUse Operand)
          (RvalueRepeat Operand Constant)
          (RvalueRef Lt MaybeMut Place)
          (RvalueAddrOf MaybeMut Place)
          (RvalueLen Place)
          (RvalueBinaryOp BinaryOp Operand Operand)
          (RvalueNullaryOp Ty)
          )

  (BinaryOp ::= + - * /)

  (Terminator ::=
              (TerminatorGoto BasicBlockId)
              ;(TerminatorSwitchInt Operand SwitchTargets)
              TerminatorResume
              TerminatorAbort
              TerminatorReturn
              TerminatorUnreachable
              (TerminatorDrop Place TargetIds)
              (TerminatorDropAndReplace Place Operand TargetIds)
              (TerminatorCall Operand Operands Place TargetIds)
              )
  (TargetIds ::=
             (BasicBlockId) ; unwind not possible
             (BasicBlockId BasicBlockId) ; unwind possible
             )

  (Operand ::=
           (OperandCopy Place)
           (OperandMove Place)
           (OperandConstant Constant)
           )

  (Constant ::= number)

  (Place ::= (LocalId Projections))
  (Projections ::= (Projection ...))
  (Projection ::=
              ProjectionDeref
              (ProjectionField FieldId)
              (ProjectionIndex LocalId)
              (ProjectionDowncast VariantId)
              )

  ; identifiers of various kinds:
  (BasicBlockId LocalId ::= variable-not-otherwise-mentioned)
  )
