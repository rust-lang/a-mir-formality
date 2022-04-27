#lang racket
(require redex  "../ty/grammar.rkt" "../decl/grammar.rkt")
(provide (all-defined-out))

(define-extended-language formality-mir formality-decl
  (basic-block-map ((basic-block-id basic-block-data) ...))
  (basic-block-data (statements terminator))

  (statements (statement ...))
  (statement
   (statement-assign place rvalue)
   (statement-set-discriminant place VariantId)
   (statement-storage-live local-variable-id)
   (statement-storage-dead local-variable-id)
   statement-nop
   )

  (rvalue
   (rvalue-use operand)
   (rvalue-repeat operand constant)
   (rvalue-ref Lt MaybeMut place)
   (rvalue-addr-of MaybeMut place)
   (rvalue-len place)
   (rvalue-binary-op binary-op operand operand)
   (nullary-op Ty)
   )

  (binary-op + - * /)

  (terminator
   (terminator-goto basic-block-id)
   ;(terminator-switch-int operand switch-targets)
   terminator-resume
   terminator-abort
   terminator-return
   terminator-unreachable
   (terminator-drop place target-ids)
   (terminator-drop-and-replace place operand target-ids)
   (terminator-call operand operands place target-ids)
   )
  (target-ids
   (basic-block-id) ; unwind not possible
   (basic-block-id basic-block-id) ; unwind possible
   )

  (operand
   (operand-copy place)
   (operand-move place)
   (operand-constant constant)
   )

  (constant number)

  (place (local-variable-id projections))
  (projections (projection ...))
  (projection
   projection-deref
   (projection-field FieldId)
   (projection-index local-variable-id)
   (projection-downcast VariantId)
   )

  ; identifiers of various kinds:
  ((basic-block-id
    local-variable-id
    id) variable-not-otherwise-mentioned)
  )
