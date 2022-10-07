#lang racket
(require redex/reduction-semantics
         "../../logic/env.rkt"
         "../grammar.rkt"
         )
(provide rvalue-actions
         )

(define-metafunction formality-body
  ;; Given an `Rvalue`, returns the operands it will access (in order)
  ;; and the places it will access.
  ;;
  ;;
  rvalue-actions : Rvalue -> RvalueActions

  [(rvalue-actions (use Operand))
   [Operand]
   ]

  [(rvalue-actions (repeat Operand Constant))
   [Operand (const Constant)]
   ]

  [(rvalue-actions (ref Lt MaybeMut Place))
   [(ref MaybeMut Place)]
   ]

  [(rvalue-actions MoveSet (addr-of MaybeMut Place))
   [(ref MaybeMut Place)]
   ]

  [(rvalue-actions MoveSet (len Place))
   [(ref () Place)]
   ]

  [(rvalue-actions MoveSet (BinaryOp Operand_l Operand_r))
   [Operand_l Operand_r]
   ]

  )