#lang racket
(require redex/reduction-semantics
         "../../../logic/env.rkt"
         "../../grammar.rkt"
         )
(provide dataflow-apply-node-active-loans
         )


(define-metafunction formality-body
  ;; Compute the effect on the move-set when transitioning from `CfgNode` to `Location`.
  dataflow-apply-node-active-loans : Γ Env CfgNode Location LoanSet -> LoanSet

  [(dataflow-apply-node-active-loans Γ Env (Place_dest = (ref Lt MaybeMut Place)) _ LoanSet)
   (remove-killed-loans LoanSet_1 Place_dest)
   (where/error LoanSet_1 (union-sets LoanSet [(Lt MaybeMut Place)]))
   ]

  [(dataflow-apply-node-active-loans Γ Env (Place = Rvalue) _ LoanSet)
   (remove-killed-loans LoanSet Place)
   ]

  [(dataflow-apply-node-active-loans Γ Env (call Operand_fn [Operand_arg ...] Place_dest [BasicBlockId _ ...]) (BasicBlockId @ 0) MoveSet)
   (remove-killed-loans LoanSet Place_dest)
   ]

  [(dataflow-apply-node-active-loans Γ Env CfgNode _ LoanSet)
   LoanSet
   ]

  )

(define-metafunction formality-body
  remove-killed-loans : LoanSet Place -> LoanSet

  [(remove-killed-loans LoanSet Place)
   LoanSet ; FIXME: killed loans
   ]

  )
