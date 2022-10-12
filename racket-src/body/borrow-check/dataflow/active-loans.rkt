#lang racket
(require redex/reduction-semantics
         "../../../logic/env.rkt"
         "../../grammar.rkt"
         "../lifetime-includes.rkt"
         )
(provide dataflow-apply-node-active-loans
         )

;; Active loans analysis:
;;
;; A loan L becomes *active* when its corresponding `(ref)` statement in the
;; MIR occurs. It becomes *inactive* when
;;
;; * the place that was borrowed is mutated, which means that the place no longer
;;   refers to the same data (note that this mutation may be illegal).
;; * the lifetime `Lt` of the loan no longer includes the current point
;;   (see `lifetime-includes`).

(define-metafunction formality-body
  ;; Compute the effect on the move-set when transitioning from `CfgNode` to `Location`.
  dataflow-apply-node-active-loans : Γ Env LivenessAnalysis CfgNode Location LoanSet -> LoanSet

  [(dataflow-apply-node-active-loans Γ Env LivenessAnalysis (Place_dest = (ref Lt MaybeMut Place)) Location LoanSet)
   (remove-dead-loans Γ Env LivenessAnalysis (remove-killed-loans LoanSet_1 Place_dest) Location)
   (where/error LoanSet_1 (union-sets LoanSet [(Lt MaybeMut Place)]))
   ]

  [(dataflow-apply-node-active-loans Γ Env LivenessAnalysis (Place = Rvalue) Location LoanSet)
   (remove-dead-loans Γ Env LivenessAnalysis (remove-killed-loans LoanSet Place) Location)
   ]

  [(dataflow-apply-node-active-loans Γ Env LivenessAnalysis (call Operand_fn [Operand_arg ...] Place_dest [BasicBlockId _ ...]) (BasicBlockId @ 0) MoveSet)
   (remove-dead-loans Γ Env LivenessAnalysis (remove-killed-loans LoanSet Place_dest) (BasicBlockId @ 0))
   ]

  [(dataflow-apply-node-active-loans Γ Env LivenessAnalysis CfgNode Location LoanSet)
   (remove-dead-loans Γ Env LivenessAnalysis LoanSet Location)
   ]

  )

(define-metafunction formality-body
  remove-killed-loans : LoanSet Place -> LoanSet

  [(remove-killed-loans LoanSet Place)
   LoanSet ; FIXME: killed loans
   ]

  )

(define-metafunction formality-body
  remove-dead-loans : Γ Env LivenessAnalysis LoanSet Location -> LoanSet

  [(remove-dead-loans Γ Env LivenessAnalysis [Loan ...] Location)
   (union-sets (remove-dead-loan Γ Env LivenessAnalysis Loan Location) ...)
   ]

  )

(define-metafunction formality-body
  remove-dead-loan : Γ Env LivenessAnalysis Loan Location -> LoanSet

  [(remove-dead-loan Γ Env LivenessAnalysis (Lt MaybeMut Place) Location)
   [(Lt MaybeMut Place)]
   (where #t (lifetime-includes Γ Env LivenessAnalysis Lt Location))
   ]

  [(remove-dead-loan Γ Env LivenessAnalysis (Lt MaybeMut Place) Location)
   []
   (where #f (lifetime-includes Γ Env LivenessAnalysis Lt Location))
   ]

  )