#lang racket
(require redex/reduction-semantics
         "../../logic/env.rkt"
         "../grammar.rkt"
         "move-set.rkt"
         "dataflow/moved-places.rkt"
         "dataflow/liveness.rkt"
         "dataflow/active-loans.rkt"
         )
(provide dataflow
         )

(define-metafunction formality-body
  ;; Generic dataflow propagation function.
  dataflow : DataflowMode Cfg LocatedCfgValues -> LocatedCfgValues

  [(dataflow DataflowMode Cfg LocatedCfgValues)
   LocatedCfgValues
   (where LocatedCfgValues (dataflow-step DataflowMode Cfg LocatedCfgValues))
   ]

  [(dataflow DataflowMode Cfg LocatedCfgValues)
   (dataflow DataflowMode Cfg LocatedCfgValues_1)
   (where LocatedCfgValues_1 (dataflow-step DataflowMode Cfg LocatedCfgValues))
   ]

  )

(define-metafunction formality-body
  dataflow-step : DataflowMode Cfg LocatedCfgValues -> LocatedCfgValues

  [(dataflow-step DataflowMode Cfg LocatedCfgValues)
   (dataflow-apply-edges DataflowMode LocatedCfgNodes CfgEdges LocatedCfgValues)
   (where/error (LocatedCfgNodes CfgEdges) Cfg)
   ]
  )

(define-metafunction formality-body
  ;; Go through each edge in the CFG and propagate value from the entry
  ;; to the exit.
  dataflow-apply-edges : DataflowMode LocatedCfgNodes CfgEdges LocatedCfgValues -> LocatedCfgValues

  [(dataflow-apply-edges DataflowMode LocatedCfgNodes [] LocatedCfgValues)
   LocatedCfgValues
   ]

  [(dataflow-apply-edges DataflowMode LocatedCfgNodes [(Location_a Location_b) CfgEdge ...] LocatedCfgValues)
   (dataflow-apply-edges DataflowMode LocatedCfgNodes [CfgEdge ...] LocatedCfgValues_1)
   (where/error LocatedCfgValues_1 (dataflow-apply-edge DataflowMode LocatedCfgNodes (Location_a Location_b) LocatedCfgValues))
   ]

  )

(define-metafunction formality-body
  ;; Propagate value for a single `A -> B` entry in the CFG. Reads the current value of A,
  ;; applies any diff based on the statement/terminator at that point, and then overwrites value of B.
  dataflow-apply-edge : DataflowMode LocatedCfgNodes CfgEdge LocatedCfgValues -> LocatedCfgValues

  [(dataflow-apply-edge ForwardDataflowMode LocatedCfgNodes (Location_a Location_b) LocatedCfgValues)
   [LocatedCfgValue_before ... (Location_b CfgValue_b1) LocatedCfgValue_after ...]
   (where/error [_ ... (Location_a CfgValue_a) _ ...] LocatedCfgValues)
   (where/error [LocatedCfgValue_before ... (Location_b CfgValue_b) LocatedCfgValue_after ...] LocatedCfgValues)
   (where/error [_ ... (Location_a CfgNode_a) _ ...] LocatedCfgNodes)
   (where/error CfgValue_a1 (dataflow-apply-node ForwardDataflowMode CfgNode_a Location_b CfgValue_a))
   (where/error CfgValue_b1 (dataflow-join ForwardDataflowMode CfgValue_a1 CfgValue_b))
   ]

  [(dataflow-apply-edge ReverseDataflowMode LocatedCfgNodes (Location_a Location_b) LocatedCfgValues)
   [LocatedCfgValue_before ... (Location_a CfgValue_a1) LocatedCfgValue_after ...]
   (where/error [_ ... (Location_b CfgValue_b) _ ...] LocatedCfgValues)
   (where/error [LocatedCfgValue_before ... (Location_a CfgValue_a) LocatedCfgValue_after ...] LocatedCfgValues)
   (where/error [_ ... (Location_a CfgNode_a) _ ...] LocatedCfgNodes)
   (where/error CfgValue_b1 (dataflow-apply-node ReverseDataflowMode CfgNode_a Location_b CfgValue_b))
   (where/error CfgValue_a1 (dataflow-join ReverseDataflowMode CfgValue_b1 CfgValue_a))
   ]

  )

(define-metafunction formality-body
  ;; Computes output value from a given `CfgNode`, when branching to `Location`,
  ;; and starting with `CfgValue`. The `Location` target is only important
  ;; for terminators, since some of them propagate differently depending on the branch.
  dataflow-apply-node : DataflowMode CfgNode Location CfgValue -> CfgValue

  [(dataflow-apply-node moved-places CfgNode Location MoveSet)
   (dataflow-apply-node-moved-places CfgNode Location MoveSet)
   ]

  [(dataflow-apply-node LivenessMode CfgNode Location MoveSet)
   (dataflow-apply-node-liveness LivenessMode CfgNode Location MoveSet)
   ]

  [(dataflow-apply-node (active-loans Γ Env) CfgNode Location LoanSet)
   (dataflow-apply-node-active-loans Γ Env CfgNode Location LoanSet)
   ]

  )


(define-metafunction formality-body
  ;; Combines two cfg values
  dataflow-join : DataflowMode CfgValue_a CfgValue_b -> CfgValue

  [(dataflow-join moved-places MoveSet_a MoveSet_b)
   (union-move-sets MoveSet_a MoveSet_b)
   ]

  [(dataflow-join LivenessMode LocalIds_a LocalIds_b)
   (union-sets LocalIds_a LocalIds_b)
   ]

  [(dataflow-join (active-loans Γ Env) LoanSet_a LoanSet_b)
   (union-sets LoanSet_a LoanSet_b)
   ]
  )
