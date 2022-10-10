#lang racket
(require redex/reduction-semantics
         "../../../logic/env.rkt"
         "../../grammar.rkt"
         )
(provide dataflow-apply-node-liveness
         )

(define-metafunction formality-body
  ;; Compute the effect on the move-set when transitioning from `CfgNode` to `Location`.
  dataflow-apply-node-liveness : LivenessMode CfgNode Location LocalIds -> LocalIds

  [(dataflow-apply-node-liveness LivenessMode CfgNode Location LocalIds)
   (union-sets LocalIds_gen (subtract-sets LocalIds LocalIds_kill))
   (where/error LocalIds_kill (written-by-node CfgNode Location))
   (where/error LocalIds_gen (read-by-node LivenessMode CfgNode))
   ]
  )

(define-metafunction formality-body
  read-by-node : LivenessMode CfgNode -> LocalIds

  [(read-by-node use-live CfgNode)
   LocalIds
   (where/error (reads: LocalIds drops: _) (evaluate-node CfgNode))
   ]

  [(read-by-node drop-live CfgNode)
   LocalIds
   (where/error (reads: _ drops: LocalIds ) (evaluate-node CfgNode))
   ]

  )

(define-metafunction formality-body
  ;; Returns list of local-ids that are overwritten entirely by
  ;; `CfgNode` when it transitions to its successor `Location`.
  written-by-node : CfgNode Location -> LocalIds

  ; Calls only overwrite when passing to their first successor (the "ok" path).
  [(written-by-node (call Operand_fn [Operand_arg ...] Place_dest [BasicBlockId _ ...]) (BasicBlockId @ 0))
   (written-by-place Place_dest)
   ]

  ; Assignment statements are unconditional.
  [(written-by-node (Place = Rvalue) _)
   (written-by-place Place)
   ]

  [(written-by-node CfgNode Location)
   []
   ]

  )

(define-metafunction formality-body
  written-by-place : Place -> LocalIds

  ; writing to `x = foo` overwrites `x` entirely
  [(written-by-place LocalId)
   [LocalId]
   ]

  ; writing to `x.f = foo` means `x` is still live
  [(written-by-place CompoundPlace)
   []
   ]

  )

(define-metafunction formality-body
  evaluate-node : CfgNode -> LivenessEffects

  [(evaluate-node (goto BasicBlockId))
   (reads: [] drops: [])
   ]

  [(evaluate-node resume)
   (reads: [] drops: [])
   ]

  [(evaluate-node abort)
   (reads: [] drops: [])
   ]

  [(evaluate-node return)
   (reads: [] drops: [])
   ]

  [(evaluate-node unreachable)
   (reads: [] drops: [])
   ]

  [(evaluate-node (drop Place [BasicBlockId ...]))
   (drop-place Place)
   ]

  [(evaluate-node (call Operand_fn [Operand_arg ...] Place_dest TargetIds))
   (union-liveness-effects (evaluate-operand Operand_fn)
                           (evaluate-operand Operand_arg) ...
                           )
   ]

  [(evaluate-node (assert Operand boolean TargetIds))
   (evaluate-operand Operand)
   ]

  [(evaluate-node (switch-int Operand Ty SwitchTargets OtherwiseTarget))
   (evaluate-operand Operand)
   ]

  [(evaluate-node (Place = Rvalue))
   (union-liveness-effects (evaluate-rvalue Rvalue)
                           (write-place Place))
   ]

  [(evaluate-node (set-discriminant Place VariantId))
   (read-place Place)
   ]

  [(evaluate-node (storage-live LocalId))
   (reads: [] drops: [])
   ]

  [(evaluate-node (storage-dead LocalId))
   (reads: [] drops: [])
   ]

  [(evaluate-node noop)
   (reads: [] drops: [])
   ]

  [(evaluate-node (fake-read Place))
   (read-place Place)
   ]

  )

(define-metafunction formality-body
  evaluate-rvalue : Rvalue -> LivenessEffects

  [(evaluate-rvalue (use Operand))
   (evaluate-operand Operand)
   ]

  [(evaluate-rvalue (repeat Operand Constant))
   (evaluate-operand Operand)
   ]

  [(evaluate-rvalue (ref Lt MaybeMut Place))
   (read-place Place)
   ]

  [(evaluate-rvalue (addr-of MaybeMut Place))
   (read-place Place)
   ]

  [(evaluate-rvalue (len Place))
   (read-place Place)
   ]

  [(evaluate-rvalue (BinaryOp Operand_l Operand_r))
   (union-liveness-effects (evaluate-operand Operand_l)
                           (evaluate-operand Operand_r))
   ]

  )

(define-metafunction formality-body
  evaluate-operand : Operand -> LivenessEffects

  [(evaluate-operand (CopyMove Place))
   (read-place Place)
   ]

  [(evaluate-operand (const _))
   (reads: [] drops: [])
   ]
  )

(define-metafunction formality-body
  read-place : Place -> LivenessEffects

  [(read-place LocalId)
   (reads: [LocalId] drops: [])
   ]

  [(read-place (* Place))
   (read-place Place)
   ]

  [(read-place (field Place FieldName))
   (read-place Place)
   ]

  [(read-place (index Place LocalId_index))
   (union-liveness-effects (read-place Place)
                           (reads: [LocalId_index] drops: []))
   ]

  [(read-place (downcast Place VariantId))
   (read-place Place)
   ]
  )

(define-metafunction formality-body
  drop-place : Place -> LivenessEffects

  [(drop-place LocalId)
   (reads: [] drops: [LocalId])
   ]

  [(drop-place (* Place))
   (drop-place Place)
   ]

  [(drop-place (field Place FieldName))
   (drop-place Place)
   ]

  [(drop-place (index Place LocalId_index))
   (union-liveness-effects (drop-place Place)
                           (reads: [LocalId_index] drops: []))
   ]

  [(drop-place (downcast Place VariantId))
   (drop-place Place)
   ]
  )

(define-metafunction formality-body
  write-place : Place -> LivenessEffects

  [(write-place LocalId)
   (reads: [] drops: [])
   ]

  [(write-place Place)
   (read-place Place)
   ]
  )


(define-metafunction formality-body
  union-liveness-effects : LivenessEffects ... -> LivenessEffects

  [(union-liveness-effects LivenessEffects ...)
   (reads: (union-sets LocalIds_r ...) drops: (union-sets LocalIds_d ...))
   (where/error [(reads: LocalIds_r drops: LocalIds_d) ...] [LivenessEffects ...])
   ]

  )
