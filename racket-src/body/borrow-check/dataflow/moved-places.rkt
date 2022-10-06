#lang racket
(require redex/reduction-semantics
         "../../grammar.rkt"
         "../move-set.rkt"
         )
(provide dataflow-apply-node-moved-places
         update-move-set-from-rvalue
         update-move-set-from-operand
         )


(define-metafunction formality-body
  ;; Compute the effect on the move-set when transitioning from `CfgNode` to `Location`.
  dataflow-apply-node-moved-places : CfgNode Location MoveSet -> MoveSet

  ; Terminators

  [(dataflow-apply-node-moved-places (goto BasicBlockId) _ MoveSet)
   MoveSet
   ]

  [(dataflow-apply-node-moved-places (drop Place [_ ... BasicBlockId _ ...]) (BasicBlockId @ 0) MoveSet)
   ; We consider `Place` to be dropped after this terminator.
   (add-moved-place MoveSet Place)
   ]

  [(dataflow-apply-node-moved-places (drop-and-replace Place [_ ... BasicBlockId _ ...]) (BasicBlockId @ 0) MoveSet)
   MoveSet
   ]

  [(dataflow-apply-node-moved-places (call Operand_fn [Operand_arg ...] Place_dest [BasicBlockId _ ...]) (BasicBlockId @ 0) MoveSet)
   (add-initialized-place MoveSet_move_args Place_dest)
   (where/error MoveSet_move_args (update-move-set-from-operands MoveSet [Operand_fn Operand_arg ...]))
   ]

  [(dataflow-apply-node-moved-places (call Operand_fn [Operand_arg ...] Place_dest [_ BasicBlockId]) (BasicBlockId @ 0) MoveSet)
   (update-move-set-from-operands MoveSet [Operand_fn Operand_arg ...])
   ]

  [(dataflow-apply-node-moved-places (Place = Rvalue) _ MoveSet_0)
   (add-initialized-place MoveSet_1 Place)
   (where/error MoveSet_1 (update-move-set-from-rvalue MoveSet_0 Rvalue))
   ]

  [(dataflow-apply-node-moved-places (set-discriminant Place VariantId) _ MoveSet)
   MoveSet
   ]

  [(dataflow-apply-node-moved-places (storage-live LocalId) _ MoveSet)
   MoveSet
   ]

  [(dataflow-apply-node-moved-places (storage-dead LocalId) _ MoveSet)
   MoveSet
   ]

  [(dataflow-apply-node-moved-places noop _ MoveSet)
   MoveSet
   ]

  [(dataflow-apply-node-moved-places (fake-read Place) _ MoveSet)
   MoveSet
   ]

  )

(define-metafunction formality-body
  ;; Update a `MoveSet` to show the effects of executing a single `Rvalue`.
  update-move-set-from-rvalue : MoveSet Rvalue -> MoveSet

  [(update-move-set-from-rvalue MoveSet (use Operand))
   (update-move-set-from-operand MoveSet Operand)
   ]

  [(update-move-set-from-rvalue MoveSet (repeat Operand Constant))
   (update-move-set-from-operand MoveSet Operand)
   ]

  [(update-move-set-from-rvalue MoveSet (ref Lt MaybeMut Place))
   MoveSet
   ]

  [(update-move-set-from-rvalue MoveSet (addr-of MaybeMut Place))
   MoveSet
   ]

  [(update-move-set-from-rvalue MoveSet (len Place))
   MoveSet
   ]

  [(update-move-set-from-rvalue MoveSet (BinaryOp Operand_l Operand_r))
   (update-move-set-from-operands MoveSet [Operand_l Operand_r])
   ]

  )

(define-metafunction formality-body
  ;; Update a `MoveSet` to show the effects of consuming a single operand
  update-move-set-from-operand : MoveSet Operand -> MoveSet

  [(update-move-set-from-operand MoveSet (copy Place))
   MoveSet
   ]

  [(update-move-set-from-operand MoveSet (move Place))
   (place-set-add MoveSet Place)
   ]

  [(update-move-set-from-operand MoveSet (const Constant))
   MoveSet
   ]

  )

(define-metafunction formality-body
  ;; Update a `MoveSet` to show the effects of consuming a list of operands
  update-move-set-from-operands : MoveSet Operands -> MoveSet

  [(update-move-set-from-operands MoveSet [])
   Operand
   ]

  [(update-move-set-from-operands MoveSet [Operand_0 Operand_1 ...])
   (update-move-set-from-operands (update-move-set-from-operand MoveSet Operand_0)
                                  [Operand_1 ...])
   ]

  )