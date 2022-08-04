#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "move-set.rkt"
         "move-set-map.rkt"
         )
(provide move-sets-for-terminator
         move-sets-for-statements
         update-move-set-from-statement
         update-move-set-from-rvalue
         update-move-set-from-operand
         update-move-set-from-operands
         )

(define-metafunction formality-body
  ;; Computes the move-sets that result from a terminator, returning a list
  ;; of `(BasicBlockId MoveSet)` pairs. Each pair indicates that the
  ;; block `BasicBlockId` must consider the paths in `MoveSet` moved (in addition to, potentially,
  ;; others).
  ;;
  ;; Subtle point: some terminators, notably `Call`, have different effects on different
  ;; blocks, as the destination place is only initialized on the "ok" path and not
  ;; on the panic path.
  move-sets-for-terminator : MoveSet Terminator -> MoveSetMap

  [(move-sets-for-terminator MoveSet (goto BasicBlockId))
   [(BasicBlockId MoveSet)]
   ]

  [(move-sets-for-terminator MoveSet resume)
   []
   ]

  [(move-sets-for-terminator MoveSet abort)
   []
   ]

  [(move-sets-for-terminator MoveSet return)
   []
   ]

  [(move-sets-for-terminator MoveSet unreachable)
   []
   ]

  [(move-sets-for-terminator MoveSet (drop Place [BasicBlockId ...]))
   [(BasicBlockId MoveSet) ...]

   ; We consider `Place` to be dropped after this terminator.
   (where/error MoveSet_drop (add-moved-place MoveSet Place))
   ]

  #;[;; FIXME-- I don't remember what this means
     (move-sets-for-terminator MoveSet (drop-and-replace Place [BasicBlockId ...]))
     [(BasicBlockId MoveSet) ...]

     ; We consider `Place` to be dropped after this terminator.
     (where/error MoveSet_drop (add-moved-place MoveSet Place))
     ]

  [(move-sets-for-terminator MoveSet (call Operand_fn [Operand_arg ...] Place_dest [BasicBlockId_ok]))
   [(BasicBlockId_ok MoveSet_ok)]

   ; This fn cannot panic: first we move out from the operands, then assign to `Place_dest`.
   (where/error MoveSet_panic (update-move-set-from-operands MoveSet [Operand_fn Operand_arg ...]))
   (where/error MoveSet_ok (add-initialized-place MoveSet_panic Place_dest))
   ]

  [(move-sets-for-terminator MoveSet (call Operand_fn [Operand_arg ...] Place_dest [BasicBlockId_ok BasicBlockId_panic]))
   [(BasicBlockId_panic MoveSet_panic)
    (BasicBlockId_ok MoveSet_ok)]

   ; First we move out from the operands. If the fn panics, this is the final state.
   (where/error MoveSet_panic (update-move-set-from-operands MoveSet [Operand_fn Operand_arg ...]))

   ; If it successfully returns, then we initialize `Place_dest` too.
   (where/error MoveSet_ok (add-initialized-place MoveSet_panic Place_dest))
   ]

  )

(define-metafunction formality-body
  ;; Update a `MoveSet` to show the effects of executing a single `Statement`.
  move-sets-for-statements : MoveSet_on-entry Statements -> MoveSetsForStatements

  [(move-sets-for-statements MoveSet_on-entry Statements)
   (accumulate-move-set-for-statements ([] MoveSet_on-entry) Statements)
   ]
  )

(define-metafunction formality-body
  ;; Update a `MoveSet` to show the effects of executing a single `Statement`.
  accumulate-move-set-for-statements : MoveSetsForStatements Statements -> MoveSetsForStatements

  [(accumulate-move-set-for-statements MoveSetsForStatements [])
   MoveSetsForStatements
   ]

  [(accumulate-move-set-for-statements ([(MoveSet_0 Statement_0) ...] MoveSet_1) [Statement_1 Statement_2 ...])
   (accumulate-move-set-for-statements ([(MoveSet_0 Statement_0) ... (MoveSet_1 Statement_1)] MoveSet_2)
                                       [Statement_2 ...])
   (where/error MoveSet_2 (update-move-set-from-statement MoveSet_1 Statement_1))
   ]
  )

(define-metafunction formality-body
  ;; Update a `MoveSet` to show the effects of executing a single `Statement`.
  update-move-set-from-statement : MoveSet Statement -> MoveSet

  [(update-move-set-from-statement MoveSet_0 (Place = Rvalue))
   MoveSet_2
   (where/error MoveSet_1 (update-move-set-from-rvalue MoveSet_0 Rvalue))
   (where/error MoveSet_2 (add-initialized-place MoveSet_1 Place))
   ]

  [(update-move-set-from-statement MoveSet (set-discriminant Place VariantId))
   MoveSet
   ]

  [(update-move-set-from-statement MoveSet (storage-live LocalId))
   MoveSet
   ]

  [(update-move-set-from-statement MoveSet (storage-dead LocalId))
   MoveSet
   ]

  [(update-move-set-from-statement MoveSet noop)
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