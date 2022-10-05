#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "initialization-update.rkt"
         "move-set.rkt"
         )
(provide initialization-check
         )

(define-judgment-form
  formality-body
  ;; Check that the function body only uses initialized data, given the set `MoveSetMap` of
  ;; places moved on entry to each block.
  #:mode (initialization-check I I)
  #:contract (initialization-check Γ MoveSetMap)

  [(where/error [BasicBlockDecl ...] (basic-block-decls-of-Γ Γ))

   ; each block and its successors must be consistent with `MoveSetMap`
   (initialization-check-block MoveSetMap BasicBlockDecl) ...

   ; the entry block must consider all local variables as moved
   (initialization-check-entry Γ MoveSetMap)
   ----------------------------------------
   (initialization-check Γ MoveSetMap)
   ]
  )


(define-judgment-form
  formality-body
  ;; Check that the entry block doesn't assume anything is initialized on entry.
  #:mode (initialization-check-entry I I)
  #:contract (initialization-check-entry Γ MoveSetMap)

  [(where/error BasicBlockId_entry (entry-basic-block-id-of-Γ Γ))
   (initialization-check-successor MoveSetMap BasicBlockId_entry (initial-move-set Γ))
   ----------------------------------------
   (initialization-check-entry Γ MoveSetMap)
   ]
  )

(define-judgment-form
  formality-body
  ;; Check that the block only uses initialized data, given the set `MoveSetMap` of
  ;; places moved on entry to each block. Also checks that the moved-on-entry sets
  ;; for the successors of this block account for moves performed during this block.
  #:mode (initialization-check-block I I)
  #:contract (initialization-check-block MoveSetMap BasicBlockDecl)

  [(where/error [_ ... (BasicBlockId MoveSet) _ ...] MoveSetMap)
   (initialization-check-block-data MoveSet BasicBlockData [(BasicBlockId_succ MoveSet_succ) ...])
   (initialization-check-successor MoveSetMap BasicBlockId_succ MoveSet_succ) ...
   ----------------------------------------
   (initialization-check-block MoveSetMap (BasicBlockId BasicBlockData))
   ]
  )

(define-judgment-form
  formality-body
  ;; Check that the move-set on entry to `BasicBlockId` lists
  ;; each place in `Places` as moved.
  #:mode (initialization-check-successor I I I)
  #:contract (initialization-check-successor MoveSetMap BasicBlockId Places)

  [(where/error [_ ... (BasicBlockId MoveSet_block) _ ...] MoveSetMap)
   (place-fully-moved MoveSet_block Place_moved) ...
   ----------------------------------------
   (initialization-check-successor MoveSetMap BasicBlockId [Place_moved ...])
   ]
  )
(define-judgment-form
  formality-body
  ;; Check that each piece of data accessed in the block body is fully initialized.
  ;; Output a `MoveSetMap` containing places that must be marked as moved in the successors.
  ;;
  ;; `MoveSet` contains the places that are moved on entry to the block.
  #:mode (initialization-check-block-data I I O)
  #:contract (initialization-check-block-data MoveSet BasicBlockData MoveSetMap_succ)

  [(where/error ([(MoveSet_statement Statement) ...] MoveSet_terminator)
                (move-sets-for-statements MoveSet_on-entry Statements))
   (initialization-check-statement MoveSet_statement Statement) ...
   (initialization-check-terminator MoveSet_terminator Terminator)
   (where/error MoveSetMap_succ (move-sets-for-terminator MoveSet_terminator Terminator))
   ----------------------------------------
   (initialization-check-block-data MoveSet_on-entry (Statements Terminator) MoveSetMap_succ)
   ]
  )

(define-judgment-form
  formality-body
  ;; Check that each piece of data accessed in `Terminator` is initialized.
  ;;
  ;; `MoveSet` contains the places that are moved on entry to the terminator.
  #:mode (initialization-check-terminator I I)
  #:contract (initialization-check-terminator MoveSet Terminator)

  [----------------------------------------
   (initialization-check-terminator MoveSet (goto BasicBlockId))
   ]

  [----------------------------------------
   (initialization-check-terminator MoveSet resume)
   ]

  [----------------------------------------
   (initialization-check-terminator MoveSet abort)
   ]

  [----------------------------------------
   (initialization-check-terminator MoveSet return)
   ]

  [----------------------------------------
   (initialization-check-terminator MoveSet unreachable)
   ]

  [----------------------------------------
   (initialization-check-terminator MoveSet (drop Place TargetIds))
   ]

  [----------------------------------------
   (initialization-check-terminator MoveSet (drop-and-replace Place TargetIds))
   ]

  [(initialization-check-operands MoveSet [Operand_f Operand_a ...])
   ----------------------------------------
   (initialization-check-terminator MoveSet (call Operand_f [Operand_a ...] Place TargetIds))
   ]

  )

(define-judgment-form
  formality-body
  ;; Check that each piece of data accessed in `Statement` is initialized.
  ;;
  ;; `MoveSet` contains the places that are moved on entry to the statement.
  #:mode (initialization-check-statement I I)
  #:contract (initialization-check-statement MoveSet Statement)

  [(initialization-check-rvalue MoveSet Rvalue)
   (where/error MoveSet_rvalue (update-move-set-from-rvalue MoveSet Rvalue))
   (place-assignable MoveSet_rvalue Place)
   ----------------------------------------
   (initialization-check-statement MoveSet (Place = Rvalue))
   ]

  [(place-fully-initialized MoveSet Place)
   ----------------------------------------
   (initialization-check-statement MoveSet (set-discriminant Place VariantId))
   ]

  [; FIXME: What, if any, conditions should we check for storage-live?
   ----------------------------------------
   (initialization-check-statement MoveSet (storage-live LocalId))
   ]

  [; FIXME: What, if any, conditions should we check for storage-dead?
   ----------------------------------------
   (initialization-check-statement MoveSet (storage-dead LocalId))
   ]

  [; FIXME: What, if any, conditions should we check for storage-dead?
   ----------------------------------------
   (initialization-check-statement MoveSet noop)
   ]

  [(place-fully-initialized MoveSet Place)
   ----------------------------------------
   (initialization-check-statement MoveSet (fake-read Place))
   ]
  )

(define-judgment-form
  formality-body
  ;; Check that each piece of data accessed in `Rvalue` is initialized.
  ;;
  ;; `MoveSet` contains the places that are moved on entry to the rvalue.
  #:mode (initialization-check-rvalue I I)
  #:contract (initialization-check-rvalue MoveSet Rvalue)

  [(initialization-check-operand MoveSet Operand)
   ----------------------------------------
   (initialization-check-rvalue MoveSet (use Operand))
   ]

  [(initialization-check-operand MoveSet Operand)
   ----------------------------------------
   (initialization-check-rvalue MoveSet (repeat Operand Constant))
   ]

  [(place-fully-initialized MoveSet Place)
   ----------------------------------------
   (initialization-check-rvalue MoveSet (ref Lt MaybeMut Place))
   ]

  [(place-fully-initialized MoveSet Place)
   ----------------------------------------
   (initialization-check-rvalue MoveSet (addr-of MaybeMut Place))
   ]

  [(place-fully-initialized MoveSet Place)
   ----------------------------------------
   (initialization-check-rvalue MoveSet (len Place))
   ]

  [(initialization-check-operands MoveSet [Operand_l Operand_r])
   ----------------------------------------
   (initialization-check-rvalue MoveSet (BinaryOp Operand_l Operand_r))
   ]
  )

(define-judgment-form
  formality-body
  ;; Check that each piece of data accessed in `Operands` is initialized;
  ;; the operands are assumed to be evaluated in order, so it's an error if one
  ;; of them moves data that is then used by a later operand.
  ;;
  ;; `MoveSet` contains the places that are moved on entry to the operands.
  #:mode (initialization-check-operands I I)
  #:contract (initialization-check-operands MoveSet Operands)

  [
   ----------------------------------------
   (initialization-check-operands MoveSet [])
   ]

  [(initialization-check-operand MoveSet_0 Operand_0)
   (where/error MoveSet_1 (update-move-set-from-operand MoveSet_0 Operand_0))
   (initialization-check-operands MoveSet_1 [Operand_1 ...])
   ----------------------------------------
   (initialization-check-operands MoveSet_0 [Operand_0 Operand_1 ...])
   ]
  )

(define-judgment-form
  formality-body
  ;; Check that each piece of data accessed in `Operand` is initialized.
  ;;
  ;; `MoveSet` contains the places that are moved on entry to the operand.
  #:mode (initialization-check-operand I I)
  #:contract (initialization-check-operand MoveSet Operand)

  [(place-fully-initialized MoveSet Place)
   ----------------------------------------
   (initialization-check-operand MoveSet (CopyMove Place))
   ]

  [----------------------------------------
   (initialization-check-operand MoveSet (const Constant))
   ]
  )

(define-judgment-form
  formality-body
  ;; Check that the given place is fully initialized, given the set of places `MoveSet`
  ;; that have been moved.
  #:mode (place-fully-initialized I I)
  #:contract (place-fully-initialized MoveSet Place)

  [(where #t (place-is-fully-initialized? MoveSet Place))
   ----------------------------------------
   (place-fully-initialized MoveSet Place)
   ]
  )

(define-judgment-form
  formality-body
  ;; Check that the given place is fully moved, given the set of places `MoveSet`
  ;; that have been moved.
  #:mode (place-fully-moved I I)
  #:contract (place-fully-moved MoveSet Place)

  [(where #t (place-is-fully-moved? MoveSet Place))
   ----------------------------------------
   (place-fully-moved MoveSet Place)
   ]
  )

(define-judgment-form
  formality-body
  ;; Check that the given place can be assigned, given the set of places `MoveSet`
  ;; that have been moved.
  ;;
  ;; Rust does not permit `x.f = y` if `x` is not initialized.
  #:mode (place-assignable I I)
  #:contract (place-assignable MoveSet Place)

  [
   ----------------------------------------
   (place-assignable _ LocalId)
   ]

  [(; is this right? seems too strict
    place-fully-initialized MoveSet Place)
   ----------------------------------------
   (place-assignable MoveSet (* Place))
   ]

  [(; is this right? seems too strict
    place-fully-initialized MoveSet Place)
   ----------------------------------------
   (place-assignable MoveSet (field Place FieldId))
   ]


  [(; is this right? seems too strict
    place-fully-initialized MoveSet Place)
   (place-fully-initialized MoveSet LocalId)
   ----------------------------------------
   (place-assignable MoveSet (index Place LocalId))
   ]
  )
