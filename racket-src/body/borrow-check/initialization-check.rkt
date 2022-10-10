#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../cfg.rkt"
         "move-set.rkt"
         "dataflow/moved-places.rkt"
         )
(provide initialization-check
         )

(define-judgment-form
  formality-body
  ;; Check that the function body only uses initialized data, given the set `MoveSetMap` of
  ;; places moved on entry to each block.
  #:mode (initialization-check I I)
  #:contract (initialization-check Γ MoveSetMap)

  [(where/error ([LocatedCfgNode ...] _) (control-flow-graph (basic-block-decls-of-Γ Γ)))
   (initialization-check-located-cfg-node MoveSetMap LocatedCfgNode) ...
   ----------------------------------------
   (initialization-check Γ MoveSetMap)
   ]
  )

(define-judgment-form
  formality-body
  ;; Check that the block only uses initialized data, given the set `MoveSetMap` of
  ;; places moved on entry to each block. Also checks that the moved-on-entry sets
  ;; for the successors of this block account for moves performed during this block.
  #:mode (initialization-check-located-cfg-node I I)
  #:contract (initialization-check-located-cfg-node MoveSetMap LocatedCfgNode)

  [; find the move set on entry to this location
   (where/error [_ ... (Location MoveSet) _ ...] MoveSetMap)
   ; check that the cfgnode is correct given that modeset
   (initialization-check-cfg-node MoveSet CfgNode)
   ----------------------------------------
   (initialization-check-located-cfg-node MoveSetMap (Location CfgNode))
   ]
  )

(define-judgment-form
  formality-body
  ;; Check that each piece of data accessed in the block body is fully initialized.
  ;; Output a `MoveSetMap` containing places that must be marked as moved in the successors.
  ;;
  ;; `MoveSet` contains the places that are moved on entry to the block.
  #:mode (initialization-check-cfg-node I I)
  #:contract (initialization-check-cfg-node MoveSet CfgNode)

  [----------------------------------------
   (initialization-check-cfg-node MoveSet (goto BasicBlockId))
   ]

  [----------------------------------------
   (initialization-check-cfg-node MoveSet resume)
   ]

  [----------------------------------------
   (initialization-check-cfg-node MoveSet abort)
   ]

  [----------------------------------------
   (initialization-check-cfg-node MoveSet return)
   ]

  [----------------------------------------
   (initialization-check-cfg-node MoveSet unreachable)
   ]

  [----------------------------------------
   (initialization-check-cfg-node MoveSet (drop Place TargetIds))
   ]

  [----------------------------------------
   (initialization-check-cfg-node MoveSet (drop-and-replace Place TargetIds))
   ]

  [(initialization-check-operands MoveSet [Operand_f Operand_a ...])
   ----------------------------------------
   (initialization-check-cfg-node MoveSet (call Operand_f [Operand_a ...] Place TargetIds))
   ]

  [(initialization-check-operand MoveSet Operand)
   ----------------------------------------
   (initialization-check-cfg-node MoveSet (assert Operand boolean TargetIds))
   ]

  [(initialization-check-operand MoveSet Operand)
   ----------------------------------------
   (initialization-check-cfg-node MoveSet (switch-int Operand Ty SwitchTargets OtherwiseTarget))
   ]

  [(initialization-check-rvalue MoveSet Rvalue)
   (where/error MoveSet_rvalue (update-move-set-from-rvalue MoveSet Rvalue))
   (place-assignable MoveSet_rvalue Place)
   ----------------------------------------
   (initialization-check-cfg-node MoveSet (Place = Rvalue))
   ]

  [(place-fully-initialized MoveSet Place)
   ----------------------------------------
   (initialization-check-cfg-node MoveSet (set-discriminant Place VariantId))
   ]

  [; FIXME: What, if any, conditions should we check for storage-live?
   ----------------------------------------
   (initialization-check-cfg-node MoveSet (storage-live LocalId))
   ]

  [; FIXME: What, if any, conditions should we check for storage-dead?
   ----------------------------------------
   (initialization-check-cfg-node MoveSet (storage-dead LocalId))
   ]

  [; FIXME: What, if any, conditions should we check for storage-dead?
   ----------------------------------------
   (initialization-check-cfg-node MoveSet noop)
   ]

  [(place-fully-initialized MoveSet Place)
   ----------------------------------------
   (initialization-check-cfg-node MoveSet (fake-read Place))
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
   (place-assignable MoveSet (field Place FieldName))
   ]


  [(; is this right? seems too strict
    place-fully-initialized MoveSet Place)
   (place-fully-initialized MoveSet LocalId)
   ----------------------------------------
   (place-assignable MoveSet (index Place LocalId))
   ]
  )
