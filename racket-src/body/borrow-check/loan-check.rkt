#lang racket
(require redex/reduction-semantics
         "../../logic/cosld-solve.rkt" ; FIXME: should be using queries
         "../grammar.rkt"
         "../cfg.rkt"
         "active-loans.rkt"
         "lifetime-includes.rkt"
         "rvalues.rkt"
         "places.rkt"
         )
(provide loan-check
         )

(define-judgment-form
  formality-body
  ;; Check that the function body only uses initialized data, given the set `MoveSetMap` of
  ;; places moved on entry to each block.
  #:mode (loan-check I I I)
  #:contract (loan-check Γ Env GoalAtLocations)

  [(where/error [(Location Goal) ...] GoalAtLocations)
   (logic:prove-top-level-goal/cosld Env_in (&& [Goal ...]) Env_s)
   (where/error ([LocatedCfgNode ...] _) (control-flow-graph-from-Γ Γ))
   (loan-check-located-cfg-node Γ Env_s LocatedCfgNode) ...
   ----------------------------------------
   (loan-check Γ Env_in GoalAtLocations)
   ]
  )

(define-judgment-form
  formality-body
  ;; Check that the block only uses initialized data, given the set `MoveSetMap` of
  ;; places moved on entry to each block. Also checks that the moved-on-entry sets
  ;; for the successors of this block account for moves performed during this block.
  #:mode (loan-check-located-cfg-node I I I)
  #:contract (loan-check-located-cfg-node Γ Env LocatedCfgNode)

  [----------------------------------------
   (loan-check-located-cfg-node Γ Env (Location (goto BasicBlockId)))
   ]

  [----------------------------------------
   (loan-check-located-cfg-node Γ Env (Location resume))
   ]

  [----------------------------------------
   (loan-check-located-cfg-node Γ Env (Location abort))
   ]

  [----------------------------------------
   (loan-check-located-cfg-node Γ Env (Location return))
   ]

  [----------------------------------------
   (loan-check-located-cfg-node Γ Env (Location unreachable))
   ]

  [(loan-check-place-assigned Γ Env Location Place)
   ----------------------------------------
   (loan-check-located-cfg-node Γ Env (Location (drop Place TargetIds)))
   ]

  [(loan-check-place-assigned Γ Env Location Place)
   ----------------------------------------
   (loan-check-located-cfg-node Γ Env (Location (drop-and-replace Place TargetIds)))
   ]

  [(loan-check-action Γ Env Location Operand_f)
   (loan-check-action Γ Env Location Operand_a) ...
   (loan-check-place-assigned Γ Env Location Place)
   ----------------------------------------
   (loan-check-located-cfg-node Γ Env (Location (call Operand_f [Operand_a ...] Place TargetIds)))
   ]

  [(loan-check-action Γ Env Location Operand)
   ----------------------------------------
   (loan-check-located-cfg-node Γ Env (Location (assert Operand boolean TargetIds)))
   ]

  [(loan-check-action Γ Env Location Operand)
   ----------------------------------------
   (loan-check-located-cfg-node Γ Env (Location (switch-int Operand Ty SwitchTargets OtherwiseTarget)))
   ]

  [(loan-check-rvalue Γ Env Location Rvalue)
   (loan-check-place-assigned Γ Env Location Place)
   ----------------------------------------
   (loan-check-located-cfg-node Γ Env (Location (Place = Rvalue)))
   ]

  [(loan-check-place-assigned Γ Env Location Place)
   ----------------------------------------
   (loan-check-located-cfg-node Γ Env (Location (set-discriminant Place VariantId)))
   ]

  [
   ----------------------------------------
   (loan-check-located-cfg-node Γ Env (Location (storage-live LocalId)))
   ]

  [(loan-check-place-access Γ Env Location storage-dead LocalId)
   ----------------------------------------
   (loan-check-located-cfg-node Γ Env (Location (storage-dead LocalId)))
   ]

  [----------------------------------------
   (loan-check-located-cfg-node Γ Env (Location noop))
   ]

  [(loan-check-place-read Γ Env Location Place)
   ----------------------------------------
   (loan-check-located-cfg-node Γ Env (Location (fake-read Place)))
   ]

  )

(define-judgment-form
  formality-body
  #:mode (loan-check-rvalue I I I I)
  #:contract (loan-check-rvalue Γ Env Location Rvalue)

  [(where [RvalueAction ...] (rvalue-actions Rvalue))
   (loan-check-action Γ Env Location RvalueAction) ...
   ----------------------------------------
   (loan-check-rvalue Γ Env Location Rvalue)
   ]
  )

(define-judgment-form
  formality-body
  #:mode (loan-check-action I I I I)
  #:contract (loan-check-action Γ Env Location RvalueAction)

  [(loan-check-place-read Γ Env Location Place)
   ----------------------------------------
   (loan-check-action Γ Env Location (copy Place))]

  [(loan-check-place-assigned Γ Env Location Place)
   ----------------------------------------
   (loan-check-action Γ Env Location (move Place))]

  [(loan-check-place-read Γ Env Location Place)
   ----------------------------------------
   (loan-check-action Γ Env Location (ref () Place))]

  [(loan-check-place-assigned Γ Env Location Place)
   ----------------------------------------
   (loan-check-action Γ Env Location (ref mut Place))]

  [----------------------------------------
   (loan-check-action Γ Env Location (const _))]

  )

(define-judgment-form
  formality-body
  #:mode (loan-check-place-read I I I I)
  #:contract (loan-check-place-read Γ Env Location Place)

  [(loan-check-place-access Γ Env Location read-place Place)
   ----------------------------------------
   (loan-check-place-read Γ Env Location Place)
   ]

  )

(define-judgment-form
  formality-body
  #:mode (loan-check-place-assigned I I I I)
  #:contract (loan-check-place-assigned Γ Env Location Place)

  [(loan-check-place-access Γ Env Location write-place Place)
   ----------------------------------------
   (loan-check-place-assigned Γ Env Location Place)
   ]
  )

(define-judgment-form
  formality-body
  #:mode (loan-check-place-access I I I I I)
  #:contract (loan-check-place-access Γ Env Location PlaceAccess Place)

  [
   (where/error [Loan ...] (loans-active-on-entry-to Γ Env Location))
   (loan-check-place-access-against-loan Γ Env Location PlaceAccess Place Loan) ...
   ----------------------------------------
   (loan-check-place-access Γ Env Location PlaceAccess Place)
   ]
  )

(define-judgment-form
  formality-body
  #:mode (loan-check-place-access-against-loan I I I I I I)
  #:contract (loan-check-place-access-against-loan Γ Env Location PlaceAccess Place Loan)

  [; Reading a shared loan? No error.
   ----------------------------------------
   (loan-check-place-access-against-loan Γ Env Location read-place Place (Lt () Place_loaned))
   ]

  [; Accessing distinct places? No error.
   (where #f (places-intersect? Place_accessed Place_loaned))
   ----------------------------------------
   (loan-check-place-access-against-loan Γ Env Location _ Place_accessed (_ _ Place_loaned))
   ]

  [; When killing the stack storage, it's ok to have a borrow if the data was not actually
   ; stored on the stack (this occurs with e.g. a reborrow of an `&mut` stored on stack).
   (where #t (place-is-indirect? Place_loaned))
   ----------------------------------------
   (loan-check-place-access-against-loan Γ Env Location storage-dead LocalId (_ _ Place_loaned))
   ]

  )