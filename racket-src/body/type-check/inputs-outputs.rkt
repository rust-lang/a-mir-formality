#lang racket
(require redex/reduction-semantics
         "../../logic/grammar.rkt"
         "../grammar.rkt"
         "type-of.rkt"
         "statement.rkt"
         "terminator.rkt"
         )
(provide type-check-goal/inputs-and-outputs
         )

;; The MIR defines local variables relating to the return slot and input parameters.
;;
;; Each of these local variables has a type that is independent from the types declared
;; in the signature.
;;
;; This function relates the types from the signature (which will be referencing placeholders
;; and the like) to those local variables.

(define-judgment-form
  formality-body
  #:mode (type-check-goal/inputs-and-outputs I O)
  #:contract (type-check-goal/inputs-and-outputs Γ GoalAtLocations)

  [(where/error (_ _ ((Ty_sigarg ..._n) -> Ty_sigret _ _) _ _) Γ)
   (where/error ((_ Ty_locret _) (_ Ty_locarg _) ..._n _ ...) (local-decls-of-Γ Γ))
   (where/error ((BasicBlockId_first _) _ ...) (basic-block-decls-of-Γ Γ))
   ; TODO: the goals should hold at "all locations"
   (where/error Location (BasicBlockId_first @ 0))
   ; FIXME: check that the where-clauses in signature are well-formed?
   ----------------------------------------
   (type-check-goal/inputs-and-outputs Γ ((Location (Ty_sigret == Ty_locret))
                                          (Location (Ty_sigarg == Ty_locarg)) ...
                                          ))
   ]
  )