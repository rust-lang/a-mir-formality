#lang racket
(require redex/reduction-semantics
         "../../logic/env.rkt"
         "../grammar.rkt"
         )
(provide liveness-constraints-from-live-variables
         )

(define-metafunction formality-body
  ;; Generates a series of liveness constraints from the set of variables that are live at the
  ;; given location. These constraints have the form `(?R -outlives- [Location])` for each lifetime
  ;; `?R` that is live. A lifetime is live if it appears in the type of
  ;; some variable that is live.
  liveness-constraints-from-live-variables : Γ Env Location LiveVariables -> LivenessConstraints

  [(liveness-constraints-from-live-variables Γ Env Location (reads: [LocalId_read ...] drops: [LocalId_drop ...]))
   [LivenessConstraint_r ... ... LivenessConstraint_d ... ...]
   (where/error [[LivenessConstraint_r ...] ...] [(liveness-constraints-from-read-variable Γ Env Location LocalId_read) ...])
   (where/error [[LivenessConstraint_d ...] ...] [(liveness-constraints-from-read-variable Γ Env Location LocalId_drop) ...])
   ]
  )

(define-metafunction formality-body
  ;; Generates the liveness constraints from the fact that `LocalId` is read at `Location`.
  ;; These all have the form `(?R -outlives- [Location])` for each lifetime `?R` that appears
  ;; in the type of `LocalId`.
  ;;
  ;; E.g., if we have `_0: &?1 &?2 u32` and it is read at location `L`, this would create
  ;; the constraint `[?1 -outlives- [L]]` and `[?2 -outlives- [L]]`.
  liveness-constraints-from-read-variable : Γ Env Location LocalId -> LivenessConstraints

  [(liveness-constraints-from-read-variable Γ Env Location LocalId)
   [(VarId_lt -outlives- [Location]) ...]
   (where/error Ty (local-ty Γ LocalId))
   (where/error [VarId_lt ...] (free-existential-variables-of-kind Env lifetime Ty))
   ]
  )

(define-metafunction formality-body
  ;; Generates the liveness constraints from the fact that `LocalId` is dropped at `Location`.
  ;; These are a suset of the results of `liveness-constraints-from-read-variable` to account
  ;; for "may dangle" annotations.
  liveness-constraints-from-dropped-variable : Γ Env LocalId -> LivenessConstraints

  [(liveness-constraints-from-dropped-variable Γ LocalId)
   (liveness-constraints-from-read-variable Γ LocalId) ; FIXME -- may_dangle
   ]
  )
