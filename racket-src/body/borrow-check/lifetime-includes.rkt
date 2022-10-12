#lang racket
(require redex/reduction-semantics
         "../../logic/env.rkt"
         "../../logic/substitution.rkt"
         "../../logic/env-inequalities.rkt"
         "../locations.rkt"
         "../grammar.rkt"
         "../cfg.rkt"
         "transitive-outlives.rkt"
         )
(provide lifetime-includes
         )

(define-metafunction
  formality-body

  ;; True if the lifetime `Lt` is live at the given `Location`.
  ;;
  ;; A lifetime is **live** if it, or some other lifetime derived from it,
  ;; appears in the type of a live variable.
  ;;
  ;; Placeholder lifetimes (and static) are always considered live by this
  ;; definition  because the caller may have live variables that refer to them.
  ;;
  ;; This is invoked by the active-loans analysis to determine when a borrow
  ;; has completed.

  lifetime-includes : Γ Env LivenessAnalysis Lt Location -> boolean

  [(lifetime-includes Γ Env LivenessAnalysis VarId Location)
   #t
   (where #t (env-contains-unmapped-existential-var Env VarId))
   (where [_ ... Lt _ ...] (outlives-transitively Env VarId))
   (where #t (lifetime-includes-1 Γ Env LivenessAnalysis Lt Location))
   ]

  [(lifetime-includes Γ Env LivenessAnalysis VarId Location)
   #f
   (where #t (env-contains-unmapped-existential-var Env VarId))
   ]

  [(lifetime-includes Γ Env LivenessAnalysis VarId Location)
   (lifetime-includes Γ Env LivenessAnalysis (apply-substitution-from-env Env VarId) Location)
   (where #t (env-maps-var Env VarId))
   ]

  [(lifetime-includes Γ Env LivenessAnalysis Lt Location)
   (lifetime-includes-1 Γ Env LivenessAnalysis Lt Location)
   ]
  )

(define-metafunction
  formality-body

  ;; Helper for lifetime-includes that is invoked for each transitively
  ;; outlived lifetime.

  lifetime-includes-1 : Γ Env LivenessAnalysis Lt Location -> boolean

  [(lifetime-includes-1 Γ Env LivenessAnalysis VarId Location)
   (in?/id VarId (lifetime-variables-live-at Γ Env LivenessAnalysis Location))
   (where #t (env-contains-unmapped-existential-var Env VarId))
   ]

  [(lifetime-includes-1 Γ Env LivenessAnalysis VarId Location)
   (lifetime-includes-1 Γ Env LivenessAnalysis (apply-substitution-from-env Env VarId) Location)
   (where #t (env-maps-var Env VarId))
   ]

  [(lifetime-includes-1 Γ Env LivenessAnalysis VarId Location)
   #t
   (where #t (env-contains-placeholder-var Env VarId))
   ]

  [(lifetime-includes-1 Γ Env LivenessAnalysis static Location)
   #t
   ]

  )

(define-metafunction formality-body

  ;; Returns the existential lifetime variables that appear in live variables
  ;; at a particular point.

  lifetime-variables-live-at : Γ Env LivenessAnalysis Location -> VarIds

  [(lifetime-variables-live-at Γ Env LivenessAnalysis Location)
   VarIds
   ; FIXME: We should be treating drop-live in a more limited way
   (where/error [LocalId ...] (union-sets
                               (variables-live-on-entry-to use-live LivenessAnalysis Location)
                               (variables-live-on-entry-to drop-live LivenessAnalysis Location)))
   (where/error [Ty ...] [(local-ty Γ LocalId) ...])
   (where/error VarIds (union-sets (free-existential-variables-of-kind Env lifetime Ty) ...))
   ]

  )

(define-metafunction formality-body

  ;; Returns the set of local variables live on entry to a particular
  ;; point for the given liveness-mode.

  variables-live-on-entry-to : LivenessMode LivenessAnalysis Location -> LocalIds

  [(variables-live-on-entry-to use-live LivenessAnalysis Location)
   LocalIds_live
   (where/error (reads: [_ ... (Location LocalIds_live) _ ...] drops: _) LivenessAnalysis)
   ]

  [(variables-live-on-entry-to drop-live LivenessAnalysis Location)
   LocalIds_live
   (where/error (reads: _ drops: [_ ... (Location LocalIds_live) _ ...]) LivenessAnalysis)
   ]
  )