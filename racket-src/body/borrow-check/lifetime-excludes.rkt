#lang racket
(require redex/reduction-semantics
         "../../logic/env.rkt"
         "../../logic/env-inequalities.rkt"
         "../locations.rkt"
         "../grammar.rkt"
         "transitive-outlives.rkt"
         "liveness.rkt"
         )
(provide lifetime-excludes
         )

(define-judgment-form
  formality-body
  #:mode (lifetime-excludes I I I I)
  #:contract (lifetime-excludes Γ Env Lt Location)

  [(where #t (env-contains-unmapped-existential-var? Env VarId))
   (where [_ ... Lt_t _ ...] (outlives-transitively Env Lt))
   (lifetime-excludes-1 Γ Env Lt_t Location) ...
   ----------------------------------------
   (lifetime-excludes Γ Env VarId Location)
   ]

  )

(define-judgment-form
  formality-body
  #:mode (lifetime-excludes-1 I I I I)
  #:contract (lifetime-excludes-1 LivenessConstraints Env Lt Location)

  [(where #t (env-contains-unmapped-existential-var? Env VarId))
   (where #f (in?/id VarId (lifetime-variables-live-at Γ Env Location)))
   ----------------------------------------
   (lifetime-excludes-1 Γ Env VarId Location)
   ]

  )

(define-metafunction formality-body
  lifetime-variables-live-at : Γ Env Location -> VarIds

  [(lifetime-variables-live-at Γ Env Location VarId)
   VarIds
   (where/error Cfg (control-flow-graph-from-Γ Γ))
   ; FIXME: We should be treating drop-live in a more limited way
   (where/error [LocalId ...] (union-sets
                               (variables-live-on-entry-to use-live Cfg Location)
                               (variables-live-on-entry-to drop-live Cfg Location)))
   (where/error [Ty ...] [(local-ty Γ LocalId) ...])
   (where/error VarIds (union-sets (free-existential-variables-of-kind Env lifetime Ty) ...))
   ]

  )
