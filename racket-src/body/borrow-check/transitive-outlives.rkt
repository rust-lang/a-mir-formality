#lang racket
(require redex/reduction-semantics
         "../../logic/env.rkt"
         "../../logic/env-inequalities.rkt"
         "../locations.rkt"
         "../grammar.rkt"
         )
(provide outlives-transitively
         )

(define-metafunction formality-body
  outlives-transitively : Env Lt -> Lts

  [(outlives-transitively Env Lt)
   (outlives-transitively-fix Env [Lt])
   ]

  )

(define-metafunction formality-body
  outlives-transitively-fix : Env Lts -> Lts

  [(outlives-transitively-fix Env Lts)
   Lts
   (where Lts (outlives-transitively-step Env Lts))
   ]

  [(outlives-transitively-fix Env Lts)
   (outlives-transitively-fix Env (outlives-transitively-step Env Lts))
   ]

  )

(define-metafunction formality-body
  outlives-transitively-step : Env Lts -> Lts

  [(outlives-transitively-step Env Lts)
   (union-sets Lts Lts_1 ...)
   (where/error [Lt ...] Lts)
   (where/error [Lts_1 ...] [(outlives-immediately Env Lt) ...])
   ]

  )

(define-metafunction formality-body
  ;; If Lt is an existential variable, returns the lifetimes that it
  ;; outlives (which may themselves be variables).
  outlives-immediately : Env Lt -> Lts

  [(outlives-immediately Env Lt)
   (known-bounds Env VarId -outlives-)
   (where #t (env-contains-unmapped-existential-var? Env VarId))
   ]

  [(outlives-immediately Env static)
   []
   ]

  [(outlives-immediately Env static)
   []
   (where #t (env-contains-placeholder-var Env VarId))
   ]

  )