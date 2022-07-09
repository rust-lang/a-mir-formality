#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         )
(provide parameter-kind-of-user-parameter
         )

(define-metafunction formality-rust
  ;; Select either `(in-scope KindedParameter)` or `(well-formed KindedParameter)` depending
  ;; on whether the expanded-implied-bounds feature gate is set.
  ;;
  ;; The `well-formed` bound elaborates to the full set of where-clauses. Traditionally,
  ;; for in-scope types, rustc has only elaborated outlives relationships, so the default
  ;; is to geneate `in-scope`, which gives the more limited elaboration.
  parameter-kind-of-user-parameter : KindedVarIds UserParameter -> ParameterKind

  [(parameter-kind-of-user-parameter [_ ... (ParameterKind VarId) _ ...] VarId)
   ParameterKind
   ]

  [(parameter-kind-of-user-parameter _ UserTy)
   type
   ]

  [(parameter-kind-of-user-parameter _ Lt)
   lifetime
   ]

  )
