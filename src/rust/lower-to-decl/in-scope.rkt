#lang racket
(require redex/reduction-semantics
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         )
(provide default-bound-for-in-scope-parameter
         )

(define-metafunction formality-rust
  ;; Select either `(in-scope KindedParameter)` or `(well-formed KindedParameter)` depending
  ;; on whether the expanded-implied-bounds feature gate is set.
  ;;
  ;; The `well-formed` bound elaborates to the full set of where-clauses. Traditionally,
  ;; for in-scope types, rustc has only elaborated outlives relationships, so the default
  ;; is to geneate `in-scope`, which gives the more limited elaboration.
  default-bound-for-in-scope-parameter : FeatureIds KindedParameter -> Biformula

  [(default-bound-for-in-scope-parameter [_ ... expanded-implied-bounds _ ...] KindedParameter)
   (well-formed KindedParameter)
   ]

  [(default-bound-for-in-scope-parameter _ KindedParameter)
   (in-scope KindedParameter)
   ]
  )
