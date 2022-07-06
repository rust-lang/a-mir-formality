#lang racket
(require redex/reduction-semantics
         "grammar.rkt")
(provide crate-has-feature
         if-crate-has-feature
         )

(define-metafunction formality-decl
  ;; Determines if a given crate has declared a given feature gate.
  crate-has-feature : CrateDecls CrateId FeatureId -> boolean

  ((crate-has-feature CrateDecls CrateId FeatureId)
   #t

   (where (_ ... CrateDecl _ ...) CrateDecls)
   (where (crate CrateId (_ ... (feature FeatureId) _ ...)) CrateDecl)
   )

  ((crate-has-feature CrateDecls CrateId FeatureId)
   #f)
  )

(define-metafunction formality-decl
  ;; Returns an empty list unless the crate has the given feature.
  if-crate-has-feature : CrateDecls CrateId FeatureId Term Term -> Term

  ((if-crate-has-feature CrateDecls CrateId FeatureId Term_1 Term_2)
   Term_1
   (side-condition (term (crate-has-feature CrateDecls CrateId FeatureId)))
   )

  ((if-crate-has-feature CrateDecls CrateId FeatureId Term_1 Term_2)
   Term_2
   )
  )
