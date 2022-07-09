#lang racket
(require redex/reduction-semantics
         "../../logic/env.rkt"
         "../grammar.rkt"
         "crate-item-decl.rkt"
         )
(provide lower-to-decl/CrateDecl
         )

(define-metafunction formality-rust
  ;; Lower an entire crate to the decl layer.
  lower-to-decl/CrateDecl : Rust/CrateDecl -> CrateDecl

  [(lower-to-decl/CrateDecl (crate CrateId (Rust/CrateItemDecl ...)))
   (crate CrateId ((lower-to-decl/CrateItemDecl FeatureIds Rust/CrateItemDecl) ...))
   (where/error FeatureIds (feature-gates-in-crate-decl (Rust/CrateItemDecl ...)))
   ]
  )

(define-metafunction formality-rust
  feature-gates-in-crate-decl : (Rust/CrateItemDecl ...) -> FeatureIds

  [(feature-gates-in-crate-decl (Rust/CrateItemDecl ...))
   (flatten [(feature-gates-in-crate-item-decl Rust/CrateItemDecl) ...])]
  )

(define-metafunction formality-rust
  feature-gates-in-crate-item-decl : Rust/CrateItemDecl -> FeatureIds

  [(feature-gates-in-crate-item-decl (feature FeatureId))
   [FeatureId]]

  [(feature-gates-in-crate-item-decl _)
   []]
  )
