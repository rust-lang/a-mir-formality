#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "associated-ty-decl.rkt"
         "fn-decl.rkt"
         )
(provide lower-to-decl/TraitItem
         )

(define-metafunction formality-rust
  ;; Lower an item from an trait to the decl layer.
  ;;
  ;; * `FeatureIds` -- features enabled on current crate
  ;; * `KindedVarIds_trait` -- generic parameters declared on the trait
  lower-to-decl/TraitItem : FeatureIds KindedVarIds_trait Rust/TraitItem -> TraitItem

  [(lower-to-decl/TraitItem _ KindedVarIds_trait Rust/AssociatedTyDecl)
   (lower-to-decl/AssociatedTyDecl KindedVarIds_trait Rust/AssociatedTyDecl)]

  [(lower-to-decl/TraitItem FeatureIds KindedVarIds_trait Rust/FnDecl)
   (lower-to-decl/FnDecl FeatureIds KindedVarIds_trait Rust/FnDecl)]

  )