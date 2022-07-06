#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "associated-ty-decl.rkt"
         "fn-decl.rkt"
         )
(provide lower-to-decl/TraitItem
         )

(define-metafunction formality-rust
  lower-to-decl/TraitItem : Rust/TraitItem -> TraitItem

  [(lower-to-decl/TraitItem Rust/AssociatedTyDecl)
   (lower-to-decl/AssociatedTyDecl Rust/AssociatedTyDecl)]

  [(lower-to-decl/TraitItem Rust/FnDecl)
   (lower-to-decl/FnDecl Rust/FnDecl)]

  )