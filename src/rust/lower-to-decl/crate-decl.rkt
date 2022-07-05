#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "crate-item-decl.rkt"
         )
(provide lower-to-decl/CrateDecl
         )

(define-metafunction formality-rust
  lower-to-decl/CrateDecl : Rust/CrateDecl -> CrateDecl

  [(lower-to-decl/CrateDecl (crate CrateId (Rust/CrateItemDecl ...)))
   (crate CrateId ((lower-to-decl/CrateItemDecl Rust/CrateItemDecl) ...))
   ]
  )