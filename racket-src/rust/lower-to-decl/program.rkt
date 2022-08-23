#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "crate-decl.rkt"
         )
(provide lower-to-decl/Program
         )

(define-metafunction formality-rust
  lower-to-decl/Program : Rust/Program -> DeclProgram

  [(lower-to-decl/Program ([Rust/CrateDecl ...] CrateId))
   ([(lower-to-decl/CrateDecl Rust/CrateDecl) ...] CrateId)
   ]
  )