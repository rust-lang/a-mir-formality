#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         )
(provide lower-to-decl/FnDecl
         )

(define-metafunction formality-rust
  lower-to-decl/FnDecl : Rust/AssociatedTyDecl -> AssociatedTyDecl

  [(lower-to-decl/FnDecl (type AssociatedTyId KindedVarIds : Rust/BoundsClause where [Rust/WhereClause ...]))
   not-yet-implemented
   ]

  )