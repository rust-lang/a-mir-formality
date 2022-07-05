#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "bounds-clause.rkt"
         "where-clause.rkt"
         )
(provide lower-to-decl/AssociatedTyDecl
         )

(define-metafunction formality-rust
  lower-to-decl/AssociatedTyDecl : Rust/AssociatedTyDecl -> AssociatedTyDecl

  [(lower-to-decl/AssociatedTyDecl (type AssociatedTyId KindedVarIds : Rust/BoundsClause where [Rust/WhereClause ...]))
   (type AssociatedTyId KindedVarIds
         (lower-to-decl/BoundsClause Rust/BoundsClause)
         where [(lower-to-decl/WhereClause Rust/WhereClause) ...])]

  )