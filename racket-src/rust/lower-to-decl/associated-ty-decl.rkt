#lang racket
(require redex/reduction-semantics
         "../../logic/env.rkt"
         "../grammar.rkt"
         "bounds-clause.rkt"
         "where-clause.rkt"
         )
(provide lower-to-decl/AssociatedTyDecl
         )

(define-metafunction formality-rust
  lower-to-decl/AssociatedTyDecl : KindedVarIds_trait Rust/AssociatedTyDecl -> AssociatedTyDecl

  [(lower-to-decl/AssociatedTyDecl KindedVarIds_trait (type AssociatedTyId KindedVarIds : Rust/BoundsClause where [Rust/WhereClause ...]))
   (type AssociatedTyId KindedVarIds
         (lower-to-decl/BoundsClause KindedVarIds_all Rust/BoundsClause)
         where [(lower-to-decl/WhereClause KindedVarIds_all Rust/WhereClause) ...])

   (where/error KindedVarIds_all (flatten [KindedVarIds_trait KindedVarIds]))]

  )