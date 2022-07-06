#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../../ty/user-ty.rkt"
         "fn-decl.rkt"
         "where-clause.rkt"
         )
(provide lower-to-decl/ImplItem
         )

(define-metafunction formality-rust
  lower-to-decl/ImplItem : Rust/ImplItem -> ImplItem

  [(lower-to-decl/ImplItem (type AssociatedTyId KindedVarIds = Rust/Ty where [Rust/WhereClause ...]))
   (type AssociatedTyId KindedVarIds = (user-ty Rust/Ty) where [(lower-to-decl/WhereClause Rust/WhereClause) ...])]

  [(lower-to-decl/ImplItem Rust/FnDecl)
   (lower-to-decl/FnDecl Rust/FnDecl)]

  )