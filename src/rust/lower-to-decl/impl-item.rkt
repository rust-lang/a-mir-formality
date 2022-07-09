#lang racket
(require redex/reduction-semantics
         "../../logic/env.rkt"
         "../grammar.rkt"
         "../../ty/user-ty.rkt"
         "fn-decl.rkt"
         "where-clause.rkt"
         )
(provide lower-to-decl/ImplItem
         )

(define-metafunction formality-rust
  ;; Lower an item from an impl to the decl layer.
  ;;
  ;; * `FeatureIds` -- features enabled on current crate
  ;; * `KindedVarIds_impl` -- generic parameters declared on the impl
  lower-to-decl/ImplItem : FeatureIds KindedVarIds_impl Rust/ImplItem -> ImplItem

  [(lower-to-decl/ImplItem _ KindedVarIds_impl (type AssociatedTyId KindedVarIds = Rust/Ty where [Rust/WhereClause ...]))
   (type AssociatedTyId KindedVarIds = (user-ty Rust/Ty)
         where [(lower-to-decl/WhereClause (flatten [KindedVarIds_impl KindedVarIds]) Rust/WhereClause) ...])]

  [(lower-to-decl/ImplItem FeatureIds KindedVarIds_impl Rust/FnDecl)
   (lower-to-decl/FnDecl FeatureIds KindedVarIds_impl Rust/FnDecl)]

  )