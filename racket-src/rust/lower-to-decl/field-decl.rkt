#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../../ty/user-ty.rkt"
         )
(provide lower-to-decl/FieldDecl
         )

(define-metafunction formality-rust
  ;; Lower an field declaration to the decl layer.
  lower-to-decl/FieldDecl : Rust/FieldDecl -> FieldDecl

  [(lower-to-decl/FieldDecl (FieldName : UserTy))
   (FieldName (user-ty UserTy))]

  )