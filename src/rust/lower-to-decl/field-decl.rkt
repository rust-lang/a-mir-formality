#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../../ty/user-ty.rkt"
         )
(provide lower-to-decl/FieldDecl
         )

(define-metafunction formality-rust
  lower-to-decl/FieldDecl : Rust/FieldDecl -> FieldDecl

  [(lower-to-decl/FieldDecl (FieldId : UserTy))
   (FieldId (user-ty UserTy))]

  )