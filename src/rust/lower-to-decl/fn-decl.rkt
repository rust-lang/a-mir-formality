#lang racket
(require redex/reduction-semantics
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         "where-clause.rkt"
         )
(provide lower-to-decl/FnDecl
         )

(define-metafunction formality-rust
  lower-to-decl/FnDecl : Rust/FnDecl -> FnDecl

  [(lower-to-decl/FnDecl (fn FnId KindedVarIds (Rust/Ty_arg ...) -> Rust/Ty_ret where [Rust/WhereClause ...] FnBody))
   (fn FnId KindedVarIds ((user-ty Rust/Ty_arg) ...) -> (user-ty Rust/Ty_ret)
       where [(lower-to-decl/WhereClause Rust/WhereClause) ...
              (in-scope (type (user-ty Rust/Ty_arg))) ...
              (in-scope (type (user-ty Rust/Ty_ret)))
              ]
       FnBody)
   ]

  )