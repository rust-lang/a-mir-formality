#lang racket
(require redex/reduction-semantics
         "../../logic/env.rkt"
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         "where-clause.rkt"
         "in-scope.rkt"
         )
(provide lower-to-decl/FnDecl
         )

(define-metafunction formality-rust
  ;; Lower an fn declaration to the decl layer.
  ;;
  ;; * `FeatureIds` -- features enabled on current crate
  ;; * `KindedVarIds_cx` -- generic parameters in scope in the context in which fn appears
  ;;                        (e.g., trait, impl, etc)
  lower-to-decl/FnDecl : FeatureIds KindedVarIds_cx Rust/FnDecl -> FnDecl

  [(lower-to-decl/FnDecl FeatureIds KindedVarIds_cx (fn FnId KindedVarIds (Rust/Ty_arg ...) -> Rust/Ty_ret where [Rust/WhereClause ...] FnBody))
   (fn FnId KindedVarIds ((user-ty Rust/Ty_arg) ...) -> (user-ty Rust/Ty_ret)
       where [(lower-to-decl/WhereClause (flatten [KindedVarIds_cx KindedVarIds]) Rust/WhereClause) ...
              (default-bound-for-in-scope-parameter FeatureIds (type (user-ty Rust/Ty_arg))) ...
              (default-bound-for-in-scope-parameter FeatureIds (type (user-ty Rust/Ty_ret)))
              ]
       FnBody)
   ]

  )
