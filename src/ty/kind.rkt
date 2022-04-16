#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../logic/env.rkt"
         )
(provide parameter-kind
         )

(define-metafunction formality-ty
  ;; Returns the `ParameterKind` for a `Parameter`.
  parameter-kind : Env Parameter -> ParameterKind

  ; The VarId syntactic form is shared by types and lifetimes
  [(parameter-kind Env VarId)
   ParameterKind
   (where/error (ParameterKind _ _) (var-binding-in-env Env VarId))]

  ; The others are not, so we can just test via patterns
  [(parameter-kind Env Lifetime) LtKind]
  [(parameter-kind Env Ty) TyKind]
  )
