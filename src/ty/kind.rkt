#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../logic/env.rkt"
         )
(provide parameter-kind
         parameter-has-type-kind?
         parameter-has-lifetime-kind?
         )

(define-metafunction formality-ty
  ;; Returns the `ParameterKind` for a `Parameter`.
  parameter-kind : Env Parameter -> ParameterKind

  ; The VarId syntactic form is shared by types and lifetimes
  [(parameter-kind Env VarId)
   ParameterKind
   (where/error (ParameterKind _ _) (var-binding-in-env Env VarId))]

  ; The others are not, so we can just test via patterns
  [(parameter-kind Env Lt) lifetime]
  [(parameter-kind Env Ty) type]
  )

(define-metafunction formality-ty
  parameter-has-type-kind? : Env Parameter -> boolean

  [(parameter-has-type-kind? Env Parameter)
   #t
   (where type (parameter-kind Env Parameter))]

  [(parameter-has-type-kind? Env Parameter)
   #f]
  )

(define-metafunction formality-ty
  parameter-has-lifetime-kind? : Env Parameter -> boolean

  [(parameter-has-lifetime-kind? Env Parameter)
   #t
   (where lifetime (parameter-kind Env Parameter))]

  [(parameter-has-lifetime-kind? Env Parameter)
   #f]
  )