#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "hook.rkt"
         "../logic/env.rkt"
         )
(provide apply-variance
         variances-for
         )

(define-metafunction formality-ty
  variances-for : Env RigidName -> (Variance ...)

  [(variances-for Env AdtId) (env-adt-variances Env AdtId)]
  [(variances-for Env ScalarId) ()]
  [(variances-for Env (Ref ())) (+ +)]
  [(variances-for Env (Ref (mut))) (+ =)]
  [(variances-for Env (Tuple number_arity)) (repeat-n-times + number_arity)]
  )

(define-metafunction formality-ty
  apply-variance : Variance RelationOp -> RelationOp

  [(apply-variance + RelationOp) RelationOp]
  [(apply-variance = RelationOp) ==]
  [(apply-variance - >=) <=]
  [(apply-variance - <=) >=]
  [(apply-variance - ==) ==]
  )