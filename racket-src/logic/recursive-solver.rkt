#lang racket
(require redex/reduction-semantics
         racket/set
         "grammar.rkt"
         "env.rkt"
         "instantiate.rkt"
         "substitution.rkt"
         )
(provide )

(define-metafunction formality-logic
  recursive-solve : QueryGoal -> Solution

  [(recursive-solve QueryGoal) #f]
  )