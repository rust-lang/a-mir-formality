#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../where-clauses.rkt"
         "../feature-gate.rkt"
         "../../logic/env.rkt"
         "../../logic/grammar.rkt"
         )
(provide trait-item-ok-goal
         )
(define-metafunction formality-decl
  trait-item-ok-goal : TraitItem -> Goal

  [(trait-item-ok-goal TraitItem)
   true-goal
   ]
  )
