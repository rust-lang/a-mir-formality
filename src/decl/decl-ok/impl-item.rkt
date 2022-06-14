#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../where-clauses.rkt"
         "../feature-gate.rkt"
         "../../logic/env.rkt"
         )
(provide impl-item-ok-goal
         )

(define-metafunction formality-decl
  impl-item-ok-goal : TraitItemDecl -> Goal
  )
