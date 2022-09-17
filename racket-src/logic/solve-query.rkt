#lang racket
(require redex/reduction-semantics
         racket/set
         "grammar.rkt"
         "env.rkt"
         "cosld-solve.rkt"
         "solution.rkt"
         )
(provide logic:solve-query)

;; Tests for this are in the rust layer.

(define-metafunction formality-logic
  logic:solve-query : Hook QueryGoal -> Solutions

  [(logic:solve-query Hook QueryGoal)
   Solutions

   (where/error (Env_0 Goal) (instantiate-query Hook QueryGoal))
   (where/error VarIds_query (existential-vars-in-env Env_0))
   (where/error [Env_1 ...] ,(judgment-holds (logic:prove-top-level-goal/cosld Env_0 Goal Env_1)
                                             Env_1))
   (where/error Solutions [(extract-solution Env_1 VarIds_query) ...])
   ]

  )