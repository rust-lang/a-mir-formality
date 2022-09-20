#lang racket
(require redex/reduction-semantics
         (prefix-in logic: "cosld-solve/prove.rkt")
         "solution.rkt"
         "solution-simplify.rkt"
         "grammar.rkt"
         )

(provide logic:prove-top-level-goal/cosld
         solve-top-level-query-goal
         )

(define-judgment-form formality-logic
  ;; Prove the query goal and construct a solution.
  #:mode (solve-top-level-query-goal I I I O)
  #:contract (solve-top-level-query-goal VarIds Env Goal Solution)

  [(logic:prove-top-level-goal/cosld Env Goal Env_out)
   (where/error Solution_0 (extract-solution Env_out VarIds_query))
   (where/error Solution_1 (simplify-solution Solution_0))
   ---------------
   (solve-top-level-query-goal VarIds_query Env Goal Solution_1)
   ]

  [(logic:prove-top-level-goal/cosld Env Goal ambiguous)
   ---------------
   (solve-top-level-query-goal VarIds_query Env Goal ambiguous)
   ]
  )

