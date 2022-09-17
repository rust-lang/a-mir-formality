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
  ;; Compute the solutions to the given query and return them.
  logic:solve-query : Hook QueryGoal -> Solutions

  [(logic:solve-query Hook QueryGoal)
   (extract-unique-solutions [] Solutions)

   (where/error (Env_0 Goal) (instantiate-query Hook QueryGoal))
   (where/error VarIds_query (existential-vars-in-env Env_0))
   (where/error Solutions ,(judgment-holds (solve-top-level-query-goal VarIds_query Env_0 Goal Solution)
                                           Solution))
   ]

  )

(define-judgment-form formality-logic
  ;; Prove the query goal and construct a solution.
  #:mode (solve-top-level-query-goal I I I O)
  #:contract (solve-top-level-query-goal VarIds Env Goal Solution)

  [(logic:prove-top-level-goal/cosld Env Goal Env_out)
   (where/error Solution (extract-solution Env_out VarIds_query))
   ---------------
   (solve-top-level-query-goal VarIds_query Env Goal Solution)
   ]
  )

(define-metafunction formality-logic
  ;; Adds `Solutions` to `Solutions_accum`, unless there is already an
  ;; alpha-equivalent solution present.
  extract-unique-solutions : Solutions_accum Solutions -> Solutions

  [(extract-unique-solutions Solutions_accum [])
   Solutions_accum
   ]

  [(extract-unique-solutions Solutions_accum [Solution_0 Solution_1 ...])
   (extract-unique-solutions Solutions_accum [Solution_1 ...])
   (where #t (in? Solution_0 Solutions_accum))
   ]

  [(extract-unique-solutions Solutions_accum [Solution_0 Solution_1 ...])
   (extract-unique-solutions [Solution_accum ... Solution_0] [Solution_1 ...])
   (where #f (in? Solution_0 Solutions_accum))
   (where/error [Solution_accum ...] Solutions_accum)
   ]
  )