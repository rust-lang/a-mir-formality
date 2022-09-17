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
   (extract-unique-solutions VarIds_query [] Envs_1)

   (where/error (Env_0 Goal) (instantiate-query Hook QueryGoal))
   (where/error VarIds_query (existential-vars-in-env Env_0))
   (where/error Envs_1 ,(judgment-holds (logic:prove-top-level-goal/cosld Env_0 Goal Env_1)
                                        Env_1))
   ]

  )

(define-metafunction formality-logic
  ;; Given the variables `VarIds_query` from the query and the final environments `Envs`,
  ;; extract the solution from each of `Envs` and return the complete set.
  ;; Alpha-equivalent duplicates are removed.
  extract-unique-solutions : VarIds_query Solutions Envs -> Solutions

  [(extract-unique-solutions VarIds_query Solutions [])
   Solutions
   ]

  [(extract-unique-solutions VarIds_query Solutions_in [Env_0 Env_1 ...])
   (extract-unique-solutions VarIds_query Solutions_0 [Env_1 ...])
   (where/error Solution_0 (extract-solution Env_0 VarIds_query))
   (where/error Solutions_0 (add-solution Solutions_in Solution_0))
   ]
  )

(define-metafunction formality-logic
  ;; Adds `Solution` to `Solutons`, unless there is already an alpha-equivalent solution present.
  add-solution : Solutions Solution -> Solutions

  [; Already a solution that is alpha-equivalent to `Solution_new`, skip it.
   (add-solution Solutions_old Solution_new)
   Solutions_old
   (where [_ ... Solution_old _ ...] Solutions_old)
   (side-condition (alpha-equivalent? (term Solution_old) (term Solution_new)))
   ]

  [; `Solution_new` looks new, append it to the list
   (add-solution [Solution_old ...] Solution_new)
   [Solution_old ... Solution_new]
   ]

  )