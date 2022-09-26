#lang racket
(require redex/reduction-semantics
         racket/set
         "grammar.rkt"
         "env.rkt"
         "solution.rkt"
         "substitution.rkt"
         "cosld-solve.rkt"
         )
(provide solution-subsumes
         solution-entails ; for unit testing
         )

(define-metafunction formality-logic
  ;; Given two solutions `a` and `b` to the same query, returns true if solution
  ;; `a` *subsumes` solution `b`, meaning that `a` is preferred and `b` can be discarded.
  ;; We typically prefer "less restrictive" solutions. Therefore, `a` subsumes `b` if
  ;; assuming `b` is true, `a` must hold, but not vice versa.

  solution-subsumes : Hook QueryGoal Solution_a Solution_b -> boolean

  [(solution-subsumes Hook QueryGoal Solution_a Solution_b)
   #t
   (where #f (solution-entails Hook QueryGoal Solution_a Solution_b))
   (where #t (solution-entails Hook QueryGoal Solution_b Solution_a))
   ]

  [(solution-subsumes Hook QueryGoal Solution_a Solution_b)
   #f
   ]

  )

(define-metafunction formality-logic
  ;; Solution A *entails* Solution B if assuming that A holds implies that B must hold.
  solution-entails : Hook QueryGoal Solution_a Solution_b -> boolean

  [(solution-entails Hook QueryGoal ambiguous Solution)
   #f
   ]

  [(solution-entails Hook QueryGoal Solution ambiguous)
   #f
   ]

  [(solution-entails Hook QueryGoal Solution_a Solution_b)
   boolean

   ; Create the base environment specified by the query
   (where/error (Env_q _) (instantiate-query Hook QueryGoal))
   (where/error VarIds_q (existential-vars-in-env Env_q))

   ; Instantiate the variables from `a` with ∀
   (where/error (:- ∃VarBinders_a (Substitution_a Relations_a)) Solution_a)
   (where/error Env_∀ (env-with-solution-vars Env_q ∀ ∃VarBinders_a))

   ; Instantiate the variables from `b` with ∃
   (where/error (:- ∃VarBinders_b (Substitution_b Relations_b)) Solution_b)
   (where/error Env_∃ (env-with-solution-vars Env_∀ ∃ ∃VarBinders_b))

   ; Add in the relations from `a` as hypotheses
   (where/error Env_h (env-with-hypotheses Env_∃ Relations_a))

   ; A entails B if, in `Env_h`, we can prove all of the following
   ; *without* any required relations, new variables, etc...
   ;
   ; * the substitutions from (a)
   ; * the substitutions from (b)
   ; * the relations from (b)
   (where/error Goal (&& (flatten [(substitution-goals Substitution_a)
                                   (substitution-goals Substitution_b)
                                   Relations_b])))

   ; Identify those query variables that are not constrained by solution A.
   ; Proving the goal cannot add additional constraints to these variables.
   (where/error VarIds_b ,(set-subtract (term VarIds_q) (term (substitution-domain Substitution_a))))

   (where/error boolean ,(judgment-holds (solve-top-level-query-goal VarIds_b Env_h Goal (:- [] ([] [])))))
   ]

  )


(define-metafunction formality-logic
  ;; Extend Env with the variables defined in the solution, but using the given quantifier
  env-with-solution-vars : Env Quantifier ∃VarBinders -> Env

  [(env-with-solution-vars Env Quantifier [])
   Env
   ]

  [(env-with-solution-vars Env Quantifier [∃VarBinder_0 ∃VarBinder_1 ...])
   (env-with-solution-vars Env_0 Quantifier [∃VarBinder_1 ...])
   (where/error (VarId ParameterKind ∃ Universe) ∃VarBinder_0)
   (where/error Env_0 (env-with-var Env VarId ParameterKind Quantifier Universe))
   ]

  )

(define-metafunction formality-logic
  ;; Converts a substitution `Substitution` into a series of equate goals.
  ;;
  ;; e.g. given a subst `[A => B, C => D]`, would return two goals, `A == B` and `C == D`.
  substitution-goals : Substitution -> Goals

  [(substitution-goals [(VarId Parameter) ...])
   [(VarId == Parameter) ...]
   ]
  )