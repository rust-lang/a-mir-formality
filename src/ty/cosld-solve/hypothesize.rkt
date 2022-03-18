#lang racket
(require redex
         "../grammar.rkt"
         "../substitution.rkt"
         "../instantiate.rkt"
         "util.rkt"
         "filter.rkt")
(provide Hypotheses-imply)

(define-judgment-form formality-ty
  #:mode (Hypotheses-imply I I I O)
  #:contract (Hypotheses-imply Env Predicates_stack Goal EnvSubstitution)

  [(where #f (in? Predicate Predicates_stack))
   (where (_ ... Hypothesis _ ... ) (filter-clauses (flatten ((env-hypotheses Env) (env-invariants Env))) Predicate))
   (Hypothesis-implies Env Predicates_stack Hypothesis Predicate EnvSubstitution_out)
   --------------- "imply-clause"
   (Hypotheses-imply Env Predicates_stack Predicate EnvSubstitution_out)
   ]

  [(equate Env Term_1 Term_2 EnvSubstitution_out)
   --------------- "prove-equate"
   (Hypotheses-imply Env Predicates_stack (Equate Term_1 Term_2) EnvSubstitution_out)
   ]

  [(Hypotheses-imply-all Env Predicates_stack Goals EnvSubstitution_out)
   --------------- "prove-all"
   (Hypotheses-imply Env Predicates_stack (All Goals) EnvSubstitution_out)
   ]

  [(Hypotheses-imply Env Predicates_stack Goal_1 EnvSubstitution_out)
   --------------- "prove-any"
   (Hypotheses-imply Env Predicates_stack (Any (Goal_0 ... Goal_1 Goal_2 ...)) EnvSubstitution_out)
   ]

  [(where Env_1 (env-with-hypotheses Env Hypotheses))
   (Hypotheses-imply Env_1 Predicates_stack Goal EnvSubstitution_out)
   --------------- "prove-implies"
   (Hypotheses-imply Env Predicates_stack (Implies Hypotheses Goal) (reset Env () EnvSubstitution_out))
   ]

  [(where/error (Env_1 Goal_1 VarIds_new) (instantiate-quantified Env (ForAll KindedVarIds Goal)))
   (Hypotheses-imply Env_1 Predicates_stack Goal_1 EnvSubstitution_out)
   --------------- "prove-forall"
   (Hypotheses-imply Env Predicates_stack (ForAll KindedVarIds Goal) (reset Env VarIds_new EnvSubstitution_out))
   ]

  [(where/error (Env_1 Goal_1 VarIds_new) (instantiate-quantified Env (Exists KindedVarIds Goal)))
   (Hypotheses-imply Env_1 Predicates_stack Goal_1 EnvSubstitution_out)
   --------------- "prove-exists"
   (Hypotheses-imply Env Predicates_stack (Exists KindedVarIds Goal) (reset Env VarIds_new EnvSubstitution_out))
   ]
  )

(define-judgment-form formality-ty
  #:mode (Hypotheses-imply-all I I I O)
  #:contract (Hypotheses-imply-all Env Predicates_stack Goals EnvSubstitution)

  [--------------- "imply-all-1"
   (Hypotheses-imply-all Env Predicates_stack () (Env ()))
   ]

  [(Hypotheses-imply Env Predicates_stack Goal_first (Env_first Substitution_first))
   (where/error (Goals_subst Predicates_subst) (apply-substitution Substitution_first ((Goals_rest ...) Predicates_stack)))
   (Hypotheses-imply-all Env_first Predicates_subst Goals_subst EnvSubstitution_rest)
   --------------- "imply-all-2"
   (Hypotheses-imply-all Env Predicates_stack (Goal_first Goals_rest ...) EnvSubstitution_rest)
   ]
  )

(define-judgment-form formality-ty
  #:mode (Hypothesis-implies I I I I O)
  #:contract (Hypothesis-implies Env Predicates_stack Clause Predicate EnvSubstitution)

  [(equate Env Predicate_1 Predicate_2 EnvSubstitution)
   --------------- "Hypothesis-fact"
   (Hypothesis-implies Env Predicates_stack Predicate_1 Predicate_2 EnvSubstitution)
   ]

  [(equate Env Predicate_1 Predicate_2 (Env_eq Substitution_eq))
   (where/error (Goals_subst Predicates_subst)
                (apply-substitution Substitution_eq (Goals (Predicate_2 Predicate_stack ...))))
   (Hypotheses-imply-all Env Predicates_subst Goals_subst EnvSubstitution)
   --------------- "clause-backchain"
   (Hypothesis-implies Env (Predicate_stack ...) (Implies Goals Predicate_1) Predicate_2 EnvSubstitution)
   ]

  [(where/error (Env_i Clause_i VarIds_i) (instantiate-quantified Env (Exists KindedVarIds Clause)))
   (Hypothesis-implies Env_i Predicates_stack Clause_i Predicate EnvSubstitution)
   --------------- "clause-forall"
   (Hypothesis-implies Env Predicates_stack (ForAll KindedVarIds Clause) Predicate (reset Env VarIds_i EnvSubstitution))
   ]

  )