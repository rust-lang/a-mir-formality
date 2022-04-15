#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "predicate.rkt"
         "relate/subtype.rkt"
         "../logic/substitution.rkt"
         "../logic/env.rkt"
         )

(provide ty:equate-predicates/vars
         ty:relate-parameters
         )

(define-metafunction formality-ty
  ty:equate-predicates/vars : Env VarIds Predicate Predicate -> (Env Goals) or Error

  [(ty:equate-predicates/vars Env VarIds Predicate_1 Predicate_2)
   (relate/all VarIds (Env ()) ((Parameter_1 == Parameter_2) ...))
   (where ((Predicate/Skeleton_1 (Parameter_1 ..._1)) (Predicate/Skeleton_1 (Parameter_2 ..._1)))
          ((flay-predicate Predicate_1) (flay-predicate Predicate_2)))
   ]

  [(ty:equate-predicates/vars _ _ _ _)
   Error
   ]

  )

(define-metafunction formality-ty
  ty:relate-parameters : Env Relation -> (Env Goals) or Error

  [(ty:relate-parameters Env Relation)
   (relate/one (existential-vars-in-env Env) Env Relation)]
  )

(define-metafunction formality-ty
  relate/all : VarIds (Env Goals) Relations -> (Env Goals) or Error

  [(relate/all VarIds (Env Goals) ())
   (Env Goals)
   ]

  [(relate/all VarIds (Env_0 (Goal_0 ...)) (Relation_1 Relation_rest ...))
   (relate/all VarIds (Env_1 (Goal_0 ... Goal_1 ...)) (Relation_rest ...))
   (where (Env_1 (Goal_1 ...)) (relate/one VarIds Env_0 Relation_1))
   ]

  [(relate/all _ _ _) Error]

  )

(define-metafunction formality-ty
  relate/one : VarIds Env Relation -> (Env Goals) or Error

  [(relate/one VarIds Env (Parameter_1 == Parameter_2))
   (equate/one/substituted VarIds Env (apply-substitution-from-env Env (Parameter_1 == Parameter_2)))
   ]

  [(relate/one VarIds Env (Parameter_1 InequalityOp Parameter_2))
   (compare/one/substituted VarIds Env (apply-substitution-from-env Env (Parameter_1 InequalityOp Parameter_2)))
   ]
  )

