#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "predicate.rkt"
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

  [(relate/one VarIds Env Relation)
   (relate/one/substituted VarIds Env (apply-substitution-from-env Env Relation))
   ]
  )

(define-metafunction formality-ty
  relate/one/substituted : VarIds_exists Env Relation -> (Env Goals) or Error

  [; X = X ===> always ok
   (relate/one/substituted _ Env (Parameter == Parameter))
   (Env ())
   ]

  [; X = P ===> occurs check ok, return `[X => P]`
   (relate/one/substituted VarIds_exists Env (VarId == Parameter))
   ((env-with-var-mapped-to Env_out VarId Parameter) ())

   (where #t (in? VarId VarIds_exists))
   (where Env_out (occurs-check Env VarId Parameter))
   ]

  [; X = P ===> but occurs check fails, return Error
   (relate/one/substituted VarIds_exists Env (VarId == Parameter))
   Error

   (where #t (in? VarId VarIds_exists))
   (where Error (occurs-check Env VarId Parameter))
   ]

  [; P = X ===> just reverse order
   (relate/one/substituted VarIds_exists Env (Parameter == VarId))
   (relate/one/substituted VarIds_exists Env (VarId == Parameter))

   (where #t (in? VarId VarIds_exists))
   ]

  [; Relating two rigid types with the same name: relate their parameters.
   (relate/one/substituted VarIds Env ((TyRigid RigidName (Parameter_1 ..._1)) == (TyRigid RigidName (Parameter_2 ..._1))))
   (relate/all VarIds (Env ()) ((Parameter_1 == Parameter_2) ...))
   ]

  [; all other sets of types cannot be related
   (relate/one/substituted _ _ _)
   Error
   ]

  )

(define-metafunction formality-ty
  ;; Checks whether `VarId` can be assigned the value of `Parameter`.
  ;;
  ;; Fails if:
  ;;
  ;; * `VarId` appears in `Parameter`
  ;; * `Parameter` mentions a placeholder that `VarId` cannot name because of its universe
  ;;
  ;; Returns a fresh environment which contains adjust universes for each
  ;; variable `V` that occur in `Parameter`. The universe of `V` cannot
  ;; be greater than the universe of `VarId` (since whatever value `V` ultimately
  ;; takes on will become part of `VarId`'s value).
  occurs-check : Env VarId Parameter -> Env or Error

  [(occurs-check Env VarId Parameter)
   Env_1

   ; can't have X = Vec<X> or whatever, that would be infinite in size
   (where #f (appears-free VarId Parameter))

   ; find the universe of `VarId`
   (where/error Universe_VarId (universe-of-var-in-env Env VarId))

   ; can't have `X = T<>` if `X` cannot see the universe of `T`
   (where/error (VarId_placeholder ...) (placeholder-variables Env Parameter))
   (where #t (all? (universe-includes Universe_VarId (universe-of-var-in-env Env VarId_placeholder)) ...))

   ; for each `X = ... Y ...`, adjust universe of Y so that it can see all values of X
   (where/error VarIds_free (free-variables Env Parameter))
   (where/error Env_1 (env-with-vars-limited-to-universe Env VarIds_free Universe_VarId))
   ]

  [(occurs-check Env VarId Parameter)
   Error
   ]

  )
