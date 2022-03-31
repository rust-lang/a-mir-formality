#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "predicate.rkt"
         "inequalities.rkt"
         "where-clauses.rkt"
         "parameters.rkt"
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

  [; X == X, X <= X, X >= X ===> always ok
   (relate/one/substituted _ Env (Parameter RelationOp Parameter))
   (Env ())
   ]

  [; X = P ===> if occurs check ok, return `[X => P]`
   ;
   ; If we had inequalities related to `X`, e.g., `X <= u32`, then
   ; we enqueue goals like `P <= u32`.
   (relate/one/substituted VarIds_exists Env (VarId == Parameter))
   (Env_3 ((Parameter_lb <= Parameter) ... (Parameter <= Parameter_ub) ...))

   (where #t (in?/id VarId VarIds_exists))
   (where Env_1 (occurs-check Env VarId Parameter))
   (where/error (Env_2 ((Parameter_lb ...) (Parameter_ub ...))) (remove-var-bounds-from-env Env_1 VarId))
   (where/error Env_3 (env-with-var-mapped-to Env_2 VarId Parameter))
   ]

  [; X = P ===> if occurs check fails, return Error
   (relate/one/substituted VarIds_exists Env (VarId == Parameter))
   Error

   (where #t (in?/id VarId VarIds_exists))
   (where Error (occurs-check Env VarId Parameter))
   ]

  [; X ?= R<...> -- Inequality between a variable and a rigid type (`==` is handled above)
   ;
   ; addressed by instantiating X with `R<P1...Pn>` for fresh P1...Pn and then requiring
   ; `R<P1...Pn> ?= R<...>` with a goal like
   ;
   ; `∃P1...Pn: (X = R<P1...Pn>) ∧ WF(R<P1..Pn>) ∧ (R<P1..Pn> ?= R<...>)`
   ;
   ; Note the requirement to prove `WF(R<P1..Pn>)`. This is necessary because the `T1 ?= T2`
   ; judgments assume that T1, T2 are WF.
   (relate/one/substituted VarIds Env (VarId InequalityOp Parameter_r))
   (Env (Goal))

   (where (TyRigid RigidName (Parameter ...)) Parameter_r)
   (where #t (in?/id VarId VarIds_exists))
   ; get the generic parameters for the rigid-name `R`
   (where/error ((VarId_rigid (ParameterKind_rigid _)) ...) (generic-parameters-for Env RigidName))
   ; make fresh names `VarId_p ...` for each parameter that don't appear in `R<...>` to avoid accidental capture
   (where/error ((VarId_rigid VarId_p) ...) (substitution-to-fresh-vars Parameter_r ((VarId_rigid ParameterKind_rigid) ...)))
   ; create the `R<P1..Pn>` type
   (where/error Parameter_p (TyRigid RigidName (VarId_p ...)))
   ; create the `∃P1...Pn: (X = R<P1...Pn>) ∧ WF(R<P1..Pn>) ∧ (R<P1..Pn> ?= R<...>)` goal
   ;
   ; I've reordered the WellFormed requirement since it helps the cosld solver
   (where/error Goal (Exists ((VarId_p ParameterKind_rigid) ...)
                             (All ((VarId = Parameter_p)
                                   (Parameter_p InequalityOp Parameter_r)
                                   (WellFormed (TyKind Parameter_p))
                                   ))))
   ]

  [; P op X ===> just reverse order
   (relate/one/substituted VarIds_exists Env (Parameter RelationOp VarId))
   (relate/one/substituted VarIds_exists Env (VarId (invert-relation RelationOp) Parameter))

   (where #t (in?/id VarId VarIds_exists))
   ]


  [; Relating two rigid types with the same name: relate their parameters according to the declared variance.
   (relate/one/substituted VarIds Env ((TyRigid RigidName (Parameter_1 ..._1)) RelationOp (TyRigid RigidName (Parameter_2 ..._1))))
   (relate/all VarIds (Env ()) ((Parameter_1 (apply-variance Variance RelationOp) Parameter_2) ...))
   (where/error (Variance ...) (variances-for Env RigidName))
   (where #t (same-length (Variance ...) (Parameter_1 ...))) ; well-formedness violation otherwise
   ]

  [; For ∀ or implication types, rewrite `==` as two `<=` relations
   (relate/one/substituted VarIds Env (Parameter_1 == Parameter_2))
   (Env ((Parameter_1 <= Parameter_2) (Parameter_2 <= Parameter_1)))

   (where #t (any? (is-forall-or-implies Parameter_1)
                   (is-forall-or-implies Parameter_2)))
   ]

  [; ∀ on the supertype side
   (relate/one/substituted VarIds Env (Parameter_1 <= (ForAll KindedVarIds Parameter_2)))
   (Env (ForAll KindedVarIds (Parameter_1 RelationOp Parameter_2)))
   ]

  [; Implication on the supertype side
   (relate/one/substituted VarIds Env (Parameter_1 <= (Implies WhereClauses Parameter_2)))
   (Env ((Implies (where-clauses->goals WhereClauses) (Parameter_1 RelationOp Parameter_2))))
   ]

  [; ∀ on the subtype side
   (relate/one/substituted VarIds Env ((ForAll KindedVarIds Parameter_1) <= Parameter_2))
   (Env (Exists KindedVarIds (Parameter_1 RelationOp Parameter_2)))
   ]

  [; Implication on the subtype side
   (relate/one/substituted VarIds Env ((Implies WhereClauses Parameter_1) <= Parameter_2))
   (Env (Goal_wc ... (Parameter_1 <= Parameter_2)))
   (where (Goal_wc ...) (where-clauses->goals WhereClauses))
   ]

  [; all other sets of types cannot be related
   (relate/one/substituted _ _ _)
   Error
   ]

  )

(define-metafunction formality-ty
  is-forall-or-implies : Parameter -> boolean

  [(is-forall-or-implies (ForAll _ _)) #t]
  [(is-forall-or-implies (Implies _ _)) #t]
  [(is-forall-or-implies _) #f]
  )

(define-metafunction formality-ty
  invert-relation : RelationOp -> RelationOp

  [(invert-relation <=) >=]
  [(invert-relation >=) <=]
  [(invert-relation ==) ==]
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
