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

  [; X == X, X <= X, X >= X:
   ;   Always ok.
   (relate/one/substituted _ Env (Parameter RelationOp Parameter))
   (Env ())
   ]

  [; X = P<... X ...> etc:
   ;   Error if `X` appears in its own value.
   (relate/one/substituted VarIds_exists Env (VarId RelationOp Parameter))
   Error

   (where #t (in?/id VarId VarIds_exists))
   (where #f (occurs-check Env VarId Parameter))
   ]

  [; X = P where occurs check ok:
   ;   Substitute `[X => P]` and prove `P <= P1`/`P >= P1` for each bound `P1` on `X`.
   (relate/one/substituted VarIds_exists Env (VarId == Parameter))
   (Env_2 ((Parameter_lb <= Parameter) ... (Parameter <= Parameter_ub) ...))

   (where #t (in?/id VarId VarIds_exists))
   (where/error #t (occurs-check Env VarId Parameter)) ; tested earlier
   (where #t (universe-check Env VarId Parameter))
   (where/error (Env_1 ((Parameter_lb ...) (Parameter_ub ...))) (remove-var-bounds-from-env Env VarId))
   (where/error Env_2 (env-with-var-mapped-to Env_1 VarId Parameter))
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

  [; `X <= P`, `X >= P` where universes, occurs-check ok:
   ;   Add `X <= P` as a bound and, for each `P1` where `P1 <= X`,
   ;   require that `P1 <= P` (respectively `>=`).
   (relate/one/substituted VarIds_exists Env (VarId InequalityOp Parameter))
   (Env_1 ((Parameter_bound InequalityOp Parameter) ...))

   (where #t (in?/id VarId VarIds_exists))
   (where/error #t (occurs-check Env VarId Parameter)) ; tested earlier
   (where #t (universe-check Env VarId Parameter))
   (where/error (Parameter_bound ...) (known-bounds Env InequalityOp VarId))
   (where/error Env_1 (env-with-var-related-to-parameter Env VarId InequalityOp Parameter))
   ]

  [; `!X <= P` where:
   ;    Prove `X <= P1` for any `P1 <= P`.
   ;
   ; * FIXME: This is not optimal.
   (relate/one/substituted VarIds_exists Env (VarId InequalityOp Parameter))
   (Env_1 ((Any ((Parameter_bound InequalityOp Parameter) ...))))

   (where #t (env-contains-placeholder-var Env VarId))
   (where/error (Parameter_bound ...) (known-bounds Env InequalityOp VarId)) ; *
   ]

  [; `P op X` ===> just reverse order
   (relate/one/substituted VarIds_exists Env (Parameter RelationOp VarId))
   (relate/one/substituted VarIds_exists Env (VarId (invert-relation RelationOp) Parameter))

   (where #t (in?/id VarId VarIds_exists))
   ]

  [; `!X <= P` where:
   ;    Prove `X <= P1` for any `P1 <= P`.
   ;
   ; * FIXME: This is not optimal.
   (relate/one/substituted VarIds_exists Env (VarId InequalityOp Parameter))
   (Env_1 ((Any ((Parameter_bound InequalityOp Parameter) ...))))

   (where #t (env-contains-placeholder-var Env VarId))
   (where/error (Parameter_bound ...) (known-bounds Env InequalityOp VarId)) ; *
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
  ;; Checks whether `VarId` appears free in `Parameter`.
  occurs-check : Env VarId Parameter -> boolean

  [(occurs-check Env VarId Parameter)
   ; can't have X = Vec<X> or whatever, that would be infinite in size
   (not? (in?/id VarId (free-variables Env Parameter)))]

  )

(define-metafunction formality-ty
  ;; Checks whether `VarId` is in a universe that can see all the values of `Parameter`.
  ;;
  ;; Fails if:
  ;;
  ;; * `Parameter` mentions a placeholder that `VarId` cannot name because of its universe
  ;;
  ;; Returns a fresh environment which contains adjust universes for each
  ;; variable `V` that occur in `Parameter`. The universe of `V` cannot
  ;; be greater than the universe of `VarId` (since whatever value `V` ultimately
  ;; takes on will become part of `VarId`'s value).
  universe-check : Env VarId Parameter -> boolean

  [(universe-check Env VarId Parameter)
   (all? (universe-includes Universe_VarId (universe-of-var-in-env Env VarId_free)) ...)

   (where/error Universe_VarId (universe-of-var-in-env Env VarId))
   (where/error (VarId_free ...) (free-variables Env Parameter))
   ]

  )


