#lang racket
(require redex/reduction-semantics
         "occurs-check.rkt"
         "universe-check.rkt"
         "rigid.rkt"
         "../grammar.rkt"
         "../inequalities.rkt"
         "../where-clauses.rkt"
         "../parameters.rkt"
         "../extrude.rkt"
         "../../logic/substitution.rkt"
         "../../logic/env.rkt"
         )
(provide compare/one/substituted
         )

(define-metafunction formality-ty
  ;;
  compare/one/substituted : Env (Parameter InequalityOp Parameter) -> (Env Goals) or Error

  [; X ?= X:
   ;   Always ok.
   (compare/one/substituted Env (Parameter InequalityOp Parameter))
   (Env ())
   ]

  [; X ?= R<...> -- Inequality between a variable and a rigid type
   (compare/one/substituted Env (VarId InequalityOp (TyRigid RigidName (Parameter ...))))
   (relate-var-to-rigid Env (VarId InequalityOp (TyRigid RigidName (Parameter ...))))
   (where #t (env-contains-existential-var Env VarId))
   ]

  [; R<...> ?= X -- Inequality between a variable and a rigid type
   (compare/one/substituted Env ((TyRigid RigidName (Parameter ...)) InequalityOp VarId))
   (relate-var-to-rigid Env (VarId (invert-inequality-op InequalityOp) (TyRigid RigidName (Parameter ...))))
   (where #t (env-contains-existential-var Env VarId))
   ]

  [; R<...> ?= X -- Inequality between a variable and a rigid type
   (compare/one/substituted Env ((TyRigid RigidName (Parameter ...)) InequalityOp VarId))
   (relate-var-to-rigid Env (VarId (invert-inequality-op InequalityOp) (TyRigid RigidName (Parameter ...))))
   (where #t (env-contains-existential-var Env VarId))
   ]

  [; R<...> ?= R<...> -- Relating two rigid types with the same name: relate their parameters according to the declared variance.
   (compare/one/substituted Env ((TyRigid RigidName (Parameter_1 ..._1)) InequalityOp (TyRigid RigidName (Parameter_2 ..._1))))
   (relate-rigid-to-rigid Env ((TyRigid RigidName (Parameter_1 ...)) InequalityOp (TyRigid RigidName (Parameter_2 ...))))
   ]

  [; `?X <= T` where occurs, universes ok:
   ;   Add `X <= P` as a bound and, for each bound `B` where `B <= X`,
   ;   require that `B <= P` (respectively `>=`).
   (compare/one/substituted Env (VarId InequalityOp Parameter))
   (Env_1 ((Parameter_bound InequalityOp Parameter) ...))

   (where #t (env-contains-existential-var Env VarId))
   (where #t (occurs-check-ok? Env VarId Parameter))
   (where #t (universe-check-ok? Env VarId Parameter))
   (where/error (Parameter_bound ...) (known-bounds Env InequalityOp VarId))
   (where/error Env_1 (env-with-var-related-to-parameter Env VarId InequalityOp Parameter))
   ]

  [; `T <= ?X` where occurs, universes ok:
   ;    Flip to `?X <= T` and use above rule.
   (compare/one/substituted Env (Parameter InequalityOp VarId))
   (compare/one/substituted Env (VarId (invert-inequality-op InequalityOp) Parameter))

   (where #t (env-contains-existential-var Env VarId))
   (where #t (occurs-check-ok? Env VarId Parameter))
   (where #t (universe-check-ok? Env VarId Parameter))
   ]

  [; Flip `>=` to `<=` for forall, exists, and implication types
   (compare/one/substituted Env (Parameter_1 >= Parameter_2))
   (compare/one/substituted Env (Parameter_2 <= Parameter_1))
   (where #t (any? (forall-exists-or-implication? Parameter_1)
                   (forall-exists-or-implication? Parameter_2)))
   ]

  [; ∀ on the supertype side
   (compare/one/substituted Env (Parameter_1 <= (ForAll KindedVarIds Parameter_2)))
   ; NB: Redex binding forms ensure that names in `KindedVarIds` do not appear free in `Parameter_1`
   (Env ((ForAll KindedVarIds (Parameter_1 <= Parameter_2))))
   ]

  [; Implication on the supertype side
   (compare/one/substituted Env (Parameter_1 <= (Implies WhereClauses Parameter_2)))
   (Env ((Implies (where-clauses->goals WhereClauses) (Parameter_1 <= Parameter_2))))
   ]

  [; ∀ on the subtype side
   (compare/one/substituted Env ((ForAll KindedVarIds Parameter_1) <= Parameter_2))
   ; NB: Redex binding forms ensure that all names in `KindedVarIds` do not appear free in `Parameter_2`
   (Env ((Exists KindedVarIds (Parameter_1 <= Parameter_2))))
   ]

  [; Implication on the subtype side
   (compare/one/substituted Env ((Implies WhereClauses Parameter_1) <= Parameter_2))
   (Env (Goal_wc ... (Parameter_1 <= Parameter_2)))
   (where (Goal_wc ...) (where-clauses->goals WhereClauses))
   ]

  [; `!X <= T` where:
   ;    Prove `X <= T1` for any `T1 <= P` from environment.
   (compare/one/substituted Env (VarId InequalityOp Parameter))
   (Env ((Any ((Parameter_bound InequalityOp Parameter) ...))))

   (where #t (env-contains-placeholder-var Env VarId))
   (where/error (Parameter_bound ...) (known-bounds Env InequalityOp VarId)) ; * FIXME: need to look through hypotheses
   ]

  [; `T <= !X` where:
   ;    Flip to `!X <= T` and use above rule.
   (compare/one/substituted Env (Parameter InequalityOp VarId))
   (compare/one/substituted Env (VarId (invert-inequality-op InequalityOp) Parameter))

   (where #t (env-contains-placeholder-var Env VarId))
   ]

  [; `?X <= T` where occurs ok, universes not ok:
   ;   Add `X <= P` as a bound and, for each bound `B` where `B <= X`,
   ;   require that `B <= P` (respectively `>=`).
   (compare/one/substituted Env (VarId InequalityOp Parameter))
   (Env_1 ((VarId InequalityOp Parameter_extruded) Goal ...))

   (where #t (env-contains-existential-var Env VarId))
   (where #t (occurs-check-ok? Env VarId Parameter))
   (where #f (universe-check-ok? Env VarId Parameter))
   (where/error Universe_VarId (universe-of-var-in-env Env VarId))
   (where/error (Env_1 Parameter_extruded (Goal ...)) (extrude-parameter Env Universe_VarId InequalityOp Parameter))
   ]

  [; `T <= ?X` where occurs ok, universes not ok:
   ;     Invert and use above rule.
   (compare/one/substituted Env (Parameter InequalityOp VarId))
   (compare/one/substituted Env (VarId (invert-inequality-op InequalityOp) Parameter))

   (where #t (env-contains-existential-var Env VarId))
   (where #t (occurs-check-ok? Env VarId Parameter))
   (where #f (universe-check-ok? Env VarId Parameter))
   ]

  [; all other sets of types cannot be compared
   (compare/one/substituted _ _)
   Error
   ]

  )

(define-metafunction formality-ty
  ;; True if this is a forall, exists, or implication type.
  forall-exists-or-implication? : Parameter -> boolean

  [(forall-exists-or-implication? (ForAll _ _)) #t]
  [(forall-exists-or-implication? (Exists _ _)) #t]
  [(forall-exists-or-implication? (Implies _ _)) #t]
  [(forall-exists-or-implication? _) #f]

  )


