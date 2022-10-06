#lang racket
(require redex/reduction-semantics
         "occurs-check.rkt"
         "universe-check.rkt"
         "rigid.rkt"
         "../kind.rkt"
         "../grammar.rkt"
         "../extrude.rkt"
         "../hypothesized-bounds.rkt"
         "../../logic/env.rkt"
         "../../logic/env-inequalities.rkt"
         )
(provide compare/one/substituted
         )

(define-metafunction formality-ty
  compare/one/substituted : Env_in (Parameter_a SubtypeOp Parameter_b) -> (Env Goals) or Error

  #:pre ,(eq? (term (parameter-kind Env_in Parameter_a))
              (term (parameter-kind Env_in Parameter_b)))

  [; X ?= X:
   ;   Always ok.
   (compare/one/substituted Env (Parameter SubtypeOp Parameter))
   (Env ())
   ]

  [; 'a <= 'b if 'a -outlives- 'b
   ; 'a >= 'b if 'a -outlived-by- 'b
   (compare/one/substituted Env (Parameter_a SubtypeOp Parameter_b))
   (Env ((Parameter_a (subtype->outlives SubtypeOp) Parameter_b)))
   (where lifetime (parameter-kind Env Parameter_a))
   (where/error lifetime (parameter-kind Env Parameter_b)) ; ought to be well-kinded
   ]

  [; X ?= R<...> -- Inequality between a variable and a rigid type
   (compare/one/substituted Env (VarId SubtypeOp (rigid-ty RigidName (Parameter ...))))
   (relate-var-to-rigid Env (VarId SubtypeOp (rigid-ty RigidName (Parameter ...))))
   (where #t (env-contains-existential-var Env VarId))
   ]

  [; R<...> ?= X -- Inequality between a variable and a rigid type
   (compare/one/substituted Env ((rigid-ty RigidName (Parameter ...)) SubtypeOp VarId))
   (relate-var-to-rigid Env (VarId (invert-inequality-op SubtypeOp) (rigid-ty RigidName (Parameter ...))))
   (where #t (env-contains-existential-var Env VarId))
   ]

  [; R<...> ?= X -- Inequality between a variable and a rigid type
   (compare/one/substituted Env ((rigid-ty RigidName (Parameter ...)) SubtypeOp VarId))
   (relate-var-to-rigid Env (VarId (invert-inequality-op SubtypeOp) (rigid-ty RigidName (Parameter ...))))
   (where #t (env-contains-existential-var Env VarId))
   ]

  [; R<...> ?= R<...> -- Relating two rigid types with the same name: relate their parameters according to the declared variance.
   (compare/one/substituted Env ((rigid-ty RigidName (Parameter_1 ..._1)) SubtypeOp (rigid-ty RigidName (Parameter_2 ..._1))))
   (relate-rigid-to-rigid Env ((rigid-ty RigidName (Parameter_1 ...)) SubtypeOp (rigid-ty RigidName (Parameter_2 ...))))
   ]

  [; `?X <= T` where occurs, universes ok:
   ;   Add `X <= P` as a bound and, for each bound `B` where `X >= B`,
   ;   require that `B <= P` (respectively `>=`).
   (compare/one/substituted Env (VarId SubtypeOp Parameter))
   (Env_1 ((Parameter_bound SubtypeOp Parameter) ...))

   (where #t (env-contains-existential-var Env VarId))
   (where #t (occurs-check-ok? Env VarId Parameter))
   (where #t (universe-check-ok? Env VarId Parameter))
   (where/error (Parameter_bound ...) (known-bounds Env VarId (invert-inequality-op SubtypeOp)))
   (where/error Env_1 (env-with-var-related-to-parameter Env VarId SubtypeOp Parameter))
   ]

  [; `T <= ?X` where occurs, universes ok:
   ;    Flip to `?X <= T` and use above rule.
   (compare/one/substituted Env (Parameter SubtypeOp VarId))
   (compare/one/substituted Env (VarId (invert-inequality-op SubtypeOp) Parameter))

   (where #t (env-contains-existential-var Env VarId))
   (where #t (occurs-check-ok? Env VarId Parameter))
   (where #t (universe-check-ok? Env VarId Parameter))
   ]

  [; `?X <= T` where occurs ok, universes not ok:
   ;   Add `X <= P` as a bound and, for each bound `B` where `B <= X`,
   ;   require that `B <= P` (respectively `>=`).
   (compare/one/substituted Env (VarId SubtypeOp Parameter))
   (Env_1 ((VarId SubtypeOp Parameter_extruded) Goal ...))

   (where #t (env-contains-existential-var Env VarId))
   (where #t (occurs-check-ok? Env VarId Parameter))
   (where #f (universe-check-ok? Env VarId Parameter))
   (where/error Universe_VarId (universe-of-var-in-env Env VarId))
   (where/error (Env_1 Parameter_extruded (Goal ...)) (extrude-parameter Env Universe_VarId SubtypeOp Parameter))
   ]

  [; `T <= ?X` where occurs ok, universes not ok:
   ;     Invert and use above rule.
   (compare/one/substituted Env (Parameter SubtypeOp VarId))
   (compare/one/substituted Env (VarId (invert-inequality-op SubtypeOp) Parameter))

   (where #t (env-contains-existential-var Env VarId))
   (where #t (occurs-check-ok? Env VarId Parameter))
   (where #f (universe-check-ok? Env VarId Parameter))
   ]

  [; Flip `>=` to `<=` for remaining rules
   (compare/one/substituted Env (Parameter_1 >= Parameter_2))
   (compare/one/substituted Env (Parameter_2 <= Parameter_1))
   ]

  [; ∀ on the supertype side
   ;
   ; if I have to prove that `T <: forall<'a> U`...
   ; I can prove...
   ; `forall<'a> { T <: U }`... and that's the same thing

   (compare/one/substituted Env (Parameter_1 <= (∀ KindedVarIds Parameter_2)))
   ; NB: Redex binding forms ensure that names in `KindedVarIds` do not appear free in `Parameter_1`
   (Env ((∀ KindedVarIds (Parameter_1 <= Parameter_2))))
   ]

  [; Implication on the supertype side
   (compare/one/substituted Env (Parameter_1 <= (implies Biformulas Parameter_2)))
   (Env ((implies Biformulas (Parameter_1 <= Parameter_2))))
   ]

  [; Ensures on the subtype side
   (compare/one/substituted Env ((ensures Parameter_1 Biformulas) <= Parameter_2))
   (Env ((implies Biformulas (Parameter_1 <= Parameter_2))))
   ]

  [; ∀ on the subtype side
   ;
   ; if I have to prove that `forall<'a> T <: U`...
   ; I can prove...
   ; `exists<'a> { T <: U }`... and that's the same thing
   (compare/one/substituted Env ((∀ KindedVarIds Parameter_1) <= Parameter_2))
   ; NB: Redex binding forms ensure that all names in `KindedVarIds` do not appear free in `Parameter_2`
   (Env ((∃ KindedVarIds (Parameter_1 <= Parameter_2))))
   ]

  [; Implication on the subtype side
   (compare/one/substituted Env ((implies [Biformula ...] Parameter_1) <= Parameter_2))
   (Env [Biformula ... (Parameter_1 <= Parameter_2)])
   ]

  [; Ensures on the supertype side
   (compare/one/substituted Env (Parameter_1 <= (ensures Parameter_2 [Biformula ...])))
   (Env [Biformula ... (Parameter_1 <= Parameter_2)])
   ]

  [; Alias on both sides with same name
   (compare/one/substituted Env ((alias-ty AliasName (Parameter_1 ...)) <= (alias-ty AliasName (Parameter_2 ...))))
   (Env ((|| (Goal_eq Goal_n))))
   (; Either all the parameters are equal (note that we have no variance on alias-ty, so they
    ; must be equal)
    where/error Goal_eq (&& ((Parameter_1 == Parameter_2) ...)))
   (; Or we can normalize both aliases to the same type
    where/error Goal_n (∃ ((type T1) (type T2))
                          (&& ((T1 <= T2)
                               (normalizes-to (alias-ty AliasName (Parameter_1 ...)) T1)
                               (normalizes-to (alias-ty AliasName (Parameter_2 ...)) T2)))
                          ))
   ]

  [; Alias on subtype
   (compare/one/substituted Env (AliasTy <= Ty))
   (Env (Goal_n))
   (; The alias has to normalize to a subtype of Ty
    where/error Goal_n (∃ ((type T))
                          (&& ((T <= Ty)
                               (normalizes-to AliasTy T)))))
   ]

  [; Alias on supertype
   (compare/one/substituted Env (Ty <= AliasTy))
   (Env (Goal_n))
   (; The alias has to normalize to a supertype of Ty
    where/error Goal_n (∃ ((type T))
                          (&& ((Ty <= T)
                               (normalizes-to AliasTy T)))))
   ]

  [; `!X <= T` where:
   ;    Prove `X <= T1` for any `T1 <= P` from environment.
   (compare/one/substituted Env (VarId <= Parameter))
   (Env ((|| Goals)))

   (where #t (env-contains-placeholder-var Env VarId))
   (where/error Goals (bound-placeholder-from-hypotheses Env VarId <= Parameter))
   ]

  [; `T <= !X` where:
   ;    Flip to `!X >= T` and use above rule.
   (compare/one/substituted Env (Parameter <= VarId))
   (Env ((|| Goals)))

   (where #t (env-contains-placeholder-var Env VarId))
   (where/error Goals (bound-placeholder-from-hypotheses Env VarId >= Parameter))
   ]

  [; all other sets of types cannot be compared
   (compare/one/substituted _ _)
   Error
   ]

  )

(define-metafunction formality-ty
  ;; Convert a subtype relation (on lifetimes) to an outlives op.
  subtype->outlives : SubtypeOp -> OutlivesOp

  [(subtype->outlives <=)  -outlives-]
  [(subtype->outlives >=)  -outlived-by-]

  )


