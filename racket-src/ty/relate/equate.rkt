#lang racket
(require redex/reduction-semantics
         "../../logic/grammar.rkt"
         "../../logic/substitution.rkt"
         "../../logic/env.rkt"
         "../../logic/env-inequalities.rkt"
         "occurs-check.rkt"
         "universe-check.rkt"
         "rigid.rkt"
         "../grammar.rkt"
         )
(provide equate/one/substituted
         )

(define-metafunction formality-ty
  ;; Equate `T1 == T2`: this is an optimization over `T1 <= T2` and `T1 >= T2`,
  ;; applied to various cases:
  ;;
  ;; *
  ;; *
  equate/one/substituted : Env (Parameter == Parameter) -> (Env Goals)

  [; T == T is always ok
   (equate/one/substituted Env (Parameter == Parameter))
   (Env ())
   ]

  [; X == T where occurs check, universe check ok:
   ;   Substitute `[X => P]` and prove `P <= P1`/`P >= P1` for each bound `P1` on `X`.
   (equate/one/substituted Env (VarId == Parameter))
   (map-var Env VarId Parameter)

   (where #t (env-contains-existential-var Env VarId))
   (where #t (occurs-check-ok? Env VarId Parameter))
   (where #t (universe-check-ok? Env VarId Parameter))
   ]

  [; X == T where occurs check, universe check ok, but in reverse
   (equate/one/substituted Env (Parameter == VarId))
   (map-var Env VarId Parameter)

   (where #t (env-contains-existential-var Env VarId))
   (where #t (occurs-check-ok? Env VarId Parameter))
   (where #t (universe-check-ok? Env VarId Parameter))
   ]

  [; X == R<...> -- equality between a variable and a rigid type
   ;
   ; Universe check fails or else previous case would have matched.
   ; In this case, we equate `X` with `R<...>` and recursively relate
   ; the parameters.
   (equate/one/substituted Env (VarId == (rigid-ty RigidName (Parameter ...))))
   (relate-var-to-rigid Env (VarId == (rigid-ty RigidName (Parameter ...))))
   (where #t (env-contains-existential-var Env VarId))
   ]

  [; R<...> == X -- equality between a variable and a rigid type
   ;
   ; Universe check fails or else previous case would have matched.
   ; In this case, we equate `X` with `R<...>` and recursively relate
   ; the parameters.
   (equate/one/substituted Env ((rigid-ty RigidName (Parameter ...)) == VarId))
   (relate-var-to-rigid Env (VarId == (rigid-ty RigidName (Parameter ...))))
   (where #t (env-contains-existential-var Env VarId))
   ]

  [; Equating two rigid types with the same name: equate their parameters
   (equate/one/substituted Env ((rigid-ty RigidName (Parameter_1 ..._1)) == (rigid-ty RigidName (Parameter_2 ..._1))))
   (Env ((Parameter_1 == Parameter_2) ...))
   ]

  [; Equating two rigid types with different names: error!
   ;
   ; This is an optimization to reduce us going down less fruitful
   ; paths (makes the debug output awfully hard to read.)
   (equate/one/substituted Env ((rigid-ty RigidName_!_0 _) == (rigid-ty RigidName_!_0 _)))
   (Env (false-goal))
   ]

  [; Equating two alias types with same name
   (equate/one/substituted Env ((alias-ty AliasName (Parameter_1 ...)) == (alias-ty AliasName (Parameter_2 ...))))
   (Env ((|| (Goal_eq Goal_n))))
   (; Either all the parameters are equal
    where/error Goal_eq (&& ((Parameter_1 == Parameter_2) ...)))
   (; Or we can normalize both aliases to the same type
    where/error Goal_n (∃ ((type T))
                          (&& ((normalizes-to (alias-ty AliasName (Parameter_1 ...)) T)
                               (normalizes-to (alias-ty AliasName (Parameter_2 ...)) T)))
                          ))
   ]

  [; Equating alias with something else
   (equate/one/substituted Env (AliasTy == Ty))
   (Env ((normalizes-to AliasTy Ty)))
   ]

  [; Equating alias with something else
   (equate/one/substituted Env (Ty == AliasTy))
   (Env ((normalizes-to AliasTy Ty)))
   ]

  [; Otherwise, `T1 == T2` if `T1 <= T2` and `T2 <= T1`
   (equate/one/substituted Env (Parameter_1 == Parameter_2))
   (Env ((Parameter_1 <= Parameter_2) (Parameter_1 >= Parameter_2)))
   ]

  )

(define-metafunction formality-ty
  map-var : Env_in VarId_in Parameter_in -> (Env Goals)
  #:pre (all? (occurs-check-ok? Env_in VarId_in Parameter_in)
              (universe-check-ok? Env_in VarId_in Parameter_in)
              (env-contains-unmapped-existential-var Env_in VarId_in))

  [(map-var Env VarId Parameter)
   (Env_2 Relations)
   (where/error Relations (known-relations Env VarId))
   (where/error Env_1 (remove-var-bounds-from-env Env VarId))
   (where/error Env_2 (env-with-var-mapped-to Env_1 VarId Parameter))
   ]

  )