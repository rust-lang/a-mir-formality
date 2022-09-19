#lang racket

(require redex/reduction-semantics
         "../../logic/grammar.rkt"
         "../grammar.rkt"
         )
(provide well-formed-goal-for-ty
         well-formed-goal-for-lt
         well-formed-goal-for-parameter
         well-formed-subgoal-for-ty
         well-formed-subgoals-for-ty
         )

(define-metafunction formality-decl
  ;; Create the goals to make a type well-formed. This is used
  ;; as part of proving the values of associated types are valid.
  well-formed-goal-for-ty : CrateDecls Ty -> Goal

  [(well-formed-goal-for-ty CrateDecls VarId)
   (well-formed (type VarId))
   ]

  [(well-formed-goal-for-ty CrateDecls Ty)
   (well-formed-subgoal-for-ty CrateDecls Ty)
   ]
  )

(define-metafunction formality-decl
  ;; Create the goals to make a lifetime well-formed. This is used
  ;; as part of proving the values of associated types are valid.
  well-formed-goal-for-lt : CrateDecls Lt -> Goal

  [(well-formed-goal-for-lt CrateDecls static)
   true-goal
   ]

  [(well-formed-goal-for-lt CrateDecls VarId)
   (well-formed (lifetime VarId))
   ]
  )

(define-metafunction formality-decl
  ;; Create the goals to make a lifetime well-formed. This is used
  ;; as part of proving the values of associated types are valid.
  well-formed-goal-for-parameter : CrateDecls KindedVarIds Parameter -> Goal

  [(well-formed-goal-for-parameter CrateDecls (_ ... (ParameterKind VarId) _ ...) VarId)
   (well-formed (ParameterKind VarId))
   ]

  [(well-formed-goal-for-parameter CrateDecls _ Ty)
   (well-formed-goal-for-ty CrateDecls Ty)
   ]

  [(well-formed-goal-for-parameter CrateDecls _ Lt)
   (well-formed-goal-for-lt CrateDecls Lt)
   ]

  )

;; WELL-FORMEDNESS SUBGOALS
;; ------------------------
;;
;; The `well-formed-subgoal-for-ty` gives the rules that make a given
;; type "syntactically well-formed". By syntactically, we mean that it's
;; valid to name the type in that particular way. This distinction can be
;; important with aliases: an alias may be ill-formed (e.g., because the trait
;; where clauses are not satisfied) even if it's normalized form is well-formed
;; (presuming we could normalize it).

(define-metafunction formality-decl
  ;; Like `well-formed-goals`, but returns a singular Goal (creating an `&&` goal if needed).
  well-formed-subgoal-for-ty : CrateDecls Ty -> Goal

  [(well-formed-subgoal-for-ty CrateDecls Ty)
   Goal
   (where [Goal] (well-formed-subgoals-for-ty CrateDecls Ty))
   ]

  [(well-formed-subgoal-for-ty CrateDecls Ty)
   (&& [Goal ...])
   (where [Goal ...] (well-formed-subgoals-for-ty CrateDecls Ty))
   ]
  )

(define-metafunction formality-decl
  ;; Given some type, returns a list of goals which,
  ;; if all true, will prove it to be well-formed based on the
  ;; language's built-in rules.
  ;;
  ;; NB: For a `VarId`, returns false-goal! If this is a universal
  ;; placeholder, there are no buitlin rules, well-formedness
  ;; comes from the environment. If this is an unbound inference
  ;; variable, we don't
  well-formed-subgoals-for-ty : CrateDecls Ty -> Goals

  ;; VARIABLES
  ;;
  ;; We don't have a rule for variables intentionally. Our caller is expected
  ;; to handle that case.
  ;; We yield false-goal here -- we don't have any rules to prove
  ;; random variables well-formed. If they're universal,
  ;; the rule needs to be in the environment. If they're existential,
  ;; they need to be unified.

  [(well-formed-subgoals-for-ty CrateDecls VarId)
   [false-goal]
   ]

  ;; RIGID TYPES

  [(well-formed-subgoals-for-ty CrateDecls (rigid-ty AdtId Parameters))
   [(well-formed-adt (rigid-ty AdtId Parameters))]
   ]

  [(well-formed-subgoals-for-ty CrateDecls (rigid-ty ScalarId []))
   []
   ]

  [(well-formed-subgoals-for-ty CrateDecls (rigid-ty (tuple _) (Ty ...)))
   [(well-formed (type Ty)) ...]
   ]

  [(well-formed-subgoals-for-ty CrateDecls (rigid-ty (fn-ptr _ _) (Ty ...)))
   [(well-formed (type Ty)) ...]
   ]

  [(well-formed-subgoals-for-ty CrateDecls (rigid-ty (ref _) (Lt Ty)))
   [(Ty -outlives- Lt)]
   ]

  ;; ALIAS TYPES
  ;;
  ;; The behavior here is a key point of departure from the normal solver, and in fact
  ;; the entire reason this predicate is builtin. If we are verifying that
  ;; an alias type is well-formed, we do so by checking that the trait is implemented
  ;; and the parameters meet their where clauses. It's then the *impl's* job to have
  ;; ensured it is WF. The default solver rules, if naively applied, would wind up
  ;; with us trying to unify the alias against every other well-formed type, looking
  ;; for one where normalization succeeds, which is a rathole that leads the solver
  ;; way off course.

  [(well-formed-subgoals-for-ty CrateDecls (alias-ty AliasName Parameters))
   [(well-formed-alias (alias-ty AliasName Parameters))]
   ]

  ;; PREDICATE TYPES

  [(well-formed-subgoals-for-ty CrateDecls (∀ KindedVarIds Ty))
   [(∀ KindedVarIds (well-formed (type Ty)))]
   ]

  [(well-formed-subgoals-for-ty CrateDecls (∃ KindedVarIds Ty))
   [false-goal] ; FIXME -- WF rules for existential types
   ]

  [(well-formed-subgoals-for-ty CrateDecls (implies Biformulas Ty))
   [(implies Biformulas
             (well-formed (type Ty)))]
   ]

  [(well-formed-subgoals-for-ty CrateDecls (ensures Ty [Biformula ...]))
   [Biformula ... (well-formed (type Ty))]
   ]
  )