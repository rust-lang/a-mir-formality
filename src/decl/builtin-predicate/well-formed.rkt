#lang racket

(require redex/reduction-semantics
         "../../logic/grammar.rkt"
         "../grammar.rkt"
         )
(provide well-formed-goal
         well-formed-goals
         )

;; WELL-FORMEDNESS CLAUSES
;; -----------------------
;;
;; The `(well-formed (type T))` predicate is kind of magical in a way
;; that manipulates inference. This somewhat violates the layering
;; by introducing non-logical considerations and I don't love it,
;; we may want to tweak the setup.
;;
;; To be more specific, we examine the current inference state for the
;; type T and return a different set of program clauses based on what
;; we see. This avoids two problems:
;;
;; * `(well-formed (type T))` would just enumerate all types T,
;;   which is unproductive; this rule could be generalized to requiring
;;   non-bound inputs.
;; * rules for alias types should not be used arbitrarily. This is really
;;   an optimization. The idea is that, if we have some type
;;   `Foo`, it doesn't make sense to go off and try to prove `Foo`
;;   WF by finding some `<P0 as Trait<P1..Pn>>::Type` that normalizes
;;   to `Foo`. This is true for WF rules in particular because one of
;;   the jobs of an impl is to prove that its associated types are WF based
;;   on the where-clauses in scope.

(define-metafunction formality-decl
  ;; Like `well-formed-goals`, but returns a singular Goal (creating an `&&` goal if needed).
  well-formed-goal : CrateDecls Ty -> Goal

  [(well-formed-goal CrateDecls Ty)
   Goal
   (where [Goal] (well-formed-goals CrateDecls Ty))
   ]

  [(well-formed-goal CrateDecls Ty)
   (&& [Goal ...])
   (where [Goal ...] (well-formed-goals CrateDecls Ty))
   ]
  )

(define-metafunction formality-decl
  ;; Given some type, returns a list of goals which,
  ;; if all true, will prove it to be well-formed.
  ;;
  ;; This is not meant to be invoked on unbound existential
  ;; variables, though it's not harmful if it is. It will
  ;; simply return a generic `well-formed` goal that will
  ;; yield ambiguous when you try to prove it (until that
  ;; variable is later constrained in some other way).
  well-formed-goals : CrateDecls Ty -> Goals

  ;; VARIABLES
  ;;
  ;; We yield false-goal here -- we don't have any rules to prove
  ;; random variables well-formed. If they're universal,
  ;; the rule needs to be in the environment. If they're existential,
  ;; they need to be unified.

  [(well-formed-goals CrateDecls VarId)
   [false-goal]
   ]

  ;; RIGID TYPES

  [(well-formed-goals CrateDecls (rigid-ty AdtId Parameters))
   [(well-formed-adt (rigid-ty AdtId Parameters))]
   ]

  [(well-formed-goals CrateDecls (rigid-ty ScalarId []))
   []
   ]

  [(well-formed-goals CrateDecls (rigid-ty (tuple _) (Ty ...)))
   [(well-formed-goal CrateDecls Ty) ...]
   ]

  [(well-formed-goals CrateDecls (rigid-ty (fn-ptr _ _) (Ty ...)))
   [(well-formed-goal CrateDecls Ty) ...]
   ]

  [(well-formed-goals CrateDecls (rigid-ty (ref _) (Lt Ty)))
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

  [(well-formed-goals CrateDecls (alias-ty AliasName Parameters))
   [(well-formed-alias (alias-ty AliasName Parameters))]
   ]

  ;; PREDICATE TYPES

  [(well-formed-goals CrateDecls (∀ KindedVarIds Ty))
   [(∀ KindedVarIds (&& (well-formed-goals Ty)))]
   ]

  [(well-formed-goals CrateDecls (∃ KindedVarIds Ty))
   [false-goal] ; FIXME -- WF rules for existential types
   ]

  [(well-formed-goals CrateDecls (implies Biformulas Ty))
   [(implies Biformulas
             (well-formed-goal CrateDecls Ty))]
   ]

  [(well-formed-goals CrateDecls (ensures Ty Biformulas))
   (flatten [Biformulas (well-formed-goals CrateDecls Ty)])
   ]
  )