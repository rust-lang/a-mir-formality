#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../logic/env.rkt"
         "../logic/substitution.rkt"
         "../ty/hook.rkt"
         "well-formed/parameter.rkt"
         )
(provide decl:categorize-goal
         decl:solve-builtin-predicate
         )

(define-metafunction formality-decl
  ;; Part of the "hook" for a formality-decl program:
  ;;
  ;; Extends ty:categorize-goal with information about of which predicates
  ;; are builtin by the decl layer.
  decl:categorize-goal : Env Goal -> Goal/Categorization

  [(decl:categorize-goal Env (well-formed (type _)))
   builtin-predicate
   ]

  [(decl:categorize-goal Env (well-formed (lifetime _)))
   builtin-predicate
   ]

  [(decl:categorize-goal Env (in-scope _))
   builtin-predicate
   ]

  ; We don't let you prove `?T: Foo` where `?T` is an unbound inference variable.
  [(decl:categorize-goal Env (is-implemented (TraitId [VarId _ ...])))
   ambiguous-goal
   (where VarId_1 (apply-substitution-from-env Env VarId))
   (where #t (env-contains-unmapped-existential-var Env VarId_1))
   ]

  [(decl:categorize-goal Env Goal)
   (ty:categorize-goal Env Goal)]

  )

(define-metafunction formality-decl
  ;; Part of the "hook" for a formality-decl program:
  ;;
  ;; Breaks down a `(well-formed (type Foo))` into goals.
  decl:solve-builtin-predicate : CrateDecls Env Predicate -> (Env Goals) or Error

  [; The `(well-formed (type T))` predicate is kind of magical in a way
   ; that manipulates inference. This somewhat violates the layering
   ; by introducing non-logical considerations and I don't love it,
   ; we may want to tweak the setup.
   ;
   ; To be more specific, we examine the current inference state for the
   ; type T and return a different set of program clauses based on what
   ; we see. This avoids two problems:
   ;
   ; * `(well-formed (type T))` would just enumerate all types T,
   ;   which is unproductive; this rule could be generalized to requiring
   ;   non-bound inputs.
   ; * rules for alias types should not be used arbitrarily. This is really
   ;   an optimization. The idea is that, if we have some type
   ;   `Foo`, it doesn't make sense to go off and try to prove `Foo`
   ;   WF by finding some `<P0 as Trait<P1..Pn>>::Type` that normalizes
   ;   to `Foo`. This is true for WF rules in particular because one of
   ;   the jobs of an impl is to prove that its associated types are WF based
   ;   on the where-clauses in scope.
   ;
   ; given an unmapped existential variable, we can't solve until more
   ; type info is available, so yield an `ambiguous` goal
   (decl:solve-builtin-predicate CrateDecls Env (well-formed (type VarId)))
   (Env [ambiguous])

   (where #t (env-contains-unmapped-existential-var Env VarId))
   ]

  [; (well-formed KP) for anything but unbound existential variables
   (decl:solve-builtin-predicate CrateDecls Env (well-formed (type Ty)))
   (Env (well-formed-subgoals-for-ty CrateDecls Ty))
   ]

  [; (well-formed (lifetime t)) is always true, we don't have ill-formed lifetimes
   (decl:solve-builtin-predicate CrateDecls Env (well-formed (lifetime Ty)))
   (Env [])
   ]

  [; (in-scope KP) :- (well-formed KP)
   (decl:solve-builtin-predicate CrateDecls Env (in-scope KindedParameter))
   (Env [(well-formed KindedParameter)])
   ]

  )