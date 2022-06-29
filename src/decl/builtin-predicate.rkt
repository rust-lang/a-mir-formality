#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../logic/env.rkt"
         "../ty/hook.rkt"
         "builtin-predicate/well-formed.rkt"
         )
(provide decl:categorize-goal
         decl:solve-builtin-predicate
         well-formed-goal-for-ty
         )

(define-metafunction formality-decl
  ;; Part of the "hook" for a formality-decl program:
  ;;
  ;; Extends ty:categorize-goal with a knowledge of which predicates
  ;; are builtin by the decl layer.
  decl:categorize-goal : Goal -> Goal/Categorization

  [(decl:categorize-goal (well-formed (type _)))
   builtin-predicate
   ]

  [(decl:categorize-goal Goal)
   (ty:categorize-goal Goal)]

  )

(define-metafunction formality-decl
  ;; Part of the "hook" for a formality-decl program:
  ;;
  ;; Breaks down a `(well-formed (type Foo))` into goals.
  decl:solve-builtin-predicate : CrateDecls Env Predicate -> (Env Goals) or Error

  [; given an unmapped existential variable, we can't solve until more
   ; type info is available, so yield an `ambiguous` goal
   (decl:solve-builtin-predicate CrateDecls Env (well-formed (type VarId)))
   (Env [ambiguous])

   (where #t (env-contains-unmapped-existential-var Env VarId))
   ]

  [(decl:solve-builtin-predicate CrateDecls Env (well-formed (type Ty)))
   (Env (well-formed-goals CrateDecls Ty))
   ]
  )

(define-metafunction formality-decl
  ;; Create the goals to make a type well-formed. This is used
  ;; as part of proving the values of associated types are valid.
  well-formed-goal-for-ty : CrateDecls Ty -> Goal

  [(well-formed-goal-for-ty CrateDecls VarId)
   (well-formed (type VarId))
   ]

  [(well-formed-goal-for-ty CrateDecls Ty)
   (well-formed-goal CrateDecls Ty)
   ]
  )