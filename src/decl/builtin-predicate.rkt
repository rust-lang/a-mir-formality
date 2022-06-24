#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../logic/env.rkt"
         "builtin-predicate/well-formed.rkt"
         )
(provide decl:is-builtin-predicate?
         decl:solve-builtin-predicate
         )

(define-metafunction formality-decl
  ;; Part of the "hook" for a formality-decl program:
  ;;
  ;; Create the clauses for solving a given predicate
  ;; (right now the predicate is not used).
  decl:is-builtin-predicate? : Goal -> boolean

  [(decl:is-builtin-predicate? (well-formed (type _)))
   #t
   ]

  [(decl:is-builtin-predicate? _)
   #f]
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