#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         )
(provide where-clause->goal
         where-clauses->goals
         where-clause->hypothesis
         where-clauses->hypotheses
         )

; FIXME: Right now, where clauses are a syntactic subset of goals and
; clauses. Do we want to commit to that?

(define-metafunction formality-ty
  ;; Convert where clauses `W ...` into goals `G ...` that prove
  ;; those where clauses hold.
  where-clauses->goals : WhereClauses -> Goals

  [(where-clauses->goals (WhereClause ...))
   ((where-clause->goal WhereClause) ...)]
  )

(define-metafunction formality-ty
  ;; Convert a where clause `W` into a goal that proves `W` is true.
  where-clause->goal : WhereClause -> Goal

  [(where-clause->goal WhereClause)
   (where-clause->goal∧clause WhereClause)]

  )

(define-metafunction formality-ty
  ;; Convert where clauses `W ...` into hypotheses that can be
  ;; assumed to be true.
  where-clauses->hypotheses : WhereClauses -> Hypotheses

  [(where-clauses->hypotheses (WhereClause ...))
   ((where-clause->hypothesis WhereClause) ...)]

  )

(define-metafunction formality-ty
  ;; Convert a where clause `W` into a hypothesis that can be
  ;; assumed to be true.
  where-clause->hypothesis : WhereClause -> Hypothesis

  [(where-clause->hypothesis WhereClause)
   (where-clause->goal∧clause WhereClause)]

  )

(define-metafunction formality-ty
  where-clause->goal∧clause : WhereClause -> Goal∧Clause

  [(where-clause->goal∧clause (∀ KindedVarIds WhereClause))
   (∀ KindedVarIds (where-clause->goal∧clause WhereClause))
   ]

  [(where-clause->goal∧clause (Ty_self : TraitId (Parameter ...)))
   (is-implemented (TraitId (Ty_self Parameter ...)))
   ]

  [(where-clause->goal∧clause (AliasTy == Ty))
   (normalizes-to AliasTy Ty)
   ]

  [(where-clause->goal∧clause (Parameter_a : Parameter_b))
   (Parameter_a -outlives- Parameter_b)
   ]

  )

(define-metafunction formality-ty
  where-clauses->goals∧clauses : WhereClauses -> Goals∧Clauses

  [(where-clauses->goals∧clauses (WhereClause ...))
   ((where-clause->goal∧clause WhereClause) ...)
   ]

  )