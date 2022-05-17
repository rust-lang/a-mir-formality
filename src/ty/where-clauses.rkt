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
  ;; Convert a set of where clauses into a set of goals that prove
  ;; those where clauses hold.
  where-clauses->goals : WhereClauses -> Goals

  [(where-clauses->goals (WhereClause ...))
   ((where-clause->goal WhereClause) ...)]
  )

(define-metafunction formality-ty
  ;; Convert a where clause `W` into a goal that proves `W` is true.
  where-clause->goal : WhereClause -> Goal

  [(where-clause->goal (∀ KindedVarIds WhereClause))
   (∀ KindedVarIds (where-clause->goal WhereClause))
   ]

  [(where-clause->goal (is-implemented TraitRef))
   (is-implemented TraitRef)
   ]

  [(where-clause->goal (normalizes-to AliasTy Ty))
   (normalizes-to AliasTy Ty)
   ]

  [(where-clause->goal (Outlives (Parameter_a : Parameter_b)))
   (Parameter_a -outlives- Parameter_b)
   ]

  )

(define-metafunction formality-ty
  ;; Convert a where clause `W` into a hypothesis that code which is
  ;; implied by `W` can assume to be true.
  where-clauses->hypotheses : WhereClauses -> Hypotheses

  [(where-clauses->hypotheses (WhereClause ...))
   ((where-clause->hypothesis WhereClause) ...)]

  )

(define-metafunction formality-ty
  ;; Convert a where clause `W` into a hypothesis that code which is
  ;; implied by `W` can assume to be true.
  where-clause->hypothesis : WhereClause -> Hypothesis

  [(where-clause->hypothesis WhereClause)
   ; Hack: we know that we restrict ourselves to the subset of things that can be
   ; interpreted either way, so just reuse `where-clause->goal`.
   (where-clause->goal WhereClause)]

  )