#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "hook.rkt"
         )
(provide where-clause->goal-from-env
         where-clauses->goals-from-env
         where-clause->hypothesis-from-env
         where-clauses->hypotheses-from-env
         )

(define-metafunction formality-ty
  ;; Convert where clauses `W ...` into goals `G ...` that prove
  ;; those where clauses hold.
  where-clauses->goals-from-env : Env WhereClauses -> Goals

  [(where-clauses->goals-from-env Env (WhereClause ...))
   ((where-clause->goal-from-env Env WhereClause) ...)]
  )

(define-metafunction formality-ty
  ;; Convert a where clause `W` into a goal that proves `W` is true.
  where-clause->goal-from-env : Env WhereClause -> Goal

  [(where-clause->goal-from-env Env WhereClause)
   (where-clause->biformula-from-env Env WhereClause)]

  )

(define-metafunction formality-ty
  ;; Convert where clauses `W ...` into hypotheses that can be
  ;; assumed to be true.
  where-clauses->hypotheses-from-env : Env WhereClauses -> Hypotheses

  [(where-clauses->hypotheses-from-env Env (WhereClause ...))
   ((where-clause->hypothesis-from-env Env WhereClause) ...)]

  )

(define-metafunction formality-ty
  ;; Convert a where clause `W` into a hypothesis that can be
  ;; assumed to be true.
  where-clause->hypothesis-from-env : Env WhereClause -> Hypothesis

  [(where-clause->hypothesis-from-env Env WhereClause)
   (where-clause->biformula-from-env Env WhereClause)]

  )
