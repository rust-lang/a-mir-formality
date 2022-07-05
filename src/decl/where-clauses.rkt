#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         )
(provide where-clause->goal
         where-clauses->goals
         where-clause->hypothesis
         where-clauses->hypotheses
         where-clause->goal∧clause
         )

(define-metafunction formality-decl
  ;; Convert where clauses `W ...` into goals `G ...` that prove
  ;; those where clauses hold.
  where-clauses->goals : CrateDecls WhereClauses -> Goals

  [(where-clauses->goals CrateDecls (WhereClause ...))
   ((where-clause->goal CrateDecls WhereClause) ...)]
  )

(define-metafunction formality-decl
  ;; Convert a where clause `W` into a goal that proves `W` is true.
  where-clause->goal : CrateDecls WhereClause -> Goal

  [(where-clause->goal CrateDecls WhereClause)
   (where-clause->goal∧clause WhereClause)]

  )

(define-metafunction formality-decl
  ;; Convert where clauses `W ...` into hypotheses that can be
  ;; assumed to be true.
  where-clauses->hypotheses : CrateDecls WhereClauses -> Hypotheses

  [(where-clauses->hypotheses CrateDecls (WhereClause ...))
   ((where-clause->hypothesis CrateDecls WhereClause) ...)]

  )

(define-metafunction formality-decl
  ;; Convert a where clause `W` into a hypothesis that can be
  ;; assumed to be true.
  where-clause->hypothesis : CrateDecls WhereClause -> Hypothesis

  [(where-clause->hypothesis CrateDecls WhereClause)
   (where-clause->goal∧clause WhereClause)]

  )

(define-metafunction formality-decl
  ;; Helper function: Converts a where-clause into a `Biformula`; used to
  ;; define the functions in `where-clauses.rkt`, which are the ones you should
  ;; actually invoke.
  where-clause->goal∧clause : WhereClause -> Biformula

  [(where-clause->goal∧clause (∀ KindedVarIds WhereClause))
   (∀ KindedVarIds (where-clause->goal∧clause WhereClause))
   ]

  [(where-clause->goal∧clause (Ty_self : TraitId (Parameter ...)))
   (is-implemented (TraitId (Ty_self Parameter ...)))
   ]

  [(where-clause->goal∧clause (AliasTy == Ty))
   (normalizes-to AliasTy Ty)
   ]

  [(where-clause->goal∧clause ((_ Parameter_a) : (_ Parameter_b)))
   (Parameter_a -outlives- Parameter_b)
   ]

  )