#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../logic/env.rkt"
         "decl-to-clause/crate-decl.rkt"
         "decl-to-clause/default-rules.rkt")
(provide decl-invariants
         decl-clauses-for-predicate
         )

(define-metafunction formality-decl
  ;; Part of the "hook" for a formality-decl program:
  ;;
  ;; Create the clauses for solving a given predicate
  ;; (right now the predicate is not used).
  decl-clauses-for-predicate : Env DeclProgram Predicate -> Clauses

  [(decl-clauses-for-predicate Env DeclProgram Predicate)
   Clauses
   (where/error (Clauses _) (program-rules DeclProgram))]
  )

(define-metafunction formality-decl
  ;; Part of the "hook" for a formality-decl program:
  ;;
  ;; Create the invariants from a given program.
  decl-invariants : DeclProgram -> Invariants

  [(decl-invariants DeclProgram)
   Invariants
   (where/error (_ Invariants) (program-rules DeclProgram))]
  )

(define-metafunction formality-decl
  ;; Return the clauses/hypothesis from multiple crates
  ;; with CrateId as the crate being compiled.
  ;;
  ;; NB: This assumes that we can compile to a complete set of
  ;; clauses. This will eventually not suffice, e.g., with
  ;; auto traits. But this helper is private, so we can refactor
  ;; that later.
  program-rules : DeclProgram -> (Clauses Invariants)

  [(program-rules (CrateDecls CrateId))
   ((flatten (Clauses_c ... Clauses_bi))
    (flatten (Invariants_c ... Invariants_bi)))
   (where (CrateDecl ...) CrateDecls)
   (where/error (Clauses_bi Invariants_bi) (default-rules ()))
   (where/error ((Clauses_c Invariants_c) ...) ((crate-decl-rules CrateDecls CrateDecl CrateId) ...))
   ]
  )


