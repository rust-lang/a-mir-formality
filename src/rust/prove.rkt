#lang racket
(require redex/reduction-semantics
         "../decl/prove.rkt"
         "../mir/all-check.rkt"
         "grammar.rkt"
         "lower-to-decl/program.rkt"
         "lower-to-decl/where-clause.rkt"
         )
(provide (all-defined-out))

(define-metafunction formality-rust
  ;; Convenient metafunction for tests:
  ;;
  ;; Given a set of declarations, make them into a crate named core
  ;; and test that the program is ok.
  rust:is-core-crate-ok : Rust/CrateItemDecls -> boolean

  [(rust:is-core-crate-ok Rust/CrateItemDecls)
   (rust:is-program-ok ([(crate core Rust/CrateItemDecls)] core))]

  )

(define-metafunction formality-rust
  ;; Convenient metafunction for tests:
  ;;
  ;; Given a rust program, returns #t if passes type checks.
  rust:is-program-ok : Rust/Program -> boolean

  [(rust:is-program-ok Rust/Program)
   ,(judgment-holds (✅-Program DeclProgram))
   (where/error DeclProgram (lower-to-decl/Program Rust/Program))
   ]

  )

(define-metafunction formality-rust
  ;; Convenient metafunction for tests:
  ;;
  ;; Given a rust program, returns #t if the where-clause can be proven true.
  ;;
  ;; You can also write forall and implication like: `(∀ [(type T)] where [(T : Debug[])] )
  rust:can-prove-where-clause-in-program : Rust/Program Rust/WhereClauseHr -> boolean

  [(rust:can-prove-where-clause-in-program Rust/Program Rust/WhereClause)
   (decl:can-prove-goal CrateDecls CrateId Goal)
   (where/error (CrateDecls CrateId) (lower-to-decl/Program Rust/Program))
   (where/error Goal (lower-to-decl/WhereClause Rust/WhereClause))
   ]

  [(rust:can-prove-where-clause-in-program Rust/Program (∀ KindedVarIds where [Rust/WhereClause_h ...] Rust/WhereClause_g))
   (decl:can-prove-goal CrateDecls CrateId (∀ KindedVarIds (implies Hypotheses Goal)))
   (where/error (CrateDecls CrateId) (lower-to-decl/Program Rust/Program))
   (where/error Goal (lower-to-decl/WhereClause Rust/WhereClause_g))
   (where/error Hypotheses [(lower-to-decl/WhereClause Rust/WhereClause_h) ...])
   ]

  )

(define-metafunction formality-rust
  ;; Convenient metafunction for tests:
  ;;
  ;; Given a rust program, returns #t if passes type checks.
  rust:can-prove-goal-in-program : Rust/Program Goal -> boolean

  [(rust:can-prove-goal-in-program Rust/Program Goal)
   (decl:can-prove-goal CrateDecls CrateId Goal)
   (where/error (CrateDecls CrateId) (lower-to-decl/Program Rust/Program))
   ]

  )