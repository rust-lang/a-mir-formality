#lang racket
(require redex/reduction-semantics
         "../decl/prove.rkt"
         "../decl/env.rkt"
         "../decl/where-clauses.rkt"
         "grammar.rkt"
         "lower.rkt"
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
   (decl:is-crate-ok CrateDecls CrateId)
   (where/error (CrateDecls CrateId) (rust→decl/Program Rust/Program))
   ]

  )

(define-metafunction formality-rust
  ;; Convenient metafunction for tests:
  ;;
  ;; Given a rust program, returns #t if passes type checks.
  rust:can-prove-where-clause-in-program : Rust/Program Rust/WhereClause -> boolean

  [(rust:can-prove-where-clause-in-program Rust/Program Rust/WhereClause)
   (decl:can-prove-goal CrateDecls CrateId Goal)
   (where/error (CrateDecls CrateId) (rust→decl/Program Rust/Program))
   (where/error WhereClause (rust→decl/WhereClause Rust/WhereClause))
   (where/error Goal (where-clause->goal CrateDecls WhereClause))
   ]

  )

(define-metafunction formality-rust
  ;; Convenient metafunction for tests:
  ;;
  ;; Given a rust program, returns #t if passes type checks.
  rust:can-prove-goal-in-program : Rust/Program Goal -> boolean

  [(rust:can-prove-goal-in-program Rust/Program Goal)
   (decl:can-prove-goal CrateDecls CrateId Goal)
   (where/error (CrateDecls CrateId) (rust→decl/Program Rust/Program))
   ]

  )