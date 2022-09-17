#lang racket
(require redex/reduction-semantics
         "../logic/env.rkt"
         "../logic/querify.rkt"
         "../logic/solve-query.rkt"
         "../decl/env.rkt"
         "../check/all-check.rkt"
         "grammar.rkt"
         "lower-to-decl/program.rkt"
         "lower-to-decl/where-clause.rkt"
         )
(provide rust:query)

(define-metafunction formality-rust
  ;; Convenient metafunction for tests:
  ;;
  ;; Given a rust program, returns #t if passes type checks.
  rust:query : Rust/Program Rust/QueryTest -> Solutions

  [(rust:query Rust/Program Rust/QueryTest)
   (logic:solve-query (env-hook Env) QueryGoal)
   (where/error (CrateDecls CrateId) (lower-to-decl/Program Rust/Program))
   (where/error Env (env-for-crate-decls CrateDecls CrateId))
   (where/error (QueryGoal _) (querify-test [] Env Rust/QueryTest))
   ]

  )

(define-metafunction formality-rust
  ;; Convert a Rust/QueryTest into a query goal.
  ;;
  ;; * `KindedVarIds` -- the new names (and their kinds) introduced into env thus far
  ;; * `Env` -- env we have built up thus far
  ;; * `QueryTest` -- query test given by user
  querify-test : KindedVarIds Env Rust/QueryTest -> (QueryGoal UniversePairs)

  ;; Exists -- introduce new existential variables into the environment
  ;;
  ;; We do not rename them, so we assume no shadowing.
  [(querify-test [KindedVarId_in ...] Env (?∃ [KindedVarId ...] Rust/QueryTest))
   (querify-test [KindedVarId_in ... KindedVarId ...] Env_1 Rust/QueryTest)
   (where/error Env_1 (env-with-vars-in-current-universe Env ∃ [KindedVarId ...]))
   ]

  ;; ForAll -- introduce new placeholders into the environment, along with hypotheses that they are WF
  ;;
  ;; We do not rename them, so we assume no shadowing.
  [(querify-test [KindedVarId_in ...] Env (?∀ [KindedVarId ...] Rust/QueryTest))
   (querify-test [KindedVarId_in ... KindedVarId ...] Env_3 Rust/QueryTest)

   (where/error Env_1 (env-with-incremented-universe Env))
   (where/error Env_2 (env-with-vars-in-current-universe Env_1 ∀ [KindedVarId ...]))
   (where/error Env_3 (env-with-hypotheses Env_2 [(well-formed KindedVarId) ...]))
   ]

  ;; Hypotheses
  [(querify-test KindedVarIds_in Env (?=> Hypotheses Rust/QueryTest))
   (querify-test KindedVarIds_in Env_1 Rust/QueryTest)
   (where/error Env_1 (env-with-hypotheses Env Hypotheses))
   ]

  [(querify-test KindedVarIds_in Env Rust/WhereClause)
   (querify-goal Env (lower-to-decl/WhereClause KindedVarIds_in Rust/WhereClause))
   ]
  )

