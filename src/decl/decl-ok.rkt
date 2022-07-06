#lang racket
(require redex/reduction-semantics
         "../logic/env.rkt"
         "grammar.rkt"
         "decl-ok/crate-item.rkt"
         "decl-ok/lang-item.rkt"
         )
(provide crate-item-ok-goal
         lang-item-ok-goals
         crate-ok-goal
         )

(define-metafunction formality-decl
  ;; Given a set of crates and the decl for the current crate,
  ;; generate the goal that proves all declarations in the current crate are
  ;; "ok". Other crates are assumed to be "ok".
  crate-ok-goal : CrateDecls_cs CrateDecl_c -> Goal

  #:pre (in? CrateDecl_c CrateDecls_cs)

  [(crate-ok-goal CrateDecls (crate CrateId (CrateItemDecl ...)))
   (&& (Goal_regular ... Goal_lang-item ... ...))

   (where/error (Goal_regular ...) ((crate-item-ok-goal CrateDecls CrateItemDecl) ...))
   (where/error ((Goal_lang-item ...) ...) ((lang-item-ok-goals CrateDecls CrateItemDecl) ...))
   ]
  )
