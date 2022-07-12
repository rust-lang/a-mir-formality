#lang racket
(require redex/reduction-semantics
         "../logic/cosld-solve.rkt"
         "../decl/decl-ok.rkt"
         "../decl/env.rkt"
         "../decl/grammar.rkt"
         "grammar.rkt"
         )
(provide prove-goal
         prove-crate-item-ok
         )


(define-judgment-form
  formality-check

  #:mode (prove-crate-item-ok I I)
  #:contract (prove-crate-item-ok DeclProgram CrateItemDecl)

  [(where/error Goal_ok (crate-item-ok-goal (crate-decls DeclProgram) CrateItemDecl))
   (where/error [Goal_lang-ok ...] (lang-item-ok-goals (crate-decls DeclProgram) CrateItemDecl))
   (prove-goal DeclProgram Goal_ok)
   (prove-goal DeclProgram Goal_lang-ok) ...
   ----------------------------------------
   (prove-crate-item-ok DeclProgram CrateItemDecl)
   ]
  )

(define-judgment-form
  formality-check

  #:mode (prove-goal I I)
  #:contract (prove-goal DeclProgram Goal)

  [(where/error Env (env-for-decl-program DeclProgram))
   (logic:prove-top-level-goal/cosld Env Goal Env_1)
   ----------------------------------------
   (prove-goal DeclProgram Goal)
   ]
  )