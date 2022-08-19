#lang racket
(require redex/reduction-semantics
         "../logic/cosld-solve.rkt"
         "../decl/decl-ok.rkt"
         "../decl/env.rkt"
         "../decl/grammar.rkt"
         "grammar.rkt"
         )
(provide prove-goal
         prove-goal-in-env
         prove-crate-item-ok
         cannot-prove-goal-in-env
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

(define-judgment-form
  formality-check

  #:mode (prove-goal-in-env I I O)
  #:contract (prove-goal-in-env Env Goal Env_out)

  [(logic:prove-top-level-goal/cosld Env Goal Env_out)
   ----------------------------------------
   (prove-goal-in-env Env Goal Env_out)
   ]
  )

(define-judgment-form
  formality-check

  ;; Checks that we cannot prove the given goal in the given environment.
  ;;
  ;; Relies on a property called "negation-as-failure" -- the caller has to interpret what this means.

  #:mode (cannot-prove-goal-in-env I I)
  #:contract (cannot-prove-goal-in-env Env Goal)

  [(side-condition ,(empty? (judgment-holds (logic:prove-top-level-goal/cosld Env Goal Env_out)
                                            Env_out
                                            )))
   ----------------------------------------
   (cannot-prove-goal-in-env Env Goal)
   ]
  )
