#lang racket
(require redex/reduction-semantics
         "grammar-extended.rkt"
         "type-check-goal.rkt"
         "../decl/decl-ok.rkt"
         "../logic/cosld-solve.rkt"
         )
(provide prove-goal
         prove-crate-item-ok
         )


(define-judgment-form
  formality-mir-extended

  #:mode (prove-crate-item-ok I I)
  #:contract (prove-crate-item-ok DeclProgram CrateItemDecl)

  [(where/error Goal_ok (crate-item-ok-goal (crate-decls DeclProgram) CrateItemDecl))
   (prove-goal DeclProgram Goal_ok)
   ----------------------------------------
   (prove-crate-item-ok DeclProgram CrateItemDecl)
   ]
  )

(define-judgment-form
  formality-mir-extended

  #:mode (prove-goal I I)
  #:contract (prove-goal DeclProgram Goal)

  [(where/error (CrateDecls CrateId) DeclProgram)
   (where/error Env (env-for-crate-decls CrateDecls CrateId))
   (logic:prove-top-level-goal/cosld Env Goal Env)
   ----------------------------------------
   (prove-goal DeclProgram Goal)
   ]
  )