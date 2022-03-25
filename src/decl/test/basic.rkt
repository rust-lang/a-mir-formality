#lang racket
(require redex/reduction-semantics
         "../decl-to-clause.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../../util.rkt")

(module+ test
  ;; Program:
  ;;
  ;; trait Debug { }
  ;; impl Debug for i32 { }
  (redex-let*
   formality-decl

   ((TraitDecl (term (Debug (trait ((TyKind Self)) () ()))))
    (TraitImplDecl (term (impl () (Debug ((scalar-ty i32))) () ())))
    (CrateDecl (term (TheCrate (crate (TraitDecl TraitImplDecl)))))
    (Env (term (env-for-crate-decl CrateDecl)))
    )

   (traced '()
           (test-equal
            (judgment-holds (decl:prove-top-level-goal/cosld
                             Env
                             (Implemented (Debug ((scalar-ty i32))))
                             Env_out)
                            Env_out)
            (term (Env))))
   )
  )

