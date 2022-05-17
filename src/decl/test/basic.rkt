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

   ((TraitDecl (term (Debug (trait ((type Self)) () ()))))
    (TraitImplDecl (term (impl () (Debug ((scalar-ty i32))) () ())))
    (CrateDecl (term (TheCrate (crate (TraitDecl TraitImplDecl)))))
    (Env (term (env-for-crate-decl CrateDecl)))
    )

   (traced '()
           (decl:test-can-prove
            Env
            (Implemented (Debug ((scalar-ty i32))))))
   )
  )
