#lang racket
(require redex/reduction-semantics
         "../decl-to-clause.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../../ty/user-ty.rkt"
         "../../util.rkt")

(module+ test
  ;; Program:
  ;;
  ;; trait Debug { }
  ;; impl Debug for i32 { }
  (redex-let*
   formality-decl

   ((TraitDecl (term (trait Debug ((type Self)) where () {})))
    (TraitImplDecl (term (impl () (Debug ((user-ty i32))) where () {})))
    (CrateDecl (term (crate TheCrate {TraitDecl TraitImplDecl})))
    )

   (traced '()
           (test-equal
            (term (decl:can-prove-goal [CrateDecl] TheCrate (is-implemented (Debug[(user-ty i32)]))))
            #t))
   )
  )