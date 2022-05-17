#lang racket
(require redex/reduction-semantics
         "../decl-to-clause.rkt"
         "../decl-ok.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../../util.rkt")

(module+ test
  (current-traced-metafunctions '(relate/one compare/one/substituted equate/one/substituted))
  ;; Program:
  ;;
  ;; trait Debug { }
  ;; struct Foo<T: Debug> { }
  ;; const MALFORMED<T>: Foo<T>;
  (redex-let*
   formality-decl

   ((TraitDecl (term (Debug (trait ((type Self)) () ()))))
    (AdtDecl_Foo (term (Foo (struct ((type T)) ((T : Debug())) ((Foo ()))))))
    (ConstDecl_Malformed (term (Malformed (const ((type T)) () (rigid-ty Foo (T))))))
    (CrateDecl (term (TheCrate (crate (TraitDecl AdtDecl_Foo ConstDecl_Malformed)))))
    (Env (term (env-for-crate-decl CrateDecl)))
    )

   (traced '()
           (decl:test-cannot-prove
            Env
            (crate-ok-goal (CrateDecl) CrateDecl)))
   )
  )
