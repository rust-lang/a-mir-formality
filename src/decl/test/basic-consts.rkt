#lang racket
(require redex/reduction-semantics
         "../decl-to-clause.rkt"
         "../decl-ok.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../../util.rkt")

(module+ test
  ;; Program:
  ;;
  ;; trait Debug { }
  ;; struct Foo<T: Debug> { }
  ;; const MALFORMED<T>: Foo<T>;
  (redex-let*
   formality-decl

   ((TraitDecl (term (Debug (trait ((TyKind Self)) () ()))))
    (AdtDecl_Foo (term (Foo (struct ((TyKind T)) ((Implemented (Debug (T)))) ((Foo ()))))))
    (ConstDecl_Malformed (term (Malformed (const ((TyKind T)) () (TyRigid Foo (T))))))
    (CrateDecl (term (TheCrate (crate (TraitDecl AdtDecl_Foo ConstDecl_Malformed)))))
    (Env (term (env-for-crate-decl CrateDecl)))
    )

   (traced '()
           (decl:test-cannot-prove
            Env
            (crate-ok-goal (CrateDecl) CrateDecl)))
   )
  )
