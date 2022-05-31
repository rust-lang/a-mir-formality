#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../prove.rkt"
         "../../util.rkt"
         "libcore.rkt")

(module+ test
  (redex-let*
   formality-decl

   ((; struct Foo { }
     AdtDecl_Foo (term (Foo (struct () () ((Foo ())))))))

   (; test that Send check fails
    redex-let*
    formality-decl
    [(; static S: Foo;
      StaticDecl (term (S (static () () (rigid-ty Foo ())))))
     (CrateDecl (term (TheCrate (crate (AdtDecl_Foo StaticDecl)))))
     ]

    (traced '()
            (decl:test-crate-decl-not-ok (core-crate-decl CrateDecl) TheCrate)
            ))


   (; test that Send check fails
    redex-let*
    formality-decl
    [(; static S: Foo;
      StaticDecl (term (S (static () () (rigid-ty Foo ())))))
     (; impl Send for Foo
      TraitImplDecl_Send (term (impl ((type T)) (rust:Send ((rigid-ty Foo ()))) () ())))
     (CrateDecl (term (TheCrate (crate (AdtDecl_Foo StaticDecl TraitImplDecl_Send)))))
     ]

    (traced '()
            (decl:test-crate-decl-ok (core-crate-decl CrateDecl) TheCrate)
            ))
   )
  )