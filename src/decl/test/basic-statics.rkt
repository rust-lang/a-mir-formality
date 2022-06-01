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
    ;
    ; FIXME(#26)  when we support auto traits, we'll need a neg impl here
    redex-let*
    formality-decl
    [(; static S: Foo;
      StaticDecl (term (S (static () () (rigid-ty Foo ())))))
     (CrateDecl (term (TheCrate (crate (AdtDecl_Foo StaticDecl)))))
     ]

    (traced '()
            (decl:test-crate-decl-not-ok (core-crate-decl CrateDecl) TheCrate)
            ))


   (; test that Sync check succeeds when an impl is present
    ;
    ; FIXME(#26)  when we support auto traits, we won't need an impl here
    redex-let*
    formality-decl
    [(; static S: Foo;
      StaticDecl (term (S (static () () (rigid-ty Foo ())))))
     (; impl Send for Foo
      TraitImplDecl_Sync (term (impl ((type T)) (rust:Sync ((rigid-ty Foo ()))) () ())))
     (CrateDecl (term (TheCrate (crate (AdtDecl_Foo StaticDecl TraitImplDecl_Sync)))))
     ]

    (traced '()
            (decl:test-crate-decl-ok (core-crate-decl CrateDecl) TheCrate)
            ))
   )
  )