#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../prove.rkt"
         "../../util.rkt"
         "../../ty/user-ty.rkt"
         "libcore.rkt")

(module+ test
  (redex-let*
   formality-decl

   [(; struct Foo { }
     AdtDecl_Foo (term (struct Foo () where () ((Foo ())))))
    ]

   (; test that Send check fails
    ;
    ; FIXME(#26)  when we support auto traits, we'll need a neg impl here
    redex-let*
    formality-decl
    [(; static S: Foo;
      StaticDecl (term (static S[] where [] : (rigid-ty Foo ()) = fn-body)))

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
      StaticDecl (term (static S[] where [] : (rigid-ty Foo[]) = dummy-body)))
     (; impl Send for Foo
      TraitImplDecl_Sync (term (impl ((type T)) (core:Sync ((rigid-ty Foo[]))) where () ())))
     (CrateDecl (term (TheCrate (crate (AdtDecl_Foo StaticDecl TraitImplDecl_Sync)))))
     ]

    (traced '()
            (decl:test-crate-decl-ok (core-crate-decl CrateDecl) TheCrate)
            ))
   )
  )