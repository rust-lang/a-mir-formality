#lang racket
(require redex/reduction-semantics
         "../../env.rkt"
         "../../decl-ok.rkt"
         "../../grammar.rkt"
         "../../prove.rkt"
         "../../../util.rkt")

(module+ test
  (redex-let*
   formality-decl

   [(; struct Foo { }
     AdtDecl_Foo (term (struct Foo () where () ((struct-variant ())))))

    (; trait Copy { }
     TraitDecl_Copy (term (trait Copy ((type Self)) where () ())))

    (; trait Partial: Copy { }
     TraitDecl_Partial (term (trait Partial((type Self)) where ((Self : Copy())) ())))

    (; trait Complete: Partial { }
     TraitDecl_Complete (term (trait Complete ((type Self)) where ((Self : Partial())) ())))

    (; impl<T> Partial for T where T: Complete {}
     TraitImplDecl_Partial (term (impl ((type T)) (Partial (T)) where ((T : Complete())) ())))

    (; impl<T> Complete for T {}
     TraitImplDecl_CompleteA (term (impl ((type T)) (Complete (T)) where () ())))

    (; impl<T: Partial> Complete for T {}
     TraitImplDecl_CompleteB (term (impl ((type T)) (Complete (T)) where ((T : Partial())) ())))

    (; crate A { ... }
     CrateDecl_A (term (A (crate (AdtDecl_Foo
                                  TraitDecl_Copy
                                  TraitDecl_Partial
                                  TraitDecl_Complete
                                  TraitImplDecl_Partial
                                  TraitImplDecl_CompleteA)))))

    (Env_A (term (env-for-crate-decl CrateDecl_A)))

    (; crate B { ... }
     CrateDecl_B (term (B (crate (AdtDecl_Foo
                                  TraitDecl_Copy
                                  TraitDecl_Partial
                                  TraitDecl_Complete
                                  TraitImplDecl_Partial
                                  TraitImplDecl_CompleteB)))))

    (Env_B (term (env-for-crate-decl CrateDecl_B)))
    ]

   (; The crate A is not well-formed:
    ;
    ; the `impl<T> Complete for T` cannot prove that `T: Complete` because it cannot
    ; prove that `T: Copy`.
    traced '()
           (decl:test-cannot-prove
            Env_A
            (crate-ok-goal (CrateDecl_A) CrateDecl_A)))

   (; The crate B, however, IS well-formed.
    traced '()
           (decl:test-can-prove
            Env_B
            (crate-ok-goal (CrateDecl_B) CrateDecl_B)))

   (redex-let*
    formality-decl
    [(Ty_Foo (term (rigid-ty Foo ())))]

    (; But `Foo: Partial` does not hold in B.
     traced '()
            (decl:test-cannot-prove
             Env_B
             (is-implemented (Partial (Ty_Foo)))))

    (; But `Foo: Partial` implies `Foo: Copy`.
     traced '()
            (decl:test-can-prove
             Env_B
             (implies ((is-implemented (Partial (Ty_Foo)))) (is-implemented (Copy (Ty_Foo))))))
    )
   )
  )