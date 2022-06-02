#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../prove.rkt"
         "../../ty/user-ty.rkt"
         "../../util.rkt")

(module+ test
  (current-traced-metafunctions '(relate/one compare/one/substituted equate/one/substituted))
  (redex-let*
   formality-decl

   ((; trait Debug { }
     TraitDecl (term (Debug (trait ((type Self)) () ()))))
    (; struct Foo<T: Debug> { }
     AdtDecl_Foo (term (Foo (struct ((type T)) ((T : Debug())) ((Foo ())))))))

   (; test that WF checks fail if `T: Debug` is missing
    redex-let*
    formality-decl
    [
     (; const BROKEN<T>: Foo<T>;
      ConstDecl_broken (term (BROKEN (const ((type T)) () (rigid-ty Foo (T)) dummy-body))))
     (CrateDecl (term (TheCrate (crate (TraitDecl AdtDecl_Foo ConstDecl_broken)))))
     ]

    (traced '()
            (decl:test-crate-decl-not-ok (CrateDecl) TheCrate)
            ))

   (; test that WF checks succeed if `T: Debug` is present
    redex-let*
    formality-decl
    [
     (; const OK<T: Debug>: Foo<T>;
      ConstDecl_ok (term (OK (const ((type T)) ((T : Debug())) (rigid-ty Foo (T)) dummy-body))))
     (CrateDecl (term (TheCrate (crate (TraitDecl AdtDecl_Foo ConstDecl_ok)))))
     ]

    (traced '()
            (decl:test-crate-decl-ok (CrateDecl) TheCrate)
            ))

   )

  )
