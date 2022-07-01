#lang racket
(require redex/reduction-semantics
         "../decl-to-clause.rkt"
         "../decl-ok.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../../ty/user-ty.rkt"
         "../../util.rkt")

;; Test the special rules for impls of the Copy trait.

(module+ test
  (redex-let*
   formality-decl

   ;; Test for that `impl Copy for i32` is permitted.

   [(; trait rust:Copy { _: Foo }
     TraitDecl_Copy (term (trait rust:Copy ((type Self)) where () ())))

    (; impl rust:Copy for i32 { }
     TraitImplDecl (term (impl () rust:Copy () for i32 where () ())))

    (; the crate has the struct, the trait, and the impl
     CrateDecl (term (TheCrate (crate (TraitDecl_Copy
                                       TraitImplDecl
                                       )))))

    ]

   (traced '()
           (test-equal
            (term (decl:is-crate-ok [CrateDecl] TheCrate))
            #t))
   )

  (redex-let*
   formality-decl

   ;; Test for that `struct Foo { } struct Bar { f: Foo } impl Copy for Bar` is not permitted
   ;; because `Foo: Copy` does not hold.

   [(; trait rust:Copy { }
     TraitDecl_Copy (term (trait rust:Copy ((type Self)) where () ())))

    (; struct Foo { }
     AdtDecl_Foo (term (struct Foo
                         () ; no generic parameters
                         where () ; no where clauses
                         ((Foo ())) ; the 1 variant (named `Foo`)
                         )))

    (; struct Bar { _: Foo }
     AdtDecl_Bar (term (struct Bar
                         () ; no generic parameters
                         where () ; no where clauses
                         ((Bar ((f (rigid-ty Foo ()))
                                ))) ; the 1 variant (named `Foo`)
                         )))

    (; impl rust:Copy for Bar { }
     TraitImplDecl (term (impl () rust:Copy () for (Bar) where () ())))

    (; the crate has the struct, the trait, and the impl
     CrateDecl (term (TheCrate (crate (TraitDecl_Copy
                                       AdtDecl_Foo
                                       AdtDecl_Bar
                                       TraitImplDecl
                                       )))))
    ]

   (traced '()
           (test-equal
            (term (decl:is-crate-ok [CrateDecl] TheCrate))
            #f
            ))

   (redex-let*
    formality-decl
    [(; impl rust:Copy for Foo { }
      TraitImplDecl_Foo (term (impl () rust:Copy () for (Foo) where () ())))

     (; the crate has the struct, the trait, and the impl
      CrateDecl_Pass (term (TheCrate (crate [TraitDecl_Copy
                                             AdtDecl_Foo
                                             AdtDecl_Bar
                                             TraitImplDecl
                                             TraitImplDecl_Foo
                                             ]))))
     ]
    (traced '()
            (test-equal
             (term (decl:is-crate-ok [CrateDecl_Pass] TheCrate))
             #t))

    )

   )
  )
