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

   [(; trait core:Copy { _: Foo }
     TraitDecl_Copy (term (trait core:Copy ((type Self)) where () ())))

    (; impl core:Copy for i32 { }
     TraitImplDecl (term (impl () (core:Copy ((user-ty i32))) where () ())))

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

   [(; trait core:Copy { }
     TraitDecl_Copy (term (trait core:Copy ((type Self)) where () ())))

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

    (; impl core:Copy for Bar { }
     TraitImplDecl (term (impl () (core:Copy ((rigid-ty Bar ()))) where () ())))

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
    [(; impl core:Copy for Foo { }
      TraitImplDecl_Foo (term (impl () (core:Copy ((rigid-ty Foo ()))) where () ())))

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
