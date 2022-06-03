#lang racket
(require redex/reduction-semantics
         "../decl-to-clause.rkt"
         "../decl-ok.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../../ty/user-ty.rkt"
         "../../util.rkt")

;; Test the special rules for impls of the Drop trait.

(module+ test
  (redex-let*
   formality-decl

   ;; Test for that `impl Drop for i32` is not permitted.

   [(; trait rust:Drop { }
     TraitDecl_Drop (term (trait rust:Drop ((type Self)) where () ())))

    (; impl rust:Drop for i32 { }
     TraitImplDecl (term (impl () (rust:Drop ((user-ty i32))) where () ())))

    (; the crate has the struct, the trait, and the impl
     CrateDecl (term (TheCrate (crate (TraitDecl_Drop
                                       TraitImplDecl
                                       )))))

    (; create the Env for checking things in this crate
     Env (term (env-for-crate-decl CrateDecl)))
    ]

   (traced '()
           (decl:test-cannot-prove
            Env
            (crate-ok-goal (CrateDecl) CrateDecl)
            ))
   )

  (redex-let*
   formality-decl

   ;; Test for case where the Drop impl is for `Foo<i32>` only.

   [(; struct Foo<T> { }
     AdtDecl_Foo (term (struct Foo ((type T)) where ()
                         ((Foo ())) ; the 1 variant (named `Foo`)
                         )))

    (; trait rust:Drop { }
     TraitDecl_Drop (term (trait rust:Drop ((type Self)) where () ())))

    (; impl rust:Drop for Foo<i32> { } //~ ERROR
     TraitImplDecl (term (impl () (rust:Drop ((rigid-ty Foo ((user-ty i32))))) where () ())))

    (; the crate has the struct, the trait, and the impl
     CrateDecl (term (TheCrate (crate (AdtDecl_Foo
                                       TraitDecl_Drop
                                       TraitImplDecl
                                       )))))

    (; create the Env for checking things in this crate
     Env (term (env-for-crate-decl CrateDecl)))
    ]

   (traced '()
           (decl:test-cannot-prove
            Env
            (crate-ok-goal (CrateDecl) CrateDecl)
            ))
   )

  (redex-let*
   formality-decl

   ;; Test for case where the Drop impl adds an extra where clause
   ;; that doesn't follow from the struct.

   [(; struct Foo<T> { }
     AdtDecl_Foo (term (struct Foo ((type T)) where ()
                         ((Foo ())) ; the 1 variant (named `Foo`)
                         )))

    (; trait Debug { }
     TraitDecl_Debug (term (trait Debug ((type Self)) where () ())))

    (; trait rust:Drop { }
     TraitDecl_Drop (term (trait rust:Drop ((type Self)) where () ())))

    (; impl<U> rust:Drop for Foo<U> where U: Debug { } //~ ERROR
     TraitImplDecl (term (impl ((type U)) (rust:Drop ((rigid-ty Foo (U)))) where ((U : Debug())) ())))

    (; the crate has the struct, the trait, and the impl
     CrateDecl (term (TheCrate (crate (AdtDecl_Foo
                                       TraitDecl_Debug
                                       TraitDecl_Drop
                                       TraitImplDecl
                                       )))))
    ]

   (traced '() (decl:test-crate-decl-not-ok (CrateDecl) TheCrate))
   )

  (redex-let*
   formality-decl

   ;; Test for the case where the `Drop` impl has more where clauses than
   ;; are syntactically present on the struct, but they are entailed by
   ;; the predicates on the struct.

   [(; struct Foo<T> { }
     AdtDecl_Foo (term (struct Foo ((type T)) where ()
                         ((Foo ())) ; the 1 variant (named `Foo`)
                         )))

    (; trait rust:Drop { }
     TraitDecl_Drop (term (trait rust:Drop ((type Self)) where () {})))

    (; impl<U> rust:Drop for Foo<U> { }
     TraitImplDecl (term (impl ((type U)) (rust:Drop ((rigid-ty Foo (U)))) where () {})))

    (; the crate has the struct, the trait, and the impl
     CrateDecl (term (TheCrate (crate (AdtDecl_Foo
                                       TraitDecl_Drop
                                       TraitImplDecl
                                       )))))

    (; create the Env for checking things in this crate
     Env (term (env-for-crate-decl CrateDecl)))
    ]

   (traced '()
           (decl:test-can-prove
            Env
            (crate-ok-goal (CrateDecl) CrateDecl)
            ))
   )

  (redex-let*
   formality-decl

   ;; Test for the case where the `Drop` impl has more where clauses than
   ;; are syntactically present on the struct, but they are entailed by
   ;; the predicates on the struct.

   [(; struct Foo<T> where T: Ord { }
     AdtDecl_Foo (term (struct Foo ((type T)) where ((T : Ord()))
                         ((Foo ())) ; the 1 variant (named `Foo`)
                         )))

    (; trait rust:Drop { }
     TraitDecl_Drop (term (trait rust:Drop ((type Self)) where () {})))

    (; trait Eq { }
     TraitDecl_Eq (term (trait Eq ((type Self)) where () {})))

    (; trait Ord: Eq { }
     TraitDecl_Ord (term (trait Ord ((type Self)) where ((Self : Eq())) {})))

    (; impl<U> rust:Drop for Foo<U> where T: Ord + Eq { }
     TraitImplDecl (term (impl ((type U))
                               (rust:Drop ((rigid-ty Foo (U))))
                               where ((U : Ord())
                                      (U : Eq())
                                      )
                               {})))

    (; the crate has the struct, the trait, and the impl
     CrateDecl (term (TheCrate (crate (AdtDecl_Foo
                                       TraitDecl_Drop
                                       TraitDecl_Eq
                                       TraitDecl_Ord
                                       TraitImplDecl
                                       )))))
    ]

   (traced '()
           (decl:test-crate-decl-ok (CrateDecl) TheCrate))
   )

  )
