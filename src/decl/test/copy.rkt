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

   ((; trait rust:Copy { _: Foo }
     TraitDecl_Copy (term (rust:Copy (trait ((type Self)) () ()))))

    (; impl rust:Copy for i32 { }
     TraitImplDecl (term (impl () (rust:Copy ((user-ty i32))) () ())))

    (; the crate has the struct, the trait, and the impl
     CrateDecl (term (TheCrate (crate (TraitDecl_Copy
                                       TraitImplDecl
                                       )))))

    (; create the Env for checking things in this crate
     Env (term (env-for-crate-decl CrateDecl)))
    )

   (traced '()
           (decl:test-can-prove
            Env
            (crate-ok-goal (CrateDecl) CrateDecl)
            ))
   )

  (redex-let*
   formality-decl

   ;; Test for that `struct Foo { } struct Bar { f: Foo } impl Copy for Bar` is not permitted
   ;; because `Foo: Copy` does not hold.

   ((; trait rust:Copy { }
     TraitDecl_Copy (term (rust:Copy (trait ((type Self)) () ()))))

    (; struct Foo { }
     AdtDecl_Foo (term (Foo (struct
                              () ; no generic parameters
                              () ; no where clauses
                              ((Foo ())) ; the 1 variant (named `Foo`)
                              ))))

    (; struct Bar { _: Foo }
     AdtDecl_Bar (term (Bar (struct
                              () ; no generic parameters
                              () ; no where clauses
                              ((Bar ((f (rigid-ty Foo ()))
                                     ))) ; the 1 variant (named `Foo`)
                              ))))

    (; impl rust:Copy for Bar { }
     TraitImplDecl (term (impl () (rust:Copy ((rigid-ty Bar ()))) () ())))

    (; the crate has the struct, the trait, and the impl
     CrateDecl (term (TheCrate (crate (TraitDecl_Copy
                                       AdtDecl_Foo
                                       AdtDecl_Bar
                                       TraitImplDecl
                                       )))))

    (; create the Env for checking things in this crate
     Env (term (env-for-crate-decl CrateDecl)))
    )

   (traced '()
           (decl:test-cannot-prove
            Env
            (crate-ok-goal (CrateDecl) CrateDecl)
            ))


   (redex-let*
    formality-decl
    [(; impl rust:Copy for Foo { }
      TraitImplDecl_Foo (term (impl () (rust:Copy ((rigid-ty Foo ()))) () ())))

     (; the crate has the struct, the trait, and the impl
      CrateDecl_Pass (term (TheCrate (crate (TraitDecl_Copy
                                             AdtDecl_Foo
                                             AdtDecl_Bar
                                             TraitImplDecl
                                             TraitImplDecl_Foo
                                             )))))

     (; create the Env for checking things in this crate
      Env (term (env-for-crate-decl CrateDecl_Pass)))

     ]
    (traced '()
            (decl:test-can-prove
             Env
             (crate-ok-goal (CrateDecl_Pass) CrateDecl_Pass)
             ))

    )

   )
  )
