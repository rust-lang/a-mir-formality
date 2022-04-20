#lang racket
(require redex/reduction-semantics
         "../decl-to-clause.rkt"
         "../decl-ok.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../../util.rkt")

;; Test the special rules for impls of the Drop trait.

(module+ test
  (redex-let*
   formality-decl

   ;; Test for that `impl Drop for i32` is not permitted.

   ((; trait rust:Drop { }
     TraitDecl_Drop (term (rust:Drop (trait ((TyKind Self)) () ()))))

    (; impl rust:Drop for i32 { }
     TraitImplDecl (term (impl () (rust:Drop ((scalar-ty i32))) () ())))

    (; the crate has the struct, the trait, and the impl
     CrateDecl (term (TheCrate (crate (TraitDecl_Drop
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
   )

  (redex-let*
   formality-decl

   ;; Test for case where the Drop impl is for `Foo<i32>` only.

   ((; struct Foo<T> { }
     AdtDecl_Foo (term (Foo (struct ((TyKind T)) ()
                              ((Foo ())) ; the 1 variant (named `Foo`)
                              ))))

    (; trait rust:Drop { }
     TraitDecl_Drop (term (rust:Drop (trait ((TyKind Self)) () ()))))

    (; impl rust:Drop for Foo<i32> { } //~ ERROR
     TraitImplDecl (term (impl () (rust:Drop ((TyRigid Foo ((scalar-ty i32))))) () ())))

    (; the crate has the struct, the trait, and the impl
     CrateDecl (term (TheCrate (crate (AdtDecl_Foo
                                       TraitDecl_Drop
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
   )

  (redex-let*
   formality-decl

   ;; Test for case where the Drop impl adds an extra where clause
   ;; that doesn't follow from the struct.

   ((; struct Foo<T> { }
     AdtDecl_Foo (term (Foo (struct ((TyKind T)) ()
                              ((Foo ())) ; the 1 variant (named `Foo`)
                              ))))

    (; trait rust:Drop { }
     TraitDecl_Drop (term (rust:Drop (trait ((TyKind Self)) () ()))))

    (; impl<U> rust:Drop for Foo<U> where U: Debug { } //~ ERROR
     TraitImplDecl (term (impl ((TyKind U)) (rust:Drop ((TyRigid Foo (U)))) ((Implemented (Debug (U)))) ())))

    (; the crate has the struct, the trait, and the impl
     CrateDecl (term (TheCrate (crate (AdtDecl_Foo
                                       TraitDecl_Drop
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
   )

  (redex-let*
   formality-decl

   ;; Test for the case where the `Drop` impl has more where clauses than
   ;; are syntactically present on the struct, but they are entailed by
   ;; the predicates on the struct.

   ((; struct Foo<T> { }
     AdtDecl_Foo (term (Foo (struct ((TyKind T)) ()
                              ((Foo ())) ; the 1 variant (named `Foo`)
                              ))))

    (; trait rust:Drop { }
     TraitDecl_Drop (term (rust:Drop (trait ((TyKind Self)) () ()))))

    (; impl<U> rust:Drop for Foo<U> { }
     TraitImplDecl (term (impl ((TyKind U)) (rust:Drop ((TyRigid Foo (U)))) () ())))

    (; the crate has the struct, the trait, and the impl
     CrateDecl (term (TheCrate (crate (AdtDecl_Foo
                                       TraitDecl_Drop
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

   ;; Test for the case where the `Drop` impl has more where clauses than
   ;; are syntactically present on the struct, but they are entailed by
   ;; the predicates on the struct.

   ((; struct Foo<T> where T: Ord { }
     AdtDecl_Foo (term (Foo (struct ((TyKind T)) ((Implemented (Ord (T))))
                              ((Foo ())) ; the 1 variant (named `Foo`)
                              ))))

    (; trait rust:Drop { }
     TraitDecl_Drop (term (rust:Drop (trait ((TyKind Self)) () ()))))

    (; trait Eq { }
     TraitDecl_Eq (term (Eq (trait ((TyKind Self)) () ()))))

    (; trait Ord: Eq { }
     TraitDecl_Ord (term (Ord (trait ((TyKind Self)) ((Implemented (Eq (Self)))) ()))))

    (; impl<U> rust:Drop for Foo<U> where T: Ord + Eq { }
     TraitImplDecl (term (impl ((TyKind U))
                               (rust:Drop ((TyRigid Foo (U))))
                               ((Implemented (Ord (U)))
                                (Implemented (Eq (U)))
                                )
                               ())))

    (; the crate has the struct, the trait, and the impl
     CrateDecl (term (TheCrate (crate (AdtDecl_Foo
                                       TraitDecl_Drop
                                       TraitDecl_Eq
                                       TraitDecl_Ord
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

  )
