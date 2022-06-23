#lang racket
(require redex/reduction-semantics
         "../env.rkt"
         "../decl-ok.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../../ty/user-ty.rkt"
         "../../util.rkt")

(module+ test
  (redex-let*
   formality-decl

   ((;; # Crate A
     ;;
     ;; trait Debug { }
     ;; impl Debug for i32 { }
     ;;
     CrateDecl_A (redex-let*
                  formality-decl
                  ((TraitDecl (term (trait Debug ((type Self)) where () {})))
                   (TraitImplDecl (term (impl () (Debug ((user-ty i32))) where () {})))
                   )
                  (term (CrateA (crate (TraitDecl TraitImplDecl))))))

    (;; # Crate B
     ;;
     ;; trait WithDebug<T: Debug> { }
     ;;
     ;; struct Foo<T: Debug> { }
     ;; impl<T> WithDebug<T> for Foo<T> { }
     ;;
     ;; # Crate Be
     ;;
     ;; includes (feature expanded-implied-bounds)
     (CrateDecl_B CrateDecl_Be) (redex-let*
                                 formality-decl
                                 ((TraitDecl_WithDebug (term (trait WithDebug ((type Self) (type T)) where ((T : Debug())) {})))
                                  (AdtDecl_Foo (term (struct Foo ((type T)) where ((T : Debug())) ((Foo ())))))
                                  (TraitImplDecl (term (impl ((type T)) (WithDebug ((rigid-ty Foo (T)) T)) where () ())))
                                  ((CrateItemDecl_B ...) (term (TraitDecl_WithDebug AdtDecl_Foo TraitImplDecl)))
                                  )
                                 (term ((CrateB (crate (CrateItemDecl_B ...)))
                                        (CrateBe (crate (CrateItemDecl_B ... (feature expanded-implied-bounds))))))))

    (;; # Crate C
     ;;
     ;; No items.
     CrateDecl_C (redex-let*
                  formality-decl
                  ()
                  (term (CrateC (crate ())))))

    (CrateDecls_AB (term (CrateDecl_A CrateDecl_B)))
    (CrateDecls_ABe (term (CrateDecl_A CrateDecl_Be)))
    (CrateDecls_ABC (term (CrateDecl_A CrateDecl_B CrateDecl_C)))
    (CrateDecls_ABeC (term (CrateDecl_A CrateDecl_Be CrateDecl_C)))
    )


   (;; Crate B cannot prove itself WF
    traced '()
           (decl:test-crate-decl-not-ok CrateDecls_AB CrateB))

   (;; Crate Be CAN prove itself WF
    traced '()
           (decl:test-crate-decl-ok CrateDecls_ABe CrateBe))

   (redex-let*
    formality-decl
    ;; Crate Be can prove `∀<T> { If (well-formed(Foo<T>)) { is-implemented(T: Debug) } }`
    ((Env (term (env-for-crate-decls CrateDecls_ABe CrateBe)))
     (Goal_ImpliedBound (term (∀ ((type T))
                                 (implies ((well-formed (type (rigid-ty Foo (T)))))
                                          (is-implemented (Debug (T))))))))
    (traced '()
            (decl:test-can-prove Env Goal_ImpliedBound)))

   (redex-let*
    formality-decl
    ;; Crate C can also prove `∀<T> { If (well-formed(Foo<T>) { is-implemented(T: Debug) } }`
    ((Env (term (env-for-crate-decls CrateDecls_ABeC CrateC)))
     (Goal_ImpliedBound (term (∀ ((type T))
                                 (implies ((well-formed (type (rigid-ty Foo (T))))
                                           (well-formed (type T)))
                                          (is-implemented (Debug (T))))))))

    (traced '()
            (decl:test-can-prove Env Goal_ImpliedBound)))

   (redex-let*
    formality-decl
    ;; and it can prove `∀<T> { If (well-formed(Foo<T>, T)) { is-implemented(Foo<T>: WithDebug<T>) } }`
    ((Env (term (env-for-crate-decls CrateDecls_ABeC CrateC)))
     (Goal_UseImpl (term (∀ ((type T))
                            (implies ((well-formed (type (rigid-ty Foo (T))))
                                      (well-formed (type T)))
                                     (is-implemented (WithDebug ((rigid-ty Foo (T)) T))))))))

    (traced '()
            (decl:test-can-prove Env Goal_UseImpl)))

   (redex-let*
    formality-decl
    ;; but it CAN prove `∀<T> { If (well-formed(Foo<T>, T), is-implemented(T: Debug)) { is-implemented(Foo<T>: WithDebug<T>) } }`
    ((Env (term (env-for-crate-decls CrateDecls_ABeC CrateC)))
     (Goal_UseImplDebug (term (∀ ((type T))
                                 (implies ((well-formed (type (rigid-ty Foo (T))))
                                           (is-implemented (Debug (T))))
                                          (is-implemented (WithDebug ((rigid-ty Foo (T)) T))))))))

    (traced '()
            (decl:test-can-prove Env Goal_UseImplDebug)))
   )
  )