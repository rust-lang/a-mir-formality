#lang racket
(require redex/reduction-semantics
         "../decl-to-clause.rkt"
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
     CrateDecl_B (redex-let*
                  formality-decl
                  ((TraitDecl_WithDebug (term (trait WithDebug ((type Self) (type T)) where ((T : Debug())) {})))
                   (AdtDecl_Foo (term (struct Foo ((type T)) where ((T : Debug())) ((Foo ())))))
                   (TraitImplDecl (term (impl ((type T)) (WithDebug ((rigid-ty Foo (T)) T)) where () ())))
                   )
                  (term (CrateB (crate (TraitDecl_WithDebug AdtDecl_Foo TraitImplDecl))))))

    (;; # Crate C
     ;;
     ;; No items.
     CrateDecl_C (redex-let*
                  formality-decl
                  ()
                  (term (CrateC (crate ())))))

    (CrateDecls_AB (term (CrateDecl_A CrateDecl_B)))
    (CrateDecls_ABC (term (CrateDecl_A CrateDecl_B CrateDecl_C)))

    (Env_B (term (env-for-crate-decls CrateDecls_AB CrateB)))
    (Env_C (term (env-for-crate-decls CrateDecls_ABC CrateC)))
    )

   (redex-let*
    formality-decl
    ;; Crate B can prove itself WF
    ((Goal_B_Ok (term (crate-ok-goal CrateDecls_AB CrateDecl_B))))
    (traced '()
            (decl:test-can-prove Env_B Goal_B_Ok))
    )

   (redex-let*
    formality-decl
    ;; Crate B can prove `∀<T> { If (well-formed(Foo<T>)) { is-implemented(T: Debug) } }`
    ((Goal_B_ImpliedBound (term (∀ ((type T))
                                   (implies ((well-formed (type (rigid-ty Foo (T)))))
                                            (is-implemented (Debug (T))))))))
    (traced '()
            (decl:test-can-prove Env_B Goal_B_ImpliedBound)))

   (redex-let*
    formality-decl
    ;; Crate C can also prove `∀<T> { If (well-formed(Foo<T>) { is-implemented(T: Debug) } }`
    ((Goal_C_ImpliedBound (term (∀ ((type T))
                                   (implies ((well-formed (type (rigid-ty Foo (T))))
                                             (well-formed (type T)))
                                            (is-implemented (Debug (T))))))))

    (traced '()
            (decl:test-can-prove Env_C Goal_C_ImpliedBound)))

   (redex-let*
    formality-decl
    ;; and it can prove `∀<T> { If (well-formed(Foo<T>, T)) { is-implemented(Foo<T>: WithDebug<T>) } }`
    ((Goal_C_UseImpl (term (∀ ((type T))
                              (implies ((well-formed (type (rigid-ty Foo (T))))
                                        (well-formed (type T)))
                                       (is-implemented (WithDebug ((rigid-ty Foo (T)) T))))))))

    (traced '()
            (decl:test-can-prove Env_C Goal_C_UseImpl)))

   (redex-let*
    formality-decl
    ;; but it CAN prove `∀<T> { If (well-formed(Foo<T>, T), is-implemented(T: Debug)) { is-implemented(Foo<T>: WithDebug<T>) } }`
    ((Goal_C_UseImplDebug (term (∀ ((type T))
                                   (implies ((well-formed (type (rigid-ty Foo (T))))
                                             (is-implemented (Debug (T))))
                                            (is-implemented (WithDebug ((rigid-ty Foo (T)) T))))))))

    (traced '()
            (decl:test-can-prove Env_C Goal_C_UseImplDebug)))
   )
  )