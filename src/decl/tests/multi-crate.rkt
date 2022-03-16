#lang racket
(require redex/reduction-semantics
         "../decl-to-clause.rkt"
         "../decl-ok.rkt"
         "../grammar.rkt"
         "../../ty/grammar.rkt"
         "../../ty/cosld-solve.rkt"
         "../../util.rkt")

(module+ test

  ;;
  ;; # Crate C
  ;;
  ;; can prove `ForAll<T> { If (WellFormed(Foo<T>) { Implemented(Foo<T>: WithDebug<T>) } }`
  ;;
  ;; cannot prove `ForAll<T> { If (WellFormed(Foo<T>) { Implemented(T: Debug) } }`

  (redex-let*
   formality-decl

   ((;; # Crate A
     ;;
     ;; trait Debug { }
     ;; impl Debug for i32 { }
     ;;
     CrateDecl_A (redex-let*
                  formality-decl
                  ((TraitDecl (term (Debug (trait ((TyKind Self)) () ()))))
                   (TraitImplDecl (term (impl () (Debug ((scalar-ty i32))) () ())))
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
                  ((TraitDecl_WithDebug (term (WithDebug (trait ((TyKind Self) (TyKind T)) ((Implemented (Debug (T)))) ()))))
                   (AdtDecl_Foo (term (Foo (struct ((TyKind T)) ((Implemented (Debug (T)))) ((Foo ()))))))
                   (TraitImplDecl (term (impl ((TyKind T)) (WithDebug ((TyApply Foo (T)) T)) () ())))
                   )
                  (term (CrateB (crate (TraitDecl_WithDebug AdtDecl_Foo TraitImplDecl))))))

    (CrateDecls (term (CrateDecl_A CrateDecl_B)))

    ;; Crate B can prove itself WF
    (Goal_B_Ok (term (crate-ok-goal CrateDecls CrateDecl_B)))
    (Env_B (term (env-with-crate-decls EmptyEnv CrateDecls CrateB)))

    ;; Crate B can prove `ForAll<T> { If (WellFormed(Foo<T>) { Implemented(T: Debug) } }`
    )

   (traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env_B
                             Goal_B_Ok
                             EnvSubstitution)
                            EnvSubstitution)
            (term ((Env_B ())))))
   )
  )

