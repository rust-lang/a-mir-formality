#lang racket
(require redex/reduction-semantics
         "../decl-to-clause.rkt"
         "../grammar.rkt"
         "../../ty/grammar.rkt"
         "../../ty/solve.rkt"
         "../../util.rkt")

(module+ test
  ;; Program:
  ;;
  ;; trait Eq: PartialEq { }
  ;; impl Debug for i32 { }
  (redex-let*
   formality-decl

   ((TraitDecl_PartialEq (term (PartialEq (trait ((TyKind Self)) () ()))))
    (TraitDecl_Eq (term (Eq (trait ((TyKind Self)) ((Implemented (PartialEq (Self)))) ()))))
    (TraitDecl_Debug (term (Debug (trait ((TyKind Self)) () ()))))
    (CrateDecl (term (TheCrate (crate (TraitDecl_PartialEq TraitDecl_Eq)))))
    (Env (term (env-with-crate-decl EmptyEnv CrateDecl)))
    )

   (traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (ForAll ((TyKind T))
                                     (Implies ((Implemented (PartialEq (T))))
                                              (Implemented (Eq (T)))))
                             EnvSubstitution)
                            EnvSubstitution)
            (term ())))

   (traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (ForAll ((TyKind T))
                                     (Implies ((Implemented (Eq (T))))
                                              (Implemented (PartialEq (T)))))
                             EnvSubstitution)
                            EnvSubstitution)
            (term ((Env ())))))

   (traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (ForAll ((TyKind T))
                                     (Implies ((Implemented (Eq (T))))
                                              (Implemented (Eq (T)))))
                             EnvSubstitution)
                            EnvSubstitution)
            (term ((Env ())))))

   (traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (ForAll ((TyKind T))
                                     (Implies ((Implemented (Eq (T))))
                                              (Implemented (Debug (T)))))
                             EnvSubstitution)
                            EnvSubstitution)
            (term ())))
   )
  )