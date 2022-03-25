#lang racket
(require redex/reduction-semantics
         "../decl-to-clause.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../../ty/grammar.rkt"
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
    (Env (term (env-for-crate-decl CrateDecl)))
    )

   (traced '()
           (test-equal
            (judgment-holds (decl:prove-top-level-goal/cosld
                             Env
                             (ForAll ((TyKind T))
                                     (Implies ((Implemented (PartialEq (T))))
                                              (Implemented (Eq (T)))))
                             Env_out)
                            Env_out)
            (term ())))

   (traced '()
           (test-equal
            (judgment-holds (decl:prove-top-level-goal/cosld
                             Env
                             (ForAll ((TyKind T))
                                     (Implies ((Implemented (Eq (T))))
                                              (Implemented (PartialEq (T)))))
                             Env_out)
                            Env_out)
            (term (Env))))

   (traced '()
           (test-equal
            (judgment-holds (decl:prove-top-level-goal/cosld
                             Env
                             (ForAll ((TyKind T))
                                     (Implies ((Implemented (Eq (T))))
                                              (Implemented (Eq (T)))))
                             Env_out)
                            Env_out)
            (term (Env))))

   (traced '()
           (test-equal
            (judgment-holds (decl:prove-top-level-goal/cosld
                             Env
                             (ForAll ((TyKind T))
                                     (Implies ((Implemented (Eq (T))))
                                              (Implemented (Debug (T)))))
                             Env_out)
                            Env_out)
            (term ())))
   )
  )