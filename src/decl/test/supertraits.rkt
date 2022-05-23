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

   ((TraitDecl_PartialEq (term (PartialEq (trait ((type Self)) () ()))))
    (TraitDecl_Eq (term (Eq (trait ((type Self)) ((Self : PartialEq())) ()))))
    (TraitDecl_Debug (term (Debug (trait ((type Self)) () ()))))
    (CrateDecl (term (TheCrate (crate (TraitDecl_PartialEq TraitDecl_Eq)))))
    (Env (term (env-for-crate-decl CrateDecl)))
    )

   (traced '()
           (decl:test-cannot-prove
            Env
            (∀ ((type T))
               (implies ((is-implemented (PartialEq (T))))
                        (is-implemented (Eq (T)))))))

   (traced '()
           (decl:test-can-prove
            Env
            (∀ ((type T))
               (implies ((is-implemented (Eq (T))))
                        (is-implemented (PartialEq (T)))))))

   (traced '()
           (decl:test-can-prove
            Env
            (∀ ((type T))
               (implies ((is-implemented (Eq (T))))
                        (is-implemented (Eq (T)))))))

   (traced '()
           (decl:test-cannot-prove
            Env
            (∀ ((type T))
               (implies ((is-implemented (Eq (T))))
                        (is-implemented (Debug (T)))))))
   )
  )