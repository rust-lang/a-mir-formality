#lang racket
(require redex/reduction-semantics
         "../env.rkt"
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

   ((TraitDecl_PartialEq (term (trait PartialEq ((type Self)) where () {})))
    (TraitDecl_Eq (term (trait Eq ((type Self))
                               where ((Self : PartialEq()))
                               {})))
    (TraitDecl_Debug (term (trait Debug ((type Self)) where () {})))
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