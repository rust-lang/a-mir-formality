#lang racket
(require redex/reduction-semantics
         "../env.rkt"
         "../decl-ok.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../../ty/user-ty.rkt"
         "../../util.rkt")

;; Various tests that check the requirements that where clauses be well-formed.

(module+ test

  (redex-let*
   formality-decl

   ((; struct Foo<'a, T> where T: 'a { }
     AdtDecl_Foo (term (struct Foo ((lifetime a) (type T)) where ((T : a)) { (struct-variant ()) })))
    (CrateDecl_C (term (C (crate (AdtDecl_Foo)))))
    (Env (term (env-for-crate-decl CrateDecl_C)))
    )

   (traced '()
           (decl:test-can-prove Env (∀ ((lifetime x) (type A))
                                       (implies
                                        ((well-formed (type (user-ty (Foo x A)))))
                                        (A -outlives- x)))))
   )
  )