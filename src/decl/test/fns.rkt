#lang racket
(require redex/reduction-semantics
         "../decl-to-clause.rkt"
         "../decl-ok.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../../util.rkt")

;; Test the special rules for impls of the Copy trait.

(module+ test
  (redex-let*
   formality-decl

   ;; Test that we can write a function

   ((; fn foo<'a, T>(&'a T) -> &'a T { ... }
     FnDecl_foo (term (fn foo
                          ((lifetime A) (type T))
                          ((rigid-ty (ref ()) (A (rigid-ty T ()))))
                          -> (rigid-ty (ref ()) (A (rigid-ty T ())))
                          where ()
                          dummy-body)))

    (; the crate has a function
     CrateDecl (term (TheCrate (crate (FnDecl_foo)))))

    )

   (traced '()
           (test-equal
            (term (decl:is-crate-ok (CrateDecl) TheCrate))
            #t))
   )
  )