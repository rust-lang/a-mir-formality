#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         )

;; Test the special rules for impls of the Copy trait.

(module+ test
  ;; Test we can impl copy for i32
  (traced '()
          (test-equal
           (term (rust:is-core-crate-ok { (trait core:Copy[] where [] {})
                                          (impl[] core:Copy[] for i32 where [] {}) }))
           #t))

  ;; Test for that `struct Foo { } struct Bar { f: Foo } impl Copy for Bar` is not permitted
  ;; because `Foo: Copy` does not hold.
  (redex-let*
   formality-rust
   [([Rust/CrateItemDecl ...] (term [(trait core:Copy[] where [] {})
                                     (struct Foo[] where [] {})
                                     (struct Bar[] where [] { (f : (Foo < >)) })
                                     (impl[] core:Copy[] for (Bar < >) where [] {})]))]
   (traced '()
           (test-equal
            (term (rust:is-core-crate-ok { Rust/CrateItemDecl ... }))
            #f))
   (traced '()
           (test-equal
            (term (rust:is-core-crate-ok { Rust/CrateItemDecl ...
                                           (impl[] core:Copy[] for (Foo < >) where [] {}) }))
            #t))
   )
  )
