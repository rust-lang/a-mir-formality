#lang racket
(require redex/reduction-semantics
         "../../ty/user-ty.rkt"
         "../../util.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         )

;; Test the special rules for impls of the Drop trait.

(module+ test
  ;; Test for that `impl Drop for i32` is not permitted.
  (traced '()
          (test-equal
           (term (rust:is-core-crate-ok { (trait core:Drop[] where [] {})
                                          (impl[] core:Drop[] for i32 where [] {})
                                          }))
           #f
           ))

  ;; Test for case where the Drop impl is for `Foo<i32>` only.
  (traced '()
          (test-equal
           (term (rust:is-core-crate-ok { (trait core:Drop[] where [] {})
                                          (struct Foo[(type T)] where [] {})
                                          (impl[] core:Drop[] for (Foo < i32 >) where [] {})
                                          }))
           #f
           ))

  ;; Test for case where the Drop impl adds an extra where clause
  ;; that doesn't follow from the struct.
  (traced '()
          (test-equal
           (term (rust:is-core-crate-ok { (trait core:Drop[] where [] {})
                                          (trait Debug[] where [] {})
                                          (struct Foo[(type T)] where [] {})
                                          (impl[(type U)] core:Drop[] for (Foo < U >) where [(U : Debug[])] {})
                                          }))
           #f
           ))

  ;; Variant of the previous test where one where clause applies and not the other.
  (traced '()
          (test-equal
           (term (rust:is-core-crate-ok { (trait core:Drop[] where [] {})
                                          (trait Eq[] where [] {})
                                          (trait Ord[] where [(Self : Eq())] {})
                                          (struct Foo[(type T)] where [(T : Eq[])] {})
                                          (impl[(type U)] core:Drop[] for (Foo < U >)
                                               where [(U : Ord[]) (U : Eq[])]
                                               {})
                                          }))
           #f
           ))

  ;; Test for the case where the `Drop` impl has more where clauses than
  ;; are syntactically present on the struct, but they are entailed by
  ;; the predicates on the struct.
  (traced '()
          (test-equal
           (term (rust:is-core-crate-ok { (trait core:Drop[] where [] {})
                                          (trait Eq[] where [] {})
                                          (trait Ord[] where [(Self : Eq())] {})
                                          (struct Foo[(type T)] where [(T : Ord[])] {})
                                          (impl[(type U)] core:Drop[] for (Foo < U >)
                                               where [(U : Ord[]) (U : Eq[])]
                                               {})
                                          }))
           #t
           ))
  )
