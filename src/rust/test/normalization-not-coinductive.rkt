#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         )

(module+ test

  (traced '()
          (test-equal
           (term (rust:can-prove-where-clause-in-program
                  ([(crate C { (trait Foo[] where [] { (type FooItem[] : [] where []) })
                               (trait Bar[] where [] { (type BarItem[] : [] where []) })
                               (impl[] Foo[] for () where [] { (type FooItem[] = (< () as Bar[] > :: BarItem[]) where []) })
                               (impl[] Bar[] for () where [] { (type BarItem[] = (< () as Foo[] > :: FooItem[]) where []) })
                               })]
                   C)
                  (∀ ((type T))
                     where []
                     (< () as Foo[] > :: FooItem[] == T))))
           #f
           )
          )
  )