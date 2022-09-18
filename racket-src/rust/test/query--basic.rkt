#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../logic/alpha-eq-util.rkt"
         "../query.rkt"
         )

(module+ test

  (traced '()
          (test-equal
           (term (rust:query
                  ([(crate C {(struct Vec[(type T)] where [] { })
                              (trait Foo[(type A)] where [] {  })
                              (impl[(type T)] Foo[T] for (Vec < T >) where [] { })
                              })]
                   C)
                  (?∀ [(type X)]
                      (?∃ [(type T)]
                          ((Vec < X >) : Foo[T])))
                  ))
           (term [(:-
                   []           ; no new inference variables are introduced
                   ([(T X)]     ; T => X
                    []))])      ; no additional constraints
           )
          )

  (traced '()
          (test-equal
           (term (test-alpha-equivalent-sets
                  (rust:query
                   ([(crate C {(struct Vec[(type T)] where [] { })
                               (trait Foo[(lifetime a)] where [] {  })
                               (impl[(type T) (lifetime a)] Foo[a] for (Vec < T >) where [(T : a)] { })
                               })]
                    C)
                   (?∀ [(type X) (lifetime body)]
                       (?=> [(X : body)]
                            (?∃ [(lifetime a)]
                                ((Vec < X >) : Foo[a]))))
                   )

                  [(:- () (() ((a -outlived-by- body))))]))
           'ok))
  )