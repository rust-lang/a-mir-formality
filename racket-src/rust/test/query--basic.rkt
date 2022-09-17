#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
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
           (stringify
            (term (rust:query
                   ([(crate C {(struct Vec[(type T)] where [] { })
                               (trait Foo[(lifetime a)] where [] {  })
                               (impl[(type T) (lifetime a)] Foo[a] for (Vec < T >) where [(T : a)] { })
                               })]
                    C)
                   (?∀ [(type X) (lifetime body)]
                       (?=> [(X : body)]
                            (?∃ [(lifetime a)]
                                ((Vec < X >) : Foo[a]))))
                   )))

           ; We find two possibilities. One of them is strictly better than the other
           ; but we are not smart enough to reduce that yet.
           ;
           ; Possibility 1. `'a` = `'body`
           ; Possibility 2. `'a` is something outlived by `'body`.
           (term ["'(:- () (((a body)) ()))"
                  "'(:- () (() ((a -outlived-by- body))))"
                  ])))
  )