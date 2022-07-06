#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../prove.rkt"
         )

(module+ test
  (traced '()
          (test-equal
           #t
           (term (rust:is-core-crate-ok
                  [(trait Debug[] where [] {})
                   (trait Display[] where [] {})
                   (trait Get[] where []
                          {(fn get[(type T) (lifetime l)]((&mut l Self)) -> () where [(T : Debug[])] trusted-fn-body)
                           ;                                                         ---------------
                           })
                   (impl[] Get[] for () where []
                        {(fn get[(type T) (lifetime l)]((&mut l ())) -> () where [] trusted-fn-body)
                         ;                                                       -- OK
                         })
                   ]
                  ))
           ))
  )
