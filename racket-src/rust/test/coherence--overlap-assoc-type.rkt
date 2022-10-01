#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../libcore.rkt"
         )

(module+ test
  (redex-let*
   formality-rust

   [
    ]

   (traced '()
           (test-equal
            (term (rust:is-program-ok ([(crate one { (trait ForeignTrait[] where [] {})
                                                     (struct ForeignStruct[] where [] {})
                                                     })
                                        (crate two { (trait LocalTrait[] where [] {})
                                                     (impl[] LocalTrait[] for (ForeignStruct < >) where [] {})
                                                     (impl[(type T)] LocalTrait[] for T where [(T : ForeignTrait[])] {})
                                                     })
                                        ]
                                       two)))
            #f
            ))

   (traced '()
           (test-equal
            (term (rust:is-program-ok ([(crate one { (trait ForeignTrait[] where [] { (type Ty[] : [] where []) })
                                                     (struct ForeignStruct[] where [] {})
                                                     })
                                        (crate two { (trait LocalTrait[] where [] {})
                                                     (impl[] LocalTrait[] for (ForeignStruct < >) where [] {})
                                                     (impl[(type T)] LocalTrait[] for T where [(T : ForeignTrait[])
                                                                                               (< T as ForeignTrait[] > :: Ty[] == ())] {})
                                                     })
                                        ]
                                       two)))
            #f
            ))

   (traced '()
           (test-equal
            (term (rust:is-program-ok ([(crate one { (trait AssocTrait[] where [] { (type Ty[] : [] where []) })
                                                     (struct ForeignStruct[] where [] {})
                                                     (trait LocalTrait[] where [] {})
                                                     (impl[] LocalTrait[] for (ForeignStruct < >) where [] {})
                                                     (impl[(type T)] LocalTrait[] for T where [(T : AssocTrait[])
                                                                                               (< T as AssocTrait[] > :: Ty[] == ())] {})
                                                     })
                                        ]
                                       one)))
            #t
            ))

   (traced '()
           (test-equal
            (term (rust:is-program-ok ([(crate one { (trait AssocTrait[] where [] { (type Ty[] : [] where []) })
                                                     (impl[] AssocTrait[] for () where [] { (type Ty[] = () where []) })
                                                     (struct ForeignStruct[] where [] {})
                                                     (trait LocalTrait[] where [] {})
                                                     (impl[] LocalTrait[] for () where [] {})
                                                     (impl[(type T)] LocalTrait[] for T
                                                          where [(T : AssocTrait[])
                                                                 (< T as AssocTrait[] > :: Ty[] == ())] {})
                                                     })
                                        ]
                                       one)))
            #f
            ))
   )
  )