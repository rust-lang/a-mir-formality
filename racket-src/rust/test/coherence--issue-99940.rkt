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

   []

   ; Test from this comment:
   ;
   ; https://github.com/rust-lang/rust/issues/99940#issuecomment-1201504984
   ;
   ; The test has one impl for `<() as Assoc>::Ty` and one impl for `LocalTy`.
   ; In the downstream crate, we are not able to prove that `<() as Assoc>::Ty == LocalTy` is
   ; false in coherence-mode: we get back ambiguous. This is weaker than necessary, in some sense,
   ; because an impl DOES exist for `<() as Assoc>::Ty` and it cannot be taken back, but it falls
   ; out from the way we've setup the coherence-mode structure.

   (traced '()
           (test-equal
            (term (rust:is-program-ok ([(crate one { (trait Assoc[] where [] { (type Ty[] : [] where [])})
                                                     (impl[] Assoc[] for () where [] { (type Ty[] = () where [])})

                                                     (trait Trait[] where [] {})
                                                     (impl[] Trait[] for (< () as Assoc[] > :: Ty[]) where [] {})
                                                     })
                                        (crate two { (struct LocalTy[] where [] {})
                                                     (impl[] Trait[] for (LocalTy < >) where [] {})
                                                     })]
                                       two)))
            #f
            ))

   ; Related test: one crate (to sidestep orphan rule restrictions) using a `Mirror` trait where
   ; `<T as Mirror>::Ty` normalizes to `T`.
   ;
   ; In this case, we have one impl for `<() as Mirror>::Ty` and for `u32`. Should be ok.
   (traced '()
           (test-equal
            (term (rust:is-program-ok ([(crate one { (trait Mirror[] where [] { (type Ty[] : [] where [])})
                                                     (impl[(type T)] Mirror[] for T where [] { (type Ty[] = T where [])})

                                                     (trait Trait[] where [] {})
                                                     (impl[] Trait[] for (< () as Mirror[] > :: Ty[]) where [] {})
                                                     (impl[] Trait[] for u32 where [] {})
                                                     })
                                        ]
                                       one)))
            #t
            ))

   ; Another mirror test, but with `<() as Mirror>::Ty` and for `()`. Should not be ok.
   (traced '()
           (test-equal
            (term (rust:is-program-ok ([(crate one { (trait Mirror[] where [] { (type Ty[] : [] where [])})
                                                     (impl[(type T)] Mirror[] for T where [] { (type Ty[] = T where [])})

                                                     (trait Trait[] where [] {})
                                                     (impl[] Trait[] for (< () as Mirror[] > :: Ty[]) where [] {})
                                                     (impl[] Trait[] for () where [] {})
                                                     })
                                        ]
                                       one)))
            #f
            ))

   )


  )
