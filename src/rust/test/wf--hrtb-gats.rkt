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

   [(Rust/Program (term ([(crate C { (trait Ref[]
                                            where []
                                            { (type Item[(lifetime a)] : [] where [(Self : a)])
                                              })

                                     (impl[(type T)] Ref[] for T
                                          where []
                                          { (type Item[(lifetime a)] = (& a T) where [(T : a)])
                                            })

                                     (trait OK[] where [] {})

                                     (impl[(type T)] OK[] for T where [] {})

                                     })] C)))

    ]

   (traced '()
           (test-term-true
            (rust:can-prove-where-clause-in-program
             Rust/Program
             (∀ [(lifetime a)]
                where []
                ; key point here:
                ;
                ;     this is actually only valid for `l` where `a: l`, but the default
                ;     bounds make it provable.
                (for[(lifetime l)] (< (& a ()) as Ref[] > :: Item[l] == (& l (& a ()))))
                )
             )
            ))

   (traced '()
           (test-term-true
            (rust:can-prove-where-clause-in-program
             Rust/Program
             (∀ [(type T)]
                where []
                ; key point here:
                ;
                ;     same as above but with a generic type T
                (for[(lifetime l)] (< T as Ref[] > :: Item[l] == (& l T)))
                )
             )
            ))

   (traced '()
           (test-term-false
            (rust:can-prove-where-clause-in-program
             Rust/Program
             (∀ [(lifetime a) (type T)]
                where [(for[(lifetime l)] (< T as Ref[] > :: Item[l] == (& l T)))]
                ; key point here:
                ;
                ;     even though we know `for<'l> <T as Ref>::Item<'l> = &'l T`,
                ;     we don't consider `<T as Ref>::Item<'static>` to be WF,
                ;     because the implied where bound requires `T: 'static`.
                ((< T as Ref[] > :: Item[static]) : OK[])
                )
             )
            ))

   (traced '()
           (test-term-true
            (rust:can-prove-where-clause-in-program
             Rust/Program
             (∀ [(lifetime a) (type T) (lifetime b)]
                where [(for[(lifetime l)] (< T as Ref[] > :: Item[l] == (& l T)))
                       (T : b)]
                ; key point here:
                ;^
                ;     in contrast to previous test, we do consider `<T as Ref>::Item<'b>` OK,
                ;     because we have the where-clause `T: 'b` in-scope.
                ((< T as Ref[] > :: Item[b]) : OK[])
                )
             )
            ))
   )
  )

