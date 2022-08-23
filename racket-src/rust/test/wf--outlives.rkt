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

   [(Rust/Program (term ([(crate C { (struct Ref[(type T) (lifetime a)]
                                       where [(T : a)]
                                       { })
                                     (struct NoRef[(type T) (lifetime a)]
                                       where []
                                       { })
                                     })] C)))

    ]

   (traced '()
           (test-term-true
            (rust:can-prove-where-clause-in-program
             Rust/Program
             (∀ [(type A)]
                where []
                ; key point here:
                ;
                ;     requires proving `A : 'b`, but that's implied by
                ;     Ref<A, 'b> being WF
                (for[(lifetime b)] ((Ref < A b >) : b))
                )
             )
            ))

   (traced '()
           (test-term-false
            (rust:can-prove-where-clause-in-program
             Rust/Program
             (∀ [(type A)]
                where []
                ; in contrast to previous test, the `NoRef` struct does not
                ; imply a connection between `A` and `b`
                (for[(lifetime b)] ((NoRef < A b >) : b))
                )
             )
            ))
   )
  )
