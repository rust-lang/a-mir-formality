#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         )

(module+ test
  (; Magic trait, implemented in terms of itself, that extends Copy
   redex-let*
   formality-rust

   [(Rust/Program
     (term ([(crate TheCrate { (struct Foo[] where [] { (counter : i32) })
                               (struct Bar[] where [] { (counter : i32) })
                               (trait Magic[] where [(Self : Copy[])] {})
                               (trait Copy[] where [] {})
                               (impl[(type T)] Magic[] for T where [(T : Magic[])] {})
                               (impl[] Copy[] for (Bar < >) where [] {})
                               })]
            TheCrate)))
    ]

   (; All decls in crate are considered 'ok'. In particular, the impl is considered 'ok',
    ; since its where clauses allow it to locally prove that `Self: Copy`.
    traced '()
           (test-equal
            #t
            (term (rust:is-program-ok Rust/Program))))

   (; ...but when we try to use it, we cannot prove that `Foo: Magic`
    ; because `Foo: Copy` does not hold...
    traced '()
           (test-equal
            #f
            (term (rust:can-prove-where-clause-in-program Rust/Program ((Foo < >) : Magic[])))))

   (; ...also cannot prove that `Foo Copy`, of course.
    traced '()
           (test-equal
            #f
            (term (rust:can-prove-where-clause-in-program Rust/Program ((Foo < >) : Copy[])))))

   (; Can prove that `Bar: Magic` -- it has a copy impl
    traced '()
           (test-equal
            #t
            (term (rust:can-prove-where-clause-in-program Rust/Program ((Bar < >) : Magic[])))))

   (; And can prove that `Bar: Copy` -- it has a copy impl
    traced '()
           (test-equal
            #t
            (term (rust:can-prove-where-clause-in-program Rust/Program ((Bar < >) : Copy[])))))
   )

  (; Mutual recursion between Magic and Copy, with Magic implemented in terms of itself,
   ; but no impl of Copy
   redex-let*
   formality-rust

   [(Rust/Program (term ([(crate C { (struct Foo[] where [] { (counter : i32) })
                                     (struct Bar[] where [] { (counter : i32) })
                                     (trait Magic[] where [(Self : Copy[])] {})
                                     (trait Copy[] where [(Self : Magic[])] {})
                                     (impl[(type T)] Magic[] for T where [(T : Magic[])] {})
                                     (impl[] Copy[] for (Bar < >) where [] {})
                                     })] C)))]

   (; All decls in crate are considered 'ok'.
    traced '()
           (test-equal
            #t
            (term (rust:is-program-ok Rust/Program))))

   (; Cannot prove that `Foo Magic`
    traced '()
           (test-equal
            #f
            (term (rust:can-prove-where-clause-in-program Rust/Program ((Foo < >) : Magic[])))))

   (; And cannot prove that `Foo Copy`
    traced '()
           (test-equal
            #f
            (term (rust:can-prove-where-clause-in-program Rust/Program ((Foo < >) : Copy[])))))

   (; Can prove that `Bar: Magic` -- it has a copy impl
    traced '()
           (test-equal
            #t
            (term (rust:can-prove-where-clause-in-program Rust/Program ((Bar < >) : Magic[])))))

   (; And can prove that `Bar: Copy` -- it has a copy impl
    traced '()
           (test-equal
            #t
            (term (rust:can-prove-where-clause-in-program Rust/Program ((Bar < >) : Copy[])))))
   )

  )