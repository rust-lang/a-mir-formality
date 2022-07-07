#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         )

(module+ test
  ; Variant A: fn parameter of type `& a T` implies `T : a`, program OK
  ;
  ; https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=9aa11573053aca0a86c88b7535c31100
  (test-term-false ; FIXME
   (rust:is-core-crate-ok
    [(fn foo[(lifetime a) (type T)]((& a T)) -> ()
         where [((& a T) : OK[])]
         trusted-fn-body
         )
     (trait OK[] where [] {})
     ]
    ))

  ; Variant B: no fn parameter, don't know `T : a`, program NOT ok
  ;
  ; https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=810f82d2047ee1b1b1aa010e1f37dcbb
  (traced '()
          (test-term-false
           (rust:is-core-crate-ok
            [(fn foo[(lifetime a) (type T)]() -> ()
                 where [((& a T) : OK[])]
                 trusted-fn-body
                 )
             (trait OK[] where [] {})
             ]
            )))

  ; Variant C: fn return type `& a T` implies `T : a`, program OK
  ;
  ; https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=9aa11573053aca0a86c88b7535c31100
  (test-term-false ; FIXME
   (rust:is-core-crate-ok
    [(fn foo[(lifetime a) (type T)]() -> (& a T)
         where [((& a T) : OK[])]
         trusted-fn-body
         )
     (trait OK[] where [] {})
     ]
    ))
  )
