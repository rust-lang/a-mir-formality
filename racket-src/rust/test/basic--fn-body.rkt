#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         )

(module+ test
  ;; Program:
  ;;
  ;; fn foo(x: i32) -> i32 {
  ;;     x
  ;; }
  (traced '()
          (test-equal
           #t
           (term (rust:is-core-crate-ok
                  [(fn foo[] (i32) -> i32
                       where []
                       { ∃ [] ([(_0 (user-ty i32) mut)
                                (_1 (user-ty i32) ())
                                ]

                               [(bb0 { [(_0 = (use (copy _1))) ; _0 = _1
                                        ]
                                       return
                                       })
                                ]
                               )
                           })
                   ]))))
  )