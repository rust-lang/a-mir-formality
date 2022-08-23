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
  ;; fn foo<'a>() -> &'a i32 {
  ;;     let x = 22;
  ;;     &x
  ;; }
  (traced '()
          (test-equal
           #t
           (term (rust:is-core-crate-ok
                  [(fn foo[(lifetime a)] () -> (& a i32)
                       where []
                       { ∃ [(lifetime ?a)
                            (lifetime ?b)
                            ]
                           ([(_0 (user-ty (& ?a i32)) mut)
                             (_1 (user-ty i32) ()) ; the variable `x`
                             ]

                            [(bb0 { [(_1 = (use (const 22))) ; x = 22
                                     (_0 = (ref ?b () _1))
                                     ]
                                    return
                                    })
                             ]
                            )
                           })
                   ]))))
  )