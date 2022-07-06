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
  ;; fn foo<'a>() -> &'a u32 {
  ;;     let x = 22;
  ;;     &x
  ;; }
  (traced '(borrow-check
            type-check-goal/Γ
            ✅-FnBody
            well-formed/Γ
            unsafe-check
            well-formed/BasicBlockDecl
            well-formed/Statement)
          (test-equal
           #t
           (term (rust:is-core-crate-ok
                  [(fn foo[(lifetime a)] () -> (& a u32)
                       where []
                       { ∃ [(lifetime ?a)
                            (lifetime ?b)
                            ]
                           ([(_0 (user-ty (& ?a u32)) mut)
                             (_1 (user-ty u32) ()) ; the variable `x`
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