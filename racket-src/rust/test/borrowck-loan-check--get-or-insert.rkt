#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         "../libcore.rkt"
         "../prove.rkt"
         )

(define-metafunction formality-rust
  ; Returns the MIR for this function, except that the argument
  ; is inserted to control whether `*x += 1` actually happens.
  ;
  ; fn get(x: &mut i32) -> &i32 {
  ;     let p = &*x;
  ;     if *p > 10 {
  ;         p
  ;     } else {
  ;         *x += 1;  // if argument is #f, then just `*x + 1`
  ;         &*x
  ;     }
  ; }
  get-or-insert : Statement -> Rust/FnDecl

  [(get-or-insert Statement)
   (fn get[(lifetime %a)] ((&mut %a i32)) -> (& %a i32)
       where []
       (∃ [(lifetime ?0)
           (lifetime ?1)
           (lifetime ?2)
           (lifetime ?3)
           (lifetime ?4)
           (lifetime ?5)
           (lifetime ?6)
           (lifetime ?7)
           (lifetime ?8)]
          {
           [(v0 (user-ty (& ?5 i32)) mut)
            (v1 (user-ty (&mut ?6 i32)) ())
            (v2 (user-ty (& ?7 i32)) ())
            (v3 (user-ty bool) mut)
            (v4 (user-ty i32) mut)
            (v5 (user-ty (tuple i32 bool)) mut)
            (v6 (user-ty (& ?8 i32)) ())]

           [(bb0 {
                  [(storage-live v2)
                   (v2 = (ref ?0 () (* v1)))
                   (fake-read v2)
                   (storage-live v3)
                   (storage-live v4)
                   (v4 = (use (copy (* v2))))
                   (v3 = (> (move v4) (const 10)))
                   (storage-dead v4)]
                  (switch-int (move v3) (user-ty bool) [(0 bb2)] [bb1])
                  })
            (bb1 {
                  [(v0 = (ref ?1 () (* v2)))]
                  (goto bb5)
                  })
            (bb2 {
                  []
                  (goto bb3)
                  })
            (bb3 {
                  [(v5 = ((checked +) (copy (* v1)) (const 1)))]
                  (assert (move (field v5 1)) #f [bb4 bb6])
                  })
            (bb4 {
                  [Statement
                   (storage-live v6)
                   (v6 = (ref ?2 () (* v1)))
                   (v0 = (ref ?3 () (* v6)))
                   (storage-dead v6)]
                  (goto bb5)
                  })
            (bb5 {
                  [(storage-dead v3)
                   (storage-dead v2)]
                  return
                  })
            (bb6 {
                  []
                  resume
                  })]
           }))
   ]
  )


(module+ test

  (redex-let*
   formality-rust

   [(Rust/Program_ok (term ([libcore
                             (crate foo { (get-or-insert noop) })
                             ]
                            foo)))
    (Rust/Program_err (term ([libcore
                              (crate foo { (get-or-insert ((* v1) = (use (move (field v5 0))))) })
                              ]
                             foo)))
    ]

   (traced '()
           (test-equal #t (term (rust:is-program-ok Rust/Program_ok))))
   (traced '()
           (test-equal #f (term (rust:is-program-ok Rust/Program_err))))
   )
  )