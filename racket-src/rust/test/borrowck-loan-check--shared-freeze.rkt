#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         "../libcore.rkt"
         "../prove.rkt"
         )

(module+ test
  (redex-let*
   formality-rust

   ; Baseline test, this should compile.
   ;
   ; fn main() {
   ;     let mut x = 22;
   ;     let y = &x;
   ;     let y = y;
   ; }
   [(Rust/Program (term ([libcore
                          (crate foo
                                 {(fn main[] () -> ()
                                      where []
                                      (∃ [(lifetime ?0)
                                          (lifetime ?1)
                                          (lifetime ?2)]
                                         {[(_0 (user-ty ()) mut)
                                           (_1 (user-ty i32) mut)
                                           (_2 (user-ty (& ?1 i32)) ())
                                           (_3 (user-ty (& ?2 i32)) ())]

                                          [(bb0 {
                                                 [(storage-live _1)
                                                  (_1 = (use (const 22)))
                                                  (fake-read _1)
                                                  (storage-live _2)
                                                  (_2 = (ref ?0 () _1))
                                                  (fake-read _2)
                                                  #;(_1 = (use (const 23)))
                                                  (storage-live _3)
                                                  (_3 = (use (copy _2)))
                                                  (fake-read _3)
                                                  (_0 = (use (const (tuple []))))
                                                  (storage-dead _3)
                                                  (storage-dead _2)
                                                  (storage-dead _1)]
                                                 return
                                                 })]
                                          }))})] foo)))
    ]

   (traced '()
           (test-equal (term (rust:is-program-ok Rust/Program)) #t))
   )
  )