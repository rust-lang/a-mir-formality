#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../ty/user-ty.rkt"
         "../prove.rkt"
         )


(module+ test
  (redex-let*
   formality-rust

   []

   ; fn argument<'a, 'b>(x: &'a u32) -> &'b u32 { x }
   (traced '()
           (test-equal (term (rust:is-program-ok
                              ([(crate return_wrong_ref
                                       {(fn argument[(lifetime %b) (lifetime %a)] ((& %a u32)) -> (& %b u32)
                                            where []
                                            (∃ [(lifetime ?0)
                                                (lifetime ?1)
                                                (lifetime ?2)
                                                (lifetime ?3)]
                                               {[(_0 (user-ty (& ?2 u32)) mut)
                                                 (_1 (user-ty (& ?3 u32)) ())]

                                                [(bb0 {
                                                       [(_0 = (ref ?0 () (* _1)))]
                                                       return
                                                       })]
                                                }))
                                        })
                                ]
                               return_wrong_ref)))
                       #f))

   ; fn argument<'a, 'b>(x: &'a u32) -> &'a u32 { x }
   (traced '()
           (test-equal (term (rust:is-program-ok
                              ([(crate return_wrong_ref
                                       {(fn argument[(lifetime %a)] ((& %a u32)) -> (& %a u32)
                                            where []
                                            (∃ [(lifetime ?0)
                                                (lifetime ?1)
                                                (lifetime ?2)
                                                (lifetime ?3)]
                                               {[(_0 (user-ty (& ?2 u32)) mut)
                                                 (_1 (user-ty (& ?3 u32)) ())]

                                                [(bb0 {
                                                       [(_0 = (ref ?0 () (* _1)))]
                                                       return
                                                       })]
                                                }))
                                        })
                                ]
                               return_wrong_ref)))
                       #t))

   ; fn argument<'a, 'b>(x: &'a u32, y: &'b u32) -> &'a u32 { x }
   (traced '()
           (test-equal (term (rust:is-program-ok
                              ([(crate return_wrong_ref
                                       {(fn argument[(lifetime %b) (lifetime %a)] ((& %a u32) (& %b u32)) -> (& %a u32)
                                            where []
                                            (∃ [(lifetime ?0)
                                                (lifetime ?1)
                                                (lifetime ?2)
                                                (lifetime ?3)
                                                (lifetime ?4)]
                                               {[(_0 (user-ty (& ?2 u32)) mut)
                                                 (_1 (user-ty (& ?3 u32)) ())
                                                 (_2 (user-ty (& ?4 u32)) ())]

                                                [(bb0 {
                                                       [(_0 = (ref ?0 () (* _1)))]
                                                       return
                                                       })]
                                                }))
                                        })
                                ]
                               return_wrong_ref)))
                       #t))


   ; fn argument<'a, 'b>(x: &'a u32, y: &'b u32) -> &'a u32 { y }
   (traced '()
           (test-equal (term (rust:is-program-ok
                              ([(crate return_wrong_ref
                                       {(fn argument[(lifetime %b) (lifetime %a)] ((& %a u32) (& %b u32)) -> (& %a u32)
                                            where []
                                            (∃ [(lifetime ?0)
                                                (lifetime ?1)
                                                (lifetime ?2)
                                                (lifetime ?3)
                                                (lifetime ?4)]
                                               {[(_0 (user-ty (& ?2 u32)) mut)
                                                 (_1 (user-ty (& ?3 u32)) ())
                                                 (_2 (user-ty (& ?4 u32)) ())]

                                                [(bb0 {
                                                       [(_0 = (ref ?0 () (* _2)))]
                                                       return
                                                       })]
                                                }))
                                        })
                                ]
                               return_wrong_ref)))
                       #f))

   ; fn argument<'a, 'b>(x: &'a u32, y: &'b u32) -> &'a u32 { return x; return y; } // sort of
   (traced '()
           (test-equal (term (rust:is-program-ok
                              ([(crate return_wrong_ref
                                       {(fn argument[(lifetime %a)] ((& %a u32) (& %a u32)) -> (& %a u32)
                                            where []
                                            (∃ [(lifetime ?0)
                                                (lifetime ?1)
                                                (lifetime ?2)
                                                (lifetime ?3)
                                                (lifetime ?4)]
                                               {[(_0 (user-ty (& ?2 u32)) mut)
                                                 (_1 (user-ty (& ?3 u32)) ())
                                                 (_2 (user-ty (& ?4 u32)) ())]

                                                [(bb0 {
                                                       [(_0 = (ref ?0 () (* _1)))
                                                        (_0 = (ref ?0 () (* _2)))]
                                                       return
                                                       })]
                                                }))
                                        })
                                ]
                               return_wrong_ref)))
                       #t))
   )
  )

