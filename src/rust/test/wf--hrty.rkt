#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         )

(module+ test
  (redex-let*
   formality-rust
   [([Rust/CrateItemDecl_Ord-IsOrd-and-friends ...]
     (term [(trait Ord[] where [] {})
            (struct IsOrd[(type T)] where [(T : Ord[])] {})
            (impl[(lifetime a) (type T)] Ord[] for (& a T) where [(T : Ord[])] {})
            (impl[] Ord[] for () where [] {})
            (impl[(type T) (type U)] Ord[] for (tuple T U) where [(T : Ord[]) (U : Ord[])] {})
            ]))]

   (traced '()
           (test-term-false
            (rust:is-core-crate-ok [Rust/CrateItemDecl_Ord-IsOrd-and-friends ...
                                    (const X[(type T)] where [] : (IsOrd < T >) = {trusted-fn-body})
                                    ;                        --    -----------
                                    ;
                                    ; Not WF because `T: Ord` is not satisfied.
                                    ;
                                    ; ✅ Compare to https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=160ed649d8271b2dd76130c879c27974
                                    ])))

   (traced '()
           (test-term-true
            (rust:is-core-crate-ok [Rust/CrateItemDecl_Ord-IsOrd-and-friends ...
                                    (const X[(type T)] where [(T : Ord[])] : (IsOrd < T >) = {trusted-fn-body})
                                    ;                        -------------    -----------
                                    ;
                                    ; WF because `T: Ord` is satisfied.
                                    ;
                                    ; ✅ Compare to https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=cd9684078d84ce89ff69f62c08fe3062
                                    ])))

   (test-term-false
    (rust:is-core-crate-ok [Rust/CrateItemDecl_Ord-IsOrd-and-friends ...
                            (const X[(type T)] where [(T : Ord[])] : (IsOrd < (& static T) >) = {trusted-fn-body})
                            ;                                                 ------------
                            ;
                            ; Not WF because `T: 'static` is not satisfied.
                            ;
                            ; ✅ Compare to https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=b09e2f5f7c8e5c15ea5287e8e94073fe
                            ]))

   (test-term-true
    (rust:is-core-crate-ok [Rust/CrateItemDecl_Ord-IsOrd-and-friends ...
                            (const X[(type T)] where [(T : Ord[]) (T : static)] : (IsOrd < (& static T) >) = {trusted-fn-body})
                            ;                                     ------------             ------------
                            ;
                            ; WF because `T: 'static` IS satisfied.
                            ;
                            ; ✅ Compare to https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=7e585c9fe5db79674e81263c4a46ac06
                            ]))

   (test-term-false
    (rust:is-core-crate-ok [Rust/CrateItemDecl_Ord-IsOrd-and-friends ...
                            (const X[(type T)] where [(T : static)] : (IsOrd < (& static T) >) = {trusted-fn-body})
                            ;                        --------------                      -
                            ;
                            ; Not WF because `T: Ord` is not satisfied.
                            ;
                            ; ✅ Compare to https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=0f9bdc2f3efec95675e206194555eb1e
                            ]))

   (test-term-true
    (rust:is-core-crate-ok [Rust/CrateItemDecl_Ord-IsOrd-and-friends ...
                            (const X[(type T)] where [(T : Ord[])] : (IsOrd < (tuple (& static ()) T) >) = {trusted-fn-body})
                            ;                         -----------                                  -
                            ;
                            ; WF because `T: Ord`
                            ;
                            ; ✅ Compare to https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=a61cec7ec092af944597b14eeded823f
                            ]))

   (test-term-false
    (rust:is-core-crate-ok [Rust/CrateItemDecl_Ord-IsOrd-and-friends ...
                            (const X[(type T)] where [] : (IsOrd < (tuple (& static ()) T) >) = {trusted-fn-body})
                            ;                        --                                 -
                            ;
                            ; Not WF because `T: Ord` not in where clause
                            ;
                            ; ✅ Compare to https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=3355b046f8890dbca4ad80f586e43e6a
                            ]))

   (test-term-true
    (rust:is-core-crate-ok [Rust/CrateItemDecl_Ord-IsOrd-and-friends ...
                            (const X[(type T)] where [(T : Ord[])] : (for [(lifetime a)] (IsOrd < (& a T) >)) = {trusted-fn-body})
                            ; WF because we assume `T: 'a`
                            ;
                            ; ✅ Compare to https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=21cf53700e288511c0d2ac7a7e3caff5
                            ]))

   (test-term-true
    (rust:is-core-crate-ok [Rust/CrateItemDecl_Ord-IsOrd-and-friends ...
                            (const X[(type T)] where [] : (for [(lifetime a)] (IsOrd < (& a T) >)) = {trusted-fn-body})
                            ; WF even though we don't have `T: Ord`, because we assume `IsOrd<&'a T>` is WF.
                            ;
                            ; ✅ Compare to https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=43f5b8244c27a1f625d86114843d5ff5
                            ]))

   (test-term-true
    (rust:is-core-crate-ok [Rust/CrateItemDecl_Ord-IsOrd-and-friends ...
                            (const X[(type T)] where [] : (for [(lifetime a)] (IsOrd < (tuple (& a ()) T) >)) = {trusted-fn-body})
                            ; WF even though we don't have `T: Ord`, because we assume `IsOrd<(&'a (), T)>` is WF.
                            ;
                            ; ✅ Compare to https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=0c7be92a6e1c8eb631335fd207068110
                            ]))

   (test-term-true
    (rust:is-core-crate-ok [Rust/CrateItemDecl_Ord-IsOrd-and-friends ...
                            (const X[(type T)] where [] : (for [(lifetime a)] (tuple (IsOrd < (& a ()) >) (IsOrd < T >))) = {trusted-fn-body})
                            ; WF even though arguably it shouldn't be :)
                            ;
                            ; ✅ Compare to https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=b1bc58a8852c54bb6c41765197452891
                            ]))
   )

  )