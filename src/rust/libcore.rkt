#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         )

(provide libcore)

(;; Defines a `Rust/CrateDecl` for (a subset of) `libcore` that can be included in various tests.
 define-term libcore

  ,(redex-let*
    formality-rust
    [(Rust/CrateDecl
      (term (crate core { (trait core:Sized[] where[] {})
                          (trait core:Send[] where[] {})
                          (trait core:Sync[] where[] {})
                          (trait core:Copy[] where[] {})
                          (trait core:Drop[] where[] {})
                          (impl[] core:Copy[] for u32 where [] {})
                          (impl[] core:Copy[] for i32 where [] {})
                          })))
     ]
    (term Rust/CrateDecl)
    )
  )

(module+ test
  (test-match formality-rust Rust/CrateDecl (term libcore))
  )