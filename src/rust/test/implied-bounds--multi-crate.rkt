#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         )

(module+ test
  (redex-let*
   formality-rust

   [(;; # Crate A
     ;;
     ;; trait Debug { }
     ;; impl Debug for i32 { }
     ;;
     Rust/CrateDecl_A (term (crate CrateA { (trait Debug[] where [] {})
                                            (impl[] Debug[] for i32 where [] {})
                                            })))

    (;; # Crate B
     ;;
     ;; trait WithDebug<T: Debug> { }
     ;;
     ;; struct Foo<T: Debug> { }
     ;; impl<T> WithDebug<T> for Foo<T> { }
     ;;
     ;; # Crate Be
     ;;
     ;; includes (feature expanded-implied-bounds)
     [Rust/CrateItemDecl_B ...] (term [(trait WithDebug[(type T)] where [(T : Debug[])] {})
                                       (struct Foo[(type T)] where [(T : Debug[])] {})
                                       (impl[(type T)] WithDebug[T] for (Foo < T >) where [] {})
                                       ]))
    (Rust/CrateDecl_B (term (crate B { Rust/CrateItemDecl_B ... })))
    (Rust/CrateDecl_Be (term (crate Be { Rust/CrateItemDecl_B ... (feature expanded-implied-bounds) })))

    (;; # Crate C
     ;;
     ;; No items.
     Rust/CrateDecl_C (term (crate CrateC {})))

    (Rust/Program_AB (term   ([Rust/CrateDecl_A Rust/CrateDecl_B] B)))
    (Rust/Program_ABe (term  ([Rust/CrateDecl_A Rust/CrateDecl_Be] Be)))
    (Rust/Program_ABC (term  ([Rust/CrateDecl_A Rust/CrateDecl_B Rust/CrateDecl_C] C)))
    (Rust/Program_ABeC (term ([Rust/CrateDecl_A Rust/CrateDecl_Be Rust/CrateDecl_C] C)))
    ]


   (;; Crate B cannot prove itself WF
    traced '()
           (test-term-false (rust:is-program-ok Rust/Program_AB)))

   (;; Crate Be CAN prove itself WF
    traced '()
           (test-term-true (rust:is-program-ok Rust/Program_ABe)))

   (;; Crate Be can prove `∀<T> { If (well-formed(Foo<T>)) { is-implemented(T: Debug) } }`
    traced '()
           (test-term-true
            (rust:can-prove-goal-in-program
             Rust/Program_ABe
             (∀ ((type T))
                (implies ((well-formed (type (rigid-ty Foo (T)))))
                         (is-implemented (Debug (T))))))))

   (;; Crate C can also prove `∀<T> { If (well-formed(Foo<T>) { is-implemented(T: Debug) } }`
    traced '()
           (test-term-true
            (rust:can-prove-goal-in-program
             Rust/Program_ABeC
             (∀ ((type T))
                (implies ((well-formed (type (rigid-ty Foo (T))))
                          (well-formed (type T)))
                         (is-implemented (Debug (T)))))
             )))

   (;; and it can prove `∀<T> { If (well-formed(Foo<T>, T)) { is-implemented(Foo<T>: WithDebug<T>) } }`
    traced '()
           (test-term-true
            (rust:can-prove-goal-in-program
             Rust/Program_ABeC
             (∀ ((type T))
                (implies ((well-formed (type (rigid-ty Foo (T))))
                          (well-formed (type T)))
                         (is-implemented (WithDebug ((rigid-ty Foo (T)) T)))))
             )))

   (;; and it can prove `∀<T> { If (well-formed(Foo<T>, T), is-implemented(T: Debug)) { is-implemented(Foo<T>: WithDebug<T>) } }`
    traced '()
           (test-term-true
            (rust:can-prove-goal-in-program
             Rust/Program_ABeC
             (∀ ((type T))
                (implies ((well-formed (type (rigid-ty Foo (T))))
                          (well-formed (type T))
                          (is-implemented (Debug (T))))
                         (is-implemented (WithDebug ((rigid-ty Foo (T)) T)))))
             )))
   )
  )