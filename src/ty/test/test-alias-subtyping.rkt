#lang racket
(require redex/reduction-semantics
         "hook.rkt"
         "../grammar.rkt"
         "../scheme.rkt"
         "../../util.rkt"
         "../../logic/instantiate.rkt"
         )

(module+ test

  (define-metafunction formality-ty
    ;; convenience for testing: write `(item T)` to reference the alias type `Item`
    item : Ty -> AliasTy

    [(item Ty) (alias-ty Item (Ty))]
    )

  (redex-let*
   formality-ty

   ((Env (term (env-with-clauses-invariants-and-generics
                ((; Just ignore well-formed rules, not interesting for testing subtyping
                  ∀ ((type T)) (well-formed (type T)))
                 (; Define a trait `AlwaysImpl` that is implemented for all types
                  ∀ ((type T)) (is-implemented (AlwaysImpl (T))))
                 (; normalizes-to `Item<Vec<T>>` to `T`
                  ∀ ((type T)) (normalizes-to (item (vec T)) T))
                 )
                ()
                ()
                )))
    )

   (traced '()
           (test-match
            formality-ty
            ((∃ () (implies () _))) ; provable!
            (term (ty:prove-scheme
                   Env
                   ()
                   ()
                   ((item (vec (scalar-ty i32)))
                    <=
                    (scalar-ty i32)
                    )
                   )
                  )
            ))

   (traced '()
           (test-match
            formality-ty
            ;; Cannot prove that `item(T) == item(U)` for arbitrary
            ;; T and U.
            () ; not provable!
            (term (ty:prove-scheme
                   Env
                   ((∀ ((type T) (type U))))
                   ()
                   ((item T)
                    <=
                    (item U)
                    )
                   )
                  )
            ))

   (traced '()
           (test-match
            formality-ty
            ;; Given that `item(T) = i32` and `item(U) = i32`,
            ;; we *can* prove that `item(T) == item(U)`.
            ((∃ () (implies () _))) ; provable!
            (term (ty:prove-scheme
                   Env
                   ((∀ ((type T) (type U))))
                   ((normalizes-to (item T) (scalar-ty i32))
                    (normalizes-to (item U) (scalar-ty i32)))
                   ((item T)
                    <=
                    (item U)
                    )
                   )
                  )
            ))

   (traced '()
           (test-match
            formality-ty
            ;; We cannot prove that `item(&'a()) <= item(&'b ())`
            ;; even though `&'a () <= &'b ()`
            () ; not provable
            (term (ty:prove-scheme
                   Env
                   ((∀ ((lifetime A) (lifetime B))))
                   ((outlives (A : B)))
                   ((item (& A unit-ty))
                    <=
                    (item (& B unit-ty))
                    )
                   )
                  )
            ))

   (traced '()
           (test-match
            formality-ty
            ;; We CAN prove that `item(&'a()) <= item(&'b ())`
            ;; when `&'a () == &'b ()`
            ((∃ () (implies () _))) ; provable
            (term (ty:prove-scheme
                   Env
                   ((∀ ((lifetime A) (lifetime B))))
                   ((outlives (A : B))
                    (outlives (B : A))
                    )
                   ((item (& A unit-ty))
                    <=
                    (item (& B unit-ty))
                    )
                   )
                  )
            ))

   (traced '()
           (test-match
            formality-ty
            ;; We can ALSO prove that `item(Vec<&'a()>) <= item(Vec<&'b ()>)`
            ;; because we can normalize and `&'a () <= &'b ()`
            ((∃ () (implies () _))) ; provable
            (term (ty:prove-scheme
                   Env
                   ((∀ ((lifetime A) (lifetime B))))
                   ((outlives (A : B)))
                   ((item (vec (& A unit-ty)))
                    <=
                    (item (vec (& B unit-ty)))
                    )
                   )
                  )
            ))
   )
  )
