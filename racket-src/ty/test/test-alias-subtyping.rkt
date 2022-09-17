#lang racket
(require redex/reduction-semantics
         "hook.rkt"
         "../grammar.rkt"
         "../scheme.rkt"
         "../user-ty.rkt"
         "../../util.rkt"
         "../../logic/instantiate.rkt"
         )

(module+ test

  (define-metafunction formality-ty
    ;; convenience for testing: write `(item T)` to reference the alias type `Item`
    item : UserTy -> AliasTy

    [(item UserTy) (alias-ty Item ((user-ty UserTy)))]
    )

  (redex-let*
   formality-ty

   ((Env (term (env-with-clauses-invariants-and-generics
                ((; Just ignore well-formed rules, not interesting for testing subtyping
                  ∀ ((type T)) (well-formed (type T)))
                 (; Define a trait `AlwaysImpl` that is implemented for all types
                  ∀ ((type T)) (is-implemented (AlwaysImpl (T))))
                 (; normalizes-to `Item<Vec<T>>` to `T`
                  ∀ ((type T)) (normalizes-to (item (Vec < T >)) T))
                 )
                ()
                ()
                )))
    )

   (traced '()
           (test-equal
            (term (ty:query
                   Env
                   ()
                   ()
                   ((item (Vec < i32 >))
                    <=
                    (user-ty i32)
                    )
                   )
                  )
            (term ty:provable)
            ))

   (traced '()
           (test-equal
            ;; Cannot prove that `item(T) == item(U)` for arbitrary
            ;; T and U.
            (term (ty:query
                   Env
                   ((∀ ((type T) (type U))))
                   ()
                   ((item T)
                    <=
                    (item U)
                    )
                   )
                  )
            (term []) ; not provable!
            ))

   (traced '()
           (test-equal
            ;; Given that `item(T) = i32` and `item(U) = i32`,
            ;; we *can* prove that `item(T) == item(U)`.
            (term (ty:query
                   Env
                   ((∀ ((type T) (type U))))
                   ((normalizes-to (item T) (user-ty i32))
                    (normalizes-to (item U) (user-ty i32)))
                   ((item T)
                    <=
                    (item U)
                    )
                   )
                  )
            (term ty:provable)
            ))

   (traced '()
           (test-equal
            ;; We cannot prove that `item(&'a()) <= item(&'b ())`
            ;; even though `&'a () <= &'b ()`
            (term (ty:query
                   Env
                   ((∀ ((lifetime A) (lifetime B))))
                   ((A -outlives- B))
                   ((item (& A ()))
                    <=
                    (item (& B ()))
                    )
                   )
                  )
            (term []) ; not provable!
            ))

   (traced '()
           (test-equal
            ;; We CAN prove that `item(&'a()) <= item(&'b ())`
            ;; when `&'a () == &'b ()`
            (term (ty:query
                   Env
                   ((∀ ((lifetime A) (lifetime B))))
                   ((A -outlives- B)
                    (B -outlives- A)
                    )
                   ((item (& A ()))
                    <=
                    (item (& B ()))
                    )
                   )
                  )
            (term ty:provable)
            ))

   (traced '()
           (test-equal
            ;; We can ALSO prove that `item(Vec<&'a()>) <= item(Vec<&'b ()>)`
            ;; because we can normalize and `&'a () <= &'b ()`
            (term (ty:query
                   Env
                   ((∀ ((lifetime A) (lifetime B))))
                   ((A -outlives- B))
                   ((item (Vec < (& A ()) >))
                    <=
                    (item (Vec < (& B ()) >))
                    )
                   )
                  )
            (term ty:provable)))

   )
  )
