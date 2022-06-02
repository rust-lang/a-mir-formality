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
                  ∀ ((type T)) (normalizes-to (item (Vec T)) T))
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
                   ((item (Vec i32))
                    <=
                    (user-ty i32)
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
                   (((item T) == (user-ty i32))
                    ((item U) == (user-ty i32)))
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
                   ((A : B))
                   ((item (& A ()))
                    <=
                    (item (& B ()))
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
                   ((A : B)
                    (B : A)
                    )
                   ((item (& A ()))
                    <=
                    (item (& B ()))
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
                   ((A : B))
                   ((item (Vec (& A ())))
                    <=
                    (item (Vec (& B ())))
                    )
                   )
                  )
            ))
   )
  )
