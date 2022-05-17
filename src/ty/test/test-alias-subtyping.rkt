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

    [(item Ty) (TyAlias Item (Ty))]
    )

  (redex-let*
   formality-ty

   ((Env (term (env-with-clauses-invariants-and-generics
                ((; Just ignore WellFormed rules, not interesting for testing subtyping
                  ∀ ((TyKind T)) (WellFormed (TyKind T)))
                 (; Define a trait `AlwaysImpl` that is implemented for all types
                  ∀ ((TyKind T)) (Implemented (AlwaysImpl (T))))
                 (; Normalize `Item<Vec<T>>` to `T`
                  ∀ ((TyKind T)) (Normalize (item (vec T)) T))
                 )
                ()
                ()
                )))
    )

   (traced '()
           (test-match
            formality-ty
            ((Exists () (Implies () _))) ; provable!
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
                   ((∀ ((TyKind T) (TyKind U))))
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
            ((Exists () (Implies () _))) ; provable!
            (term (ty:prove-scheme
                   Env
                   ((∀ ((TyKind T) (TyKind U))))
                   ((Normalize (item T) (scalar-ty i32))
                    (Normalize (item U) (scalar-ty i32)))
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
                   ((∀ ((LtKind A) (LtKind B))))
                   ((Outlives (A : B)))
                   ((item (& A TyUnit))
                    <=
                    (item (& B TyUnit))
                    )
                   )
                  )
            ))

   (traced '()
           (test-match
            formality-ty
            ;; We CAN prove that `item(&'a()) <= item(&'b ())`
            ;; when `&'a () == &'b ()`
            ((Exists () (Implies () _))) ; provable
            (term (ty:prove-scheme
                   Env
                   ((∀ ((LtKind A) (LtKind B))))
                   ((Outlives (A : B))
                    (Outlives (B : A))
                    )
                   ((item (& A TyUnit))
                    <=
                    (item (& B TyUnit))
                    )
                   )
                  )
            ))

   (traced '()
           (test-match
            formality-ty
            ;; We can ALSO prove that `item(Vec<&'a()>) <= item(Vec<&'b ()>)`
            ;; because we can normalize and `&'a () <= &'b ()`
            ((Exists () (Implies () _))) ; provable
            (term (ty:prove-scheme
                   Env
                   ((∀ ((LtKind A) (LtKind B))))
                   ((Outlives (A : B)))
                   ((item (vec (& A TyUnit)))
                    <=
                    (item (vec (& B TyUnit)))
                    )
                   )
                  )
            ))
   )
  )
