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
  (redex-let*
   formality-ty

   ((Env (term (env-with-clauses-invariants-and-generics
                ((; Just ignore well-formed rules, not interesting for testing subtyping
                  ∀ ((type T)) (well-formed (type T)))
                 (; Define a trait `AlwaysImpl` that is implemented for all types
                  ∀ ((type T)) (is-implemented (AlwaysImpl (T))))
                 )
                ()
                ()
                )))
    )

   (traced '()

           ; Given a T:
           ;
           ; &'a T -outlives- &'b T if
           ;
           ; 'a -outlives- 'b

           (test-match
            formality-ty

            ((∃
              ((lifetime VarId_B) (lifetime VarId_A))
              (implies
               ((VarId_A -outlives- VarId_B))
               ((rigid-ty (ref ()) (VarId_A VarId_T))
                -outlives-
                (rigid-ty (ref ()) (VarId_B VarId_T))))))

            (term (ty:prove-scheme
                   Env
                   ((∀ ((type T)))
                    (∃ ((lifetime A) (lifetime B))))
                   ()
                   ((rigid-ty (ref ()) (A T)) -outlives- (rigid-ty (ref ()) (B T)))
                   ))
            )
           )

   (traced '()
           (test-match
            formality-ty

            ; for<'a> fn(&'a T) -outlives- static

            ((∃
              ()
              (implies
               ()
               ((∀
                 ((lifetime A))
                 (implies
                  [(well-formed (type _))]
                  (rigid-ty
                   (fn-ptr _ 1)
                   ((rigid-ty (ref ()) (A (rigid-ty u32 ())))
                    (rigid-ty (tuple 0) ())))))
                -outlives-
                static))))

            (term (ty:prove-scheme
                   Env
                   ()
                   ()
                   ((user-ty (for ((lifetime A)) (fn ((& A u32)) -> ())))
                    -outlives-
                    static)
                   ))
            )
           )

   (traced '()
           (test-match
            formality-ty

            ; for<'a> fn(&'a &'b T) -outlives- 'b

            ((∃ _ _)) ; provable

            (redex-let*
             formality-ty
             [(Ty_fn (term (user-ty (for ((lifetime A)) (fn ((& A (& B i32))) -> ())))))
              (Goal (term (Ty_fn -outlives- B)))
              ]
             (term (ty:prove-scheme
                    Env
                    ((∃ ((lifetime B))))
                    ()
                    Goal
                    ))
             )
            )
           )

   (traced '()
           (test-match
            formality-ty

            ; For some T, exists A where (A : T)...
            ;
            ; true if (A -outlives- 'static)

            ((∃
              ((lifetime VarId_A))
              (implies
               ((VarId_A -outlives- static))
               (VarId_A -outlives- VarId_T))))

            (term (ty:prove-scheme
                   Env
                   ((∀ ((type T)))
                    (∃ ((lifetime A))))
                   ()
                   (A
                    -outlives-
                    T)
                   ))
            )
           )

   (traced '()

           ; Given any !A and !B, cannot prove
           ;
           ; !A -outlives- !B

           (test-match
            formality-ty

            ()

            (term (ty:prove-scheme
                   Env
                   ((∀ ((lifetime A) (lifetime B))))
                   ()
                   (A -outlives- B)
                   ))
            )
           )

   (traced '()

           ; Given any !A and !B where !A:!B, can prove
           ;
           ; !A -outlives- !B

           (test-match
            formality-ty

            ((∃ () (implies () (VarId_A -outlives- VarId_B))))

            (term (ty:prove-scheme
                   Env
                   ((∀ ((lifetime A) (lifetime B))))
                   ((A -outlives- B))
                   (A -outlives- B)
                   ))
            )
           )

   (traced '()

           ; Given any !A and !B where !A:!B, cannot prove A == B

           (test-match
            formality-ty

            ()

            (term (ty:prove-scheme
                   Env
                   ((∀ ((lifetime A) (lifetime B))))
                   ((A -outlives- B)
                    )
                   (A == B)
                   ))
            )
           )

   (traced '()

           ; Given any !A and !B where !A:!B and !B:!A, can prove A == B

           (test-match
            formality-ty

            ((∃ () (implies () (VarId_A == VarId_B))))

            (term (ty:prove-scheme
                   Env
                   ((∀ ((lifetime A) (lifetime B))))
                   ((A -outlives- B)
                    (B -outlives- A)
                    )
                   (A == B)
                   ))
            )
           )
   )
  )
