#lang racket
(require redex/reduction-semantics
         "hook.rkt"
         "../grammar.rkt"
         "../user-ty.rkt"
         "../../util.rkt"
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

           (test-equal
            (term (ty:query
                   Env
                   ((∀ ((type T)))
                    (∃ ((lifetime A) (lifetime B))))
                   ()
                   ((rigid-ty (ref ()) (A T)) -outlives- (rigid-ty (ref ()) (B T)))
                   ))
            (term [(:- () (() ((A -outlives- B))))])))

   (traced '()
           ; for<'a> fn(&'a T) -outlives- static
           (test-equal
            (term (ty:query
                   Env
                   ()
                   ()
                   ((user-ty (for ((lifetime A)) (fn ((& A u32)) -> ())))
                    -outlives-
                    static)
                   ))
            (term ty:provable)))

   (traced '()
           ; for<'a> fn(&'a &'b T) -outlives- 'b
           (test-equal
            (redex-let*
             formality-ty
             [(Ty_fn (term (user-ty (for ((lifetime A)) (fn ((& A (& B i32))) -> ())))))
              (Goal (term (Ty_fn -outlives- B)))
              ]
             (term (ty:query
                    Env
                    ((∃ ((lifetime B))))
                    ()
                    Goal
                    ))
             )
            (term ty:provable)
            ))

   (traced '()
           (test-match
            formality-ty

            ; For some T, exists A where (A : T)...
            ;
            ; true if (A -outlives- 'static)

            [(:- []
                 ([] [(A -outlives- static)]))]

            (term (ty:query
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

           (test-equal
            (term (ty:query
                   Env
                   ((∀ ((lifetime A) (lifetime B))))
                   ()
                   (A -outlives- B)
                   ))
            (term [])))

   (traced '()

           ; Given any !A and !B where !A:!B, can prove
           ;
           ; !A -outlives- !B

           (test-equal
            (term (ty:query
                   Env
                   ((∀ ((lifetime A) (lifetime B))))
                   ((A -outlives- B))
                   (A -outlives- B)
                   ))
            (term ty:provable)))

   (traced '()

           ; Given any !A and !B where !A:!B, cannot prove A == B

           (test-equal
            (term (ty:query
                   Env
                   ((∀ ((lifetime A) (lifetime B))))
                   ((A -outlives- B)
                    )
                   (A == B)
                   ))
            (term [])))

   (traced '()

           ; Given any !A and !B where !A:!B and !B:!A, can prove A == B

           (test-equal
            (term (ty:query
                   Env
                   ((∀ ((lifetime A) (lifetime B))))
                   ((A -outlives- B)
                    (B -outlives- A)
                    )
                   (A == B)
                   ))
            (term ty:provable)))
   )
  )
