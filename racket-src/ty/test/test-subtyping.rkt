#lang racket
(require redex/reduction-semantics
         "hook.rkt"
         "../grammar.rkt"
         "../user-ty.rkt"
         "../scheme.rkt"
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
           (ty:test-can-prove Env ((∀ ((type T)) T) <= (user-ty u32)))
           )

   (traced '()
           (ty:test-can-prove Env ((user-ty u32) >= (∀ ((type T)) T)))
           )

   (traced '()
           (ty:test-cannot-prove Env ((user-ty u32) <= (∀ ((type T)) T)))
           )

   (traced '()
           (ty:test-cannot-prove Env ((∀ ((type T)) T) >= (user-ty u32)))
           )

   ; Cannot have an implication on the subtype side that doesn't appear on the supertype side
   (traced '()
           (ty:test-cannot-prove Env ((∀ ((type T)) (implies ((is-implemented (NeverImpl [T]))) T))
                                      <=
                                      (∀ ((type T)) T)))
           )

   ; ...unless we can prove it.
   (traced '()
           (ty:test-can-prove Env ((∀ ((type T)) (implies ((is-implemented (AlwaysImpl [T]))) T))
                                   <=
                                   (∀ ((type T)) T)))
           )

   ; OK if the implication is on both sides.
   (traced '()
           (ty:test-can-prove Env ((∀ ((type T)) (implies ((is-implemented (NeverImpl [T]))) T))
                                   <=
                                   (∀ ((type T)) (implies ((is-implemented (NeverImpl [T]))) T))))
           )

   ; OK if the implication is just on supertype side: that means that the consumer will prove it,
   ; but in this case it's not really needed anyway.
   (traced '()
           (ty:test-can-prove Env ((∀ ((type T)) T)
                                   <=
                                   (∀ ((type T)) (implies ((is-implemented (NeverImpl [T]))) T))))
           )

   (traced '()
           (test-equal
            (term (ty:prove-scheme
                   Env
                   ((∀ ((type T)))
                    (∃ ((type U))))
                   ()
                   (T <= U)
                   ))
            (term [(:- [] ([] ((U >= T))))])
            ))

   (traced '()
           ; Test that, given some placeholder type T
           ;
           ;     U <: &'a T
           ;
           ; is true if
           ;
           ;     U = &'b V
           ;     'a = 'a
           ;
           ; and
           ;
           ;     V <= T
           ;     'b -outlives- 'a
           ;
           ; FIXME. Give Rust's setup with types, we could make `V <= T` just be `V = T`, right?
           (test-match
            formality-ty

            [(:-
              [(VarId_b lifetime ∃ (universe 1)) (VarId_V type ∃ (universe 1))]
              ([(U (rigid-ty (ref ()) (VarId_b VarId_V)))] ; U = &'b V
               [(VarId_V <= T)
                (VarId_b -outlives- a)]
               ))]

            (term (ty:prove-scheme
                   Env
                   ((∀ ((type T)))
                    (∃ ((type U) (lifetime a))))
                   ()
                   (U <= (user-ty (& a T)))
                   ))
            )
           )

   (; Test for capture avoidance -- we should not be able to prove this!
    test-equal
    (term (ty:prove-scheme
           Env
           ((∀ ((type T)))
            )
           ()
           ((user-ty (fn (T) -> ()))
            <=
            (user-ty (for ((type T)) (fn (T) -> ())))
            )))
    (term []))

   (traced '()
           (; Test for ensures: we can add ensures for things we can prove
            test-equal
            (term (ty:prove-scheme
                   Env
                   ((∀ ((type T)))
                    )
                   ((is-implemented (Debug [T])))
                   (T
                    <=
                    (ensures T ((is-implemented (Debug [T]))))
                    )))
            (term ty:provable)))

   (; Test for ensures: we cannot add ensures for things we cannot prove
    test-equal
    (term (ty:prove-scheme
           Env
           ((∀ ((type T)))
            )
           ()
           (T
            <=
            (ensures T ((is-implemented (Debug [T]))))
            )))
    (term []))


   (; Test for ensures: we can use ensures on LHS to prove ensures on RHS
    test-equal
    (term (ty:prove-scheme
           Env
           ((∀ ((type T)))
            )
           ()
           ((ensures T ((is-implemented (Debug [T]))))
            <=
            (ensures T ((is-implemented (Debug [T]))))
            )))
    (term ty:provable))

   (; Test for implication in subtype
    ;
    ; cannot use an implication type whose premises we cannot prove
    test-match
    formality-ty
    () ; no solutions
    (term (ty:prove-scheme
           Env
           ((∀ ((type T)))
            )
           ()
           ((implies ((is-implemented (Debug [T]))) T)
            <=
            T
            ))))

   (; Test for implication in subtype:
    ;
    ; can use an implication type whose premises
    ; we CAN prove
    test-equal
    (term (ty:prove-scheme
           Env
           ((∀ ((type T)))
            )
           ((is-implemented (Debug [T])))
           ((implies ((is-implemented (Debug [T]))) T)
            <=
            T
            )))
    (term ty:provable))


   (; Test for implication in supertype
    ;
    ; can add implications, no problem
    test-equal
    (term (ty:prove-scheme
           Env
           ((∀ ((type T)))
            )
           ()
           (T
            <=
            (implies ((is-implemented (Debug [T]))) T)
            )))
    (term ty:provable))

   (; Test for implication in supertype
    ;
    ; base type must match
    test-equal
    (term (ty:prove-scheme
           Env
           ((∀ ((type T) (type U)))
            )
           ()
           (U
            <=
            (implies ((is-implemented (Debug [T]))) T)
            )))
    (term []))

   (; Test for implication on both sides
    test-equal
    (term (ty:prove-scheme
           Env
           ((∀ ((type T)))
            )
           ()
           ((implies ((is-implemented (Debug [T]))) T)
            <=
            (implies ((is-implemented (Debug [T]))) T)
            )))
    (term ty:provable))

   (; #25860 -- the buggy path we have today, where implied bounds
    ; are not reflected in the type -- subtyping works
    test-equal
    (term (ty:prove-scheme
           Env
           ((∀ ((type T) (lifetime X)))
            )
           ()
           ((; fn foo<'a, 'b, T>(_: &'a &'b (), v: &'b T) -> &'a T { v }
             ∀ ((lifetime A) (lifetime B))
               (user-ty (fn ((& A (& B ())) (& B T)) -> (& A T))))
            <=
            (; fn(&'static &'x (), &'x T) -> &'static T
             user-ty (fn ((& static (& X ())) (& X T)) -> (& static T)))
            )))
    (term ty:provable) ; uh-oh!
    )

   (traced '()
           (; #25860 -- the fixed path, where implied bounds are part of the
            ; resulting type
            test-match
            formality-ty
            () ; no solutions
            (term (ty:prove-scheme
                   Env
                   ((∀ ((type T) (lifetime X)))
                    )
                   ()
                   ((; fn foo<'a, 'b, T>(_: &'a &'b (), v: &'b T) -> &'a T { v }
                     ∀ ((lifetime A) (lifetime B))
                       (implies ((B -outlives- A)) ; implied bound!
                                (user-ty (fn ((& A (& B ())) (& B T)) -> (& A T)))))
                    <=
                    (; fn(&'static &'x (), &'x T) -> &'static T
                     user-ty (fn ((& static (& X ())) (& X T)) -> (& static T)))
                    )
                   )
                  )
            ))

   (traced '()
           (; #25860 -- an upcast that discharges implied bound successfully
            test-equal
            (term (ty:prove-scheme
                   Env
                   ((∀ ((type T) (lifetime X)))
                    )
                   ()
                   ((; fn foo<'a, 'b, T>(_: &'a &'b (), v: &'b T) -> &'a T { v }
                     ∀ ((lifetime A) (lifetime B))
                       (implies ((B -outlives- A)) ; implied bound!
                                (user-ty (fn ((& A (& B ())) (& B T)) -> (& A T)))))
                    <=
                    (; fn(&'x &'static (), &'static T) -> &'x T
                     user-ty (fn ((& X (& static ())) (& static T)) -> (& X T)))
                    )
                   )
                  )
            (term ty:provable)
            ))
   )
  )
