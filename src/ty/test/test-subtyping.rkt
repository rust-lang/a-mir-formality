#lang racket
(require redex/reduction-semantics
         "hook.rkt"
         "../grammar.rkt"
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
           (ty:test-can-prove Env ((∀ ((type T)) T) <= (scalar-ty u32)))
           )

   (traced '()
           (ty:test-can-prove Env ((scalar-ty u32) >= (∀ ((type T)) T)))
           )

   (traced '()
           (ty:test-cannot-prove Env ((scalar-ty u32) <= (∀ ((type T)) T)))
           )

   (traced '()
           (ty:test-cannot-prove Env ((∀ ((type T)) T) >= (scalar-ty u32)))
           )

   ; Cannot have an implication on the subtype side that doesn't appear on the supertype side
   (traced '()
           (ty:test-cannot-prove Env ((∀ ((type T)) (implies ((is-implemented (NeverImpl (T)))) T))
                                      <=
                                      (∀ ((type T)) T)))
           )

   ; ...unless we can prove it.
   (traced '()
           (ty:test-can-prove Env ((∀ ((type T)) (implies ((is-implemented (AlwaysImpl (T)))) T))
                                   <=
                                   (∀ ((type T)) T)))
           )

   ; OK if the implication is on both sides.
   (traced '()
           (ty:test-can-prove Env ((∀ ((type T)) (implies ((is-implemented (NeverImpl (T)))) T))
                                   <=
                                   (∀ ((type T)) (implies ((is-implemented (NeverImpl (T)))) T))))
           )

   ; OK if the implication is just on supertype side: that means that the consumer will prove it,
   ; but in this case it's not really needed anyway.
   (traced '()
           (ty:test-can-prove Env ((∀ ((type T)) T)
                                   <=
                                   (∀ ((type T)) (implies ((is-implemented (NeverImpl (T)))) T))))
           )

   (test-match
    formality-ty

    ((∃ ((type Ty_U)) (implies ((Ty_U >= Ty_T)) (Ty_T <= Ty_U)))
     )

    (term (ty:prove-scheme
           Env
           ((∀ ((type T)))
            (∃ ((type U))))
           ()
           (T <= U)
           ))
    )

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

            ((∃
              ((lifetime VarId_A)
               (type VarId_TheTy)
               (lifetime VarId_TheLt))
              (implies
               ((VarId_TheTy <= VarId_T)
                (VarId_TheLt -outlives- VarId_A))
               ((rigid-ty (ref ()) (VarId_TheLt VarId_TheTy))
                <=
                (rigid-ty (ref ()) (VarId_A VarId_T)))))
             )

            (term (ty:prove-scheme
                   Env
                   ((∀ ((type T)))
                    (∃ ((type U) (lifetime A))))
                   ()
                   (U <= (& A T))
                   ))
            )
           )

   (; Test for capture avoidance -- we should not be able to prove this!
    test-match
    formality-ty
    () ; no solutions
    (term (ty:prove-scheme
           Env
           ((∀ ((type T)))
            )
           ()
           ((rigid-ty (fn-ptr "" 1) (T TyUnit)) ; fn(T)
            <=
            (∀ ((type T)) (fn (T) TyUnit)) ; forall<T> fn(T)
            ))))

   (traced '()
           (; Test for ensures: we can add ensures for things we can prove
            test-match
            formality-ty
            ((∃ _ (implies _ _)))
            (term (ty:prove-scheme
                   Env
                   ((∀ ((type T)))
                    )
                   ((is-implemented (Debug (T))))
                   (T
                    <=
                    (Ensures T ((is-implemented (Debug (T)))))
                    )))))

   (; Test for ensures: we cannot add ensures for things we cannot prove
    test-match
    formality-ty
    () ; no solutions
    (term (ty:prove-scheme
           Env
           ((∀ ((type T)))
            )
           ()
           (T
            <=
            (Ensures T ((is-implemented (Debug (T)))))
            ))))


   (; Test for ensures: we can use ensures on LHS to prove ensures on RHS
    test-match
    formality-ty
    ((∃ _ (implies _ _)))
    (term (ty:prove-scheme
           Env
           ((∀ ((type T)))
            )
           ()
           ((Ensures T ((is-implemented (Debug (T)))))
            <=
            (Ensures T ((is-implemented (Debug (T)))))
            ))))

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
           ((implies ((is-implemented (Debug (T)))) T)
            <=
            T
            ))))

   (; Test for implication in subtype:
    ;
    ; can use an implication type whose premises
    ; we CAN prove
    test-match
    formality-ty
    ((∃ () (implies () _)))
    (term (ty:prove-scheme
           Env
           ((∀ ((type T)))
            )
           ((is-implemented (Debug (T))))
           ((implies ((is-implemented (Debug (T)))) T)
            <=
            T
            ))))


   (; Test for implication in supertype
    ;
    ; can add implications, no problem
    test-match
    formality-ty
    ((∃ () (implies () _)))
    (term (ty:prove-scheme
           Env
           ((∀ ((type T)))
            )
           ()
           (T
            <=
            (implies ((is-implemented (Debug (T)))) T)
            ))))

   (; Test for implication in supertype
    ;
    ; base type must match
    test-match
    formality-ty
    () ; no solutions
    (term (ty:prove-scheme
           Env
           ((∀ ((type T) (type U)))
            )
           ()
           (U
            <=
            (implies ((is-implemented (Debug (T)))) T)
            ))))

   (; Test for implication on both sides
    test-match
    formality-ty
    ((∃ () (implies () _)))
    (term (ty:prove-scheme
           Env
           ((∀ ((type T)))
            )
           ()
           ((implies ((is-implemented (Debug (T)))) T)
            <=
            (implies ((is-implemented (Debug (T)))) T)
            ))))

   (; #25860 -- the buggy path we have today, where implied bounds
    ; are not reflected in the type -- subtyping works
    test-match
    formality-ty
    ((∃ () (implies () _))) ; provable! uh-oh!
    (term (ty:prove-scheme
           Env
           ((∀ ((type T) (lifetime X)))
            )
           ()
           ((; fn foo<'a, 'b, T>(_: &'a &'b (), v: &'b T) -> &'a T { v }
             ∀ ((lifetime A) (lifetime B))
               (fn ((& A (& B TyUnit)) (& B T)) (& A T)))
            <=
            (; fn(&'static &'x (), &'x T) -> &'static T
             fn ((& static (& X TyUnit)) (& X T)) (& static T))
            )
           )
          )
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
                       (implies ((outlives (B : A))) ; implied bound!
                                (fn ((& A (& B TyUnit)) (& B T)) (& A T))))
                    <=
                    (; fn(&'static &'x (), &'x T) -> &'static T
                     fn ((& static (& X TyUnit)) (& X T)) (& static T))
                    )
                   )
                  )
            ))

   (traced '()
           (; #25860 -- an upcast that discharges implied bound successfully
            test-match
            formality-ty
            ((∃ () (implies () _))) ; provable!
            (term (ty:prove-scheme
                   Env
                   ((∀ ((type T) (lifetime X)))
                    )
                   ()
                   ((; fn foo<'a, 'b, T>(_: &'a &'b (), v: &'b T) -> &'a T { v }
                     ∀ ((lifetime A) (lifetime B))
                       (implies ((outlives (B : A))) ; implied bound!
                                (fn ((& A (& B TyUnit)) (& B T)) (& A T))))
                    <=
                    (; fn(&'x &'static (), &'static T) -> &'x T
                     fn ((& X (& static TyUnit)) (& static T)) (& X T))
                    )
                   )
                  )
            ))
   )
  )
