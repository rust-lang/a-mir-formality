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
                ((; Just ignore WellFormed rules, not interesting for testing subtyping
                  ∀ ((TyKind T)) (WellFormed (TyKind T)))
                 (; Define a trait `AlwaysImpl` that is implemented for all types
                  ∀ ((TyKind T)) (Implemented (AlwaysImpl (T))))
                 )
                ()
                ()
                )))
    )

   (traced '()
           (ty:test-can-prove Env ((∀ ((TyKind T)) T) <= (scalar-ty u32)))
           )

   (traced '()
           (ty:test-can-prove Env ((scalar-ty u32) >= (∀ ((TyKind T)) T)))
           )

   (traced '()
           (ty:test-cannot-prove Env ((scalar-ty u32) <= (∀ ((TyKind T)) T)))
           )

   (traced '()
           (ty:test-cannot-prove Env ((∀ ((TyKind T)) T) >= (scalar-ty u32)))
           )

   ; Cannot have an implication on the subtype side that doesn't appear on the supertype side
   (traced '()
           (ty:test-cannot-prove Env ((∀ ((TyKind T)) (Implies ((Implemented (NeverImpl (T)))) T))
                                      <=
                                      (∀ ((TyKind T)) T)))
           )

   ; ...unless we can prove it.
   (traced '()
           (ty:test-can-prove Env ((∀ ((TyKind T)) (Implies ((Implemented (AlwaysImpl (T)))) T))
                                   <=
                                   (∀ ((TyKind T)) T)))
           )

   ; OK if the implication is on both sides.
   (traced '()
           (ty:test-can-prove Env ((∀ ((TyKind T)) (Implies ((Implemented (NeverImpl (T)))) T))
                                   <=
                                   (∀ ((TyKind T)) (Implies ((Implemented (NeverImpl (T)))) T))))
           )

   ; OK if the implication is just on supertype side: that means that the consumer will prove it,
   ; but in this case it's not really needed anyway.
   (traced '()
           (ty:test-can-prove Env ((∀ ((TyKind T)) T)
                                   <=
                                   (∀ ((TyKind T)) (Implies ((Implemented (NeverImpl (T)))) T))))
           )

   (test-match
    formality-ty

    ((Exists ((TyKind Ty_U)) (Implies ((Ty_U >= Ty_T)) (Ty_T <= Ty_U)))
     )

    (term (ty:prove-scheme
           Env
           ((∀ ((TyKind T)))
            (Exists ((TyKind U))))
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

            ((Exists
              ((LtKind VarId_A)
               (TyKind VarId_TheTy)
               (LtKind VarId_TheLt))
              (Implies
               ((VarId_TheTy <= VarId_T)
                (VarId_TheLt -outlives- VarId_A))
               ((TyRigid (Ref ()) (VarId_TheLt VarId_TheTy))
                <=
                (TyRigid (Ref ()) (VarId_A VarId_T)))))
             )

            (term (ty:prove-scheme
                   Env
                   ((∀ ((TyKind T)))
                    (Exists ((TyKind U) (LtKind A))))
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
           ((∀ ((TyKind T)))
            )
           ()
           ((TyRigid (Fn "" 1) (T TyUnit)) ; fn(T)
            <=
            (∀ ((TyKind T)) (fn (T) TyUnit)) ; forall<T> fn(T)
            ))))

   (traced '()
           (; Test for ensures: we can add ensures for things we can prove
            test-match
            formality-ty
            ((Exists _ (Implies _ _)))
            (term (ty:prove-scheme
                   Env
                   ((∀ ((TyKind T)))
                    )
                   ((Implemented (Debug (T))))
                   (T
                    <=
                    (Ensures T ((Implemented (Debug (T)))))
                    )))))

   (; Test for ensures: we cannot add ensures for things we cannot prove
    test-match
    formality-ty
    () ; no solutions
    (term (ty:prove-scheme
           Env
           ((∀ ((TyKind T)))
            )
           ()
           (T
            <=
            (Ensures T ((Implemented (Debug (T)))))
            ))))


   (; Test for ensures: we can use ensures on LHS to prove ensures on RHS
    test-match
    formality-ty
    ((Exists _ (Implies _ _)))
    (term (ty:prove-scheme
           Env
           ((∀ ((TyKind T)))
            )
           ()
           ((Ensures T ((Implemented (Debug (T)))))
            <=
            (Ensures T ((Implemented (Debug (T)))))
            ))))

   (; Test for implication in subtype
    ;
    ; cannot use an implication type whose premises we cannot prove
    test-match
    formality-ty
    () ; no solutions
    (term (ty:prove-scheme
           Env
           ((∀ ((TyKind T)))
            )
           ()
           ((Implies ((Implemented (Debug (T)))) T)
            <=
            T
            ))))

   (; Test for implication in subtype:
    ;
    ; can use an implication type whose premises
    ; we CAN prove
    test-match
    formality-ty
    ((Exists () (Implies () _)))
    (term (ty:prove-scheme
           Env
           ((∀ ((TyKind T)))
            )
           ((Implemented (Debug (T))))
           ((Implies ((Implemented (Debug (T)))) T)
            <=
            T
            ))))


   (; Test for implication in supertype
    ;
    ; can add implications, no problem
    test-match
    formality-ty
    ((Exists () (Implies () _)))
    (term (ty:prove-scheme
           Env
           ((∀ ((TyKind T)))
            )
           ()
           (T
            <=
            (Implies ((Implemented (Debug (T)))) T)
            ))))

   (; Test for implication in supertype
    ;
    ; base type must match
    test-match
    formality-ty
    () ; no solutions
    (term (ty:prove-scheme
           Env
           ((∀ ((TyKind T) (TyKind U)))
            )
           ()
           (U
            <=
            (Implies ((Implemented (Debug (T)))) T)
            ))))

   (; Test for implication on both sides
    test-match
    formality-ty
    ((Exists () (Implies () _)))
    (term (ty:prove-scheme
           Env
           ((∀ ((TyKind T)))
            )
           ()
           ((Implies ((Implemented (Debug (T)))) T)
            <=
            (Implies ((Implemented (Debug (T)))) T)
            ))))

   (; #25860 -- the buggy path we have today, where implied bounds
    ; are not reflected in the type -- subtyping works
    test-match
    formality-ty
    ((Exists () (Implies () _))) ; provable! uh-oh!
    (term (ty:prove-scheme
           Env
           ((∀ ((TyKind T) (LtKind X)))
            )
           ()
           ((; fn foo<'a, 'b, T>(_: &'a &'b (), v: &'b T) -> &'a T { v }
             ∀ ((LtKind A) (LtKind B))
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
                   ((∀ ((TyKind T) (LtKind X)))
                    )
                   ()
                   ((; fn foo<'a, 'b, T>(_: &'a &'b (), v: &'b T) -> &'a T { v }
                     ∀ ((LtKind A) (LtKind B))
                       (Implies ((Outlives (B : A))) ; implied bound!
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
            ((Exists () (Implies () _))) ; provable!
            (term (ty:prove-scheme
                   Env
                   ((∀ ((TyKind T) (LtKind X)))
                    )
                   ()
                   ((; fn foo<'a, 'b, T>(_: &'a &'b (), v: &'b T) -> &'a T { v }
                     ∀ ((LtKind A) (LtKind B))
                       (Implies ((Outlives (B : A))) ; implied bound!
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
