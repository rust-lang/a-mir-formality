#lang racket
(require redex/reduction-semantics
         "universe-check.rkt"
         "../hypothesized-bounds.rkt"
         "../grammar.rkt"
         "../kind.rkt"
         "../inequalities.rkt"
         "../where-clauses.rkt"
         "../extrude.rkt"
         "../../logic/env.rkt"
         )
(provide outlives/one/substituted
         )

(define-metafunction formality-ty
  outlives/one/substituted : Env (Parameter OutlivesOp Parameter) -> (Env Goals) or Error

  [; X : X:
   ;   Always ok.
   (outlives/one/substituted Env (Parameter OutlivesOp Parameter))
   (Env ())
   ]

  [; 'static : X for all X.
   (outlives/one/substituted Env (static -outlives- Parameter))
   (Env ())
   ]

  [; And the reverse
   (outlives/one/substituted Env (Parameter -outlived-by- static))
   (Env ())
   ]

  [; Try to reduce to subproblems, if we can.
   (outlives/one/substituted Env (Parameter_0 -outlives- Parameter_1))
   (Env_out Goals_out)
   (where #t (any? (parameter-has-type-kind? Env Parameter_0)
                   (parameter-has-type-kind? Env Parameter_1)))
   (where (Env_out Goals_out) (outlives/one/substituted/reduce Env (Parameter_0 -outlives- Parameter_1)))
   ]

  [; Try to reduce to subproblems, if we can.
   (outlives/one/substituted Env (Parameter_0 -outlived-by- Parameter_1))
   (Env_out Goals_out)
   (where #t (any? (parameter-has-type-kind? Env Parameter_0)
                   (parameter-has-type-kind? Env Parameter_1)))
   (where (Env_out Goals_out) (outlives/one/substituted/reduce Env (Parameter_1 -outlives- Parameter_0)))
   ]

  [; !A -outlives- !B
   ; !A -outlived-by- !B
   (outlives/one/substituted Env (VarId_!A OutlivesOp VarId_!B))
   (Env ((Any Goals)))

   (where (lifetime ∀ _) (var-binding-in-env Env VarId_!A))
   (where (lifetime ∀ _) (var-binding-in-env Env VarId_!B))
   (where Goals (bound-placeholder-from-hypotheses Env VarId_!A OutlivesOp VarId_!B))
   ]

  [; !A -outlives- static
   (outlives/one/substituted Env (VarId_!A -outlives- static))
   (Env ((Any Goals)))

   (where (lifetime ∀ _) (var-binding-in-env Env VarId_!A))
   (where Goals (bound-placeholder-from-hypotheses Env VarId_!A -outlives- static))
   ]

  [; ?X -outlives- P where P in universe(?X):
   ;    * Adds `?X -outlives- P` as a constraint
   ;    * For each `P1 -outlives- ?X` constraint, require `P1 -outlives- P`
   (outlives/one/substituted Env (VarId_?X OutlivesOp Parameter))
   (Env_b ((Parameter_b OutlivesOp Parameter) ...))
   (where #t (env-contains-existential-var Env VarId_?X))
   (where #t (universe-check-ok? Env VarId_?X Parameter))
   (where/error (Parameter_b ...) (known-bounds Env OutlivesOp VarId_?X))
   (where/error Env_b (env-with-var-related-to-parameter Env VarId_?X OutlivesOp Parameter))
   ]

  [; ?X -outlives- P where P NOT in universe(?X):
   ;    * Extrude a `P1` in `universe(?X)` such that `P1 -outlives- P`
   ;    * Require that `?X -outlives- P1`
   (outlives/one/substituted Env (VarId_?X OutlivesOp Parameter))
   (Env_e (Goal_e ... (VarId_?X OutlivesOp Parameter_e)))
   (where #t (env-contains-existential-var Env VarId_?X))
   (where #f (universe-check-ok? Env VarId_?X Parameter))
   (where/error Universe_VarId (universe-of-var-in-env Env VarId_?X))
   (where/error (Env_e Parameter_e (Goal_e ...)) (extrude-parameter Env Universe_VarId OutlivesOp VarId_?X Parameter))
   ]

  [; P -outlives- ?X (regardless of universe):
   ;    * Reverse of one of the two cases above.
   (outlives/one/substituted Env (Parameter OutlivesOp VarId_?X))
   (outlives/one/substituted Env (VarId_?X (invert-inequality-op OutlivesOp) Parameter))
   (where #t (env-contains-existential-var Env VarId_?X))
   ]
  )

(define-metafunction formality-ty
  ;; Outlives relations involving types can often be reduced to subproblems.
  outlives/one/substituted/reduce : Env_in (Parameter_a -outlives- Parameter_b) -> (Env Goals) or ()

  #:pre (any? (parameter-has-type-kind? Env_in Parameter_a)
              (parameter-has-type-kind? Env_in Parameter_b)
              )

  [; (&'a T : P) or (&'a mut T : P) if
   ;     'a : P
   ;
   ; This is a hack / optimization on the more general case below that is based on knowing
   ; that `&'a T` is only well-formed it `T : 'a`. The general rule would have us prove
   ; that *both* `T : P` and `'a : P`. But if `T : 'a` and `'a : P`, then `T : P` is implied by
   ; transitivity, so we can check check whether `'a : P` and that should be good enough.
   ;
   ; We could do this for arbitrary rigid types if we inspected their where-clauses,
   ; but I didn't feel like writing up that logic just now, and the rule below is not wrong,
   ; it just produces more answers than are necessary. (So, alternatively, we could also
   ; work on the "answer subsumption" logic.)
   (outlives/one/substituted/reduce Env ((TyRigid (Ref _) (Lt _)) -outlives- Parameter))
   (Env ((Lt -outlives- Parameter)))
   ]

  [; R<Pr_0...Pr_n> : P1 if
   ;     ∀i (Pr_i : P1)
   (outlives/one/substituted/reduce Env ((TyRigid RigidName (Parameter ...)) -outlives- Parameter_r))
   (Env ((Parameter -outlives- Parameter_r) ...))
   ]

  [; (P : &'a T) or (P : &'a mut T) if
   ;     P : 'a
   ;
   ; This is a hack / optimization on the more general case below that is based on knowing
   ; that `&'a T` is only well-formed it `T : 'a`. The general rule would have us prove
   ; that *either* `P : T` or `P : 'a`. But if `P : T`, then `P : 'a` too, so we can just check
   ; whether `P : 'a` and that is good enough.
   ;
   ; We could do this for arbitrary rigid types if we inspected their where-clauses,
   ; but I didn't feel like writing up that logic just now, and the rule below is not wrong,
   ; it just produces more answers than are necessary. (So, alternatively, we could also
   ; work on the "answer subsumption" logic.)
   (outlives/one/substituted/reduce Env (Parameter -outlives- (TyRigid (Ref _) (Lt _))))
   (Env ((Parameter -outlives- Lt)))
   ]

  [; P : R<Pr_0...Pr_n> if
   ;     ∃i (P0 : Pr_i)
   (outlives/one/substituted/reduce Env (Parameter -outlives- (TyRigid RigidName (Parameter_r ...))))
   (Env ((Any ((Parameter -outlives- Parameter_r) ...))))
   ]

  [; A<Pa ...> : P if
   ;      ∀i. (Pa_i : P) or
   ;      (A<Pa ...> ~~~> T; T : P)
   ;
   ; To establish that an alias type outlives P we can either normalize the alias type or
   ; we can relate P to each of the alias type's parameters. The latter is based on the reasoning
   ; that the alias must be a function of its inputs and other static things.
   (outlives/one/substituted/reduce Env ((TyAlias AliasName (Parameter_a ...)) -outlives- Parameter))
   (Env ((Any (Goal_each Goal_n))))
   (where/error Goal_each (All ((Parameter_a -outlives- Parameter) ...)))
   (where/error Goal_n (∃ ((type T)) (All ((Normalize (TyAlias AliasName (Parameter_a ...)) T)
                                           (T -outlives- Parameter)))))
   ]

  [; P : A<Pa ...> if
   ;      (A<Pa ...> ~~~> T; P : T)
   ;
   ; To establish that P outlives an alias type we *must* normalize.
   ; No matter what its arguments, the alias type could normalize to `i32` or some such thing,
   ; in which case only static outlives it.
   (outlives/one/substituted/reduce Env ((TyAlias AliasName (Parameter_a ...)) -outlives- Parameter))
   (Env (Goal_n))
   (where/error Goal_n (∃ ((type T)) (All ((Normalize (TyAlias AliasName (Parameter_a ...)) T)
                                           (Parameter -outlives- T)))))
   ]

  [; !X : T if
   ;    `T1 : T` for any `X : T1` (`X -outlives- T1` or `T1 -outlived-by- X`) from environment.
   (outlives/one/substituted/reduce Env (VarId -outlives- Parameter))
   (Env ((Any Goals)))

   (where (type ∀ _) (var-binding-in-env Env VarId))
   (where/error Goals (bound-placeholder-from-hypotheses Env VarId -outlives- Parameter))
   ]

  [; T : !X where !X has no known bounds:
   ;
   ; Require T : static
   (outlives/one/substituted/reduce Env (Parameter -outlives- VarId))
   (Env ((Parameter -outlives- static)))

   (where (type ∀ _) (var-binding-in-env Env VarId))
   (where () (bound-placeholder-from-hypotheses Env VarId -outlived-by- Parameter))
   ]

  [; T : !X where !X has known bounds:
   ;    `T1 : T` for any `T1 : X` (`T1 -outlives- X`) from environment.
   ;
   ; We could merge this rule and the one above by always adding static, but it would
   ; yield strictly less precise results.
   (outlives/one/substituted/reduce Env (Parameter -outlives- VarId))
   (Env ((Any (Goal_0 Goal_1 ...))))

   (where (type ∀ _) (var-binding-in-env Env VarId))
   (where/error (Goal_0 Goal_1 ...) (bound-placeholder-from-hypotheses Env VarId -outlived-by- Parameter))
   ]

  [; P0 : ∀ P1 if
   ;     ∀ (P0:P1)
   ;
   ; e.g. `'a : forall<'b> fn(&'b &'a u32)` is true
   ;
   ; e.g. `'a : forall<'b> fn(&'b u32)` is false
   (outlives/one/substituted/reduce Env (Parameter -outlives- (∀ KindedVarIds Ty)))
   (Env ((∀ KindedVarIds (Parameter -outlives- Ty))))
   ]

  [; ∀ P0 : P1 if
   ;     ∃ (P0:P1)
   ;
   ; e.g. `forall<'b> fn(&'b) : 'a` is true because
   ;    `'b : 'a` if `'b = 'a`
   ;
   ; e.g. `forall<'b> fn(&'b &'a u32) : 'a` is false because
   ;    `exists 'b. (fn(&'b &'a u32) : 'a)` is false because
   ;    `(fn(&'b &'a u32) : 'a)` is false because
   ;    `'b : 'a`
   (outlives/one/substituted/reduce Env ((∀ KindedVarIds Ty) -outlives- Parameter))
   (Env ((∃ KindedVarIds (Parameter -outlives- Ty))))
   ]

  [; P0 : (WC => P1) if WC => (P0 : P1)
   (outlives/one/substituted/reduce Env (Parameter -outlives- (Implies WhereClauses Ty)))
   (Env ((Implies (where-clauses->goals WhereClauses) (Parameter -outlives- Ty))))
   ]

  [; (WC => P0) : P1 if WC, (P0 : P1)
   (outlives/one/substituted/reduce Env ((Implies WhereClauses Ty) -outlives- Parameter))
   (Env (Goal_wc ... (Parameter -outlives- Ty)))
   (where (Goal_wc ...) (where-clauses->goals WhereClauses))
   ]

  [; P0 : ∃ P1 if
   ;     ∃ (P0:P1)
   ;
   ; e.g. `'a : dyn Write` is true, because we know nothing about the `dyn Write`, so it could
   ; indeed be a type like `&'a u32`
   ;
   ; e.g. `'a : dyn (Write + 'a)` is also true (same reason)
   ;
   ; e.g. `'a : dyn (Write + 'a + 'b)` requires `'a: 'b`...
   ;    `∃T. ('a : (T ensures (T: Write, T: 'a, T: 'b)))`
   ;    `∃T. ('a : T), (T: Write), (T: 'a), (T: 'b)`
   ;    the first goal adds the bound that `'a : T`
   ;    second goal ... uh ... let's ignore that one for now lol, it's certainly true for *some* T,
   ;        though we will have trouble finding an instance :)
   ;    third goal adds the bound that `T : 'a`
   ;    this will relate `'a: 'a`, which is true
   ;    third goal adds the bound that `T : 'b`
   ;    this will relate `'a: 'b`
   ;
   ; e.g. `'a : dyn (Write + 'static)` requires 'a: 'static`
   ;    `∃T. ('a : (T ensures (T: Write, T: 'static)))`
   ;    `∃T. ('a : T), (T: Write), (T: 'static)`
   ;    the first goal adds the bound that `'a : T`
   ;    second goal ... uh ... let's ignore that one for now lol, it's certainly true for *some* T,
   ;        though we will have trouble finding an instance :)
   ;    third goal adds the bound that `T : 'static`
   ;    this will relate `'a: 'static`
   (outlives/one/substituted/reduce Env (Parameter -outlives- (∃ KindedVarIds Ty)))
   (Env ((∃ KindedVarIds (Parameter -outlives- Ty))))
   ]

  [; ∃ P0 : P1 if
   ;     ∀ (P0:P1)
   ;
   ; e.g. `dyn Write : 'a` is false because
   ;     `∀T. (T : 'a)` is false
   ;     (we know nothing about the `T`)
   ;
   ; e.g. `dyn (Write + 'static) : 'a` is true because
   ;     `∀T. ((T ensures (T: Write, T: 'static)) : 'a)`
   ;     `∀T. (T: Write, T: 'static) => (T : 'a)`
   ;     find the bounds on `T`, find `'static`
   ;     `∀T. (T: Write, T: 'static) => Any (('static : 'a))`
   ;
   ; e.g. `dyn (Write + 'b) : 'a` is false because
   ;     `∀T. ((T ensures (T: Write, T: 'b)) : 'a)`
   ;     `∀T. (T: Write, T: 'b) => (T : 'a)`
   ;     find the bounds on `T`, find `'static`
   ;     `∀T. (T: Write, T: 'b) => Any (('b : 'a))`
   ;     ...and here I assume `'b: 'a` cannot be proven.
   (outlives/one/substituted/reduce Env ((∃ KindedVarIds Ty) -outlives- Parameter))
   (Env ((∀ KindedVarIds (Ty -outlives- Parameter))))
   ]

  [; P0 : (P1 ensures WC) if
   ;    WC, (P0 : P1)
   (outlives/one/substituted/reduce Env (Parameter -outlives- (Ensures Ty WhereClauses)))
   (Env (Goal_wc ... (Parameter -outlives- Ty)))
   (where (Goal_wc ...) (where-clauses->goals WhereClauses))
   ]

  [; (P0 ensures WC) : P1 if
   ;     WC => (P0 : P1)
   (outlives/one/substituted/reduce Env (Parameter -outlives- (Ensures Ty WhereClauses)))
   (Env ((Implies (where-clauses->goals WhereClauses) (Parameter -outlives- Ty))))
   ]

  [(outlives/one/substituted/reduce Env (Parameter_a -outlives- Parameter_b))
   ()
   ]
  )