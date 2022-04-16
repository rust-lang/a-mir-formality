#lang racket
(require redex/reduction-semantics
         "universe-check.rkt"
         "../grammar.rkt"
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

  [; ?X -outlives- P where P in universe(?X):
   ;    * Adds `?X -outlives- P` as a constraint
   ;    * For each `P1 -outlives- ?X` constraint, require `P1 -outlives- P`
   (outlives/one/substituted Env (VarId OutlivesOp Parameter))
   (Env ((Parameter_b OutlivesOp Parameter) ...))
   (where #t (env-contains-existential-var Env VarId))
   (where #t (universe-check-ok? Env VarId Parameter))
   (where/error (Parameter_b ...) (known-bounds Env (invert-inequality-op OutlivesOp) VarId))
   (where/error Env_b (env-with-var-related-to-parameter Env VarId OutlivesOp Parameter))
   ]

  [; ?X -outlives- P where P NOT in universe(?X):
   ;    * Extrude a `P1` in `universe(?X)` such that `P1 -outlives- P`
   ;    * Require that `?X -outlives- P1`
   (outlives/one/substituted Env (VarId OutlivesOp Parameter))
   (Env ((VarId OutlivesOp Parameter_1)))
   (where #t (env-contains-existential-var Env VarId))
   (where #f (universe-check-ok? Env VarId Parameter))
   (where/error Universe_VarId (universe-of-var-in-env Env VarId))
   (where/error Parameter_1 (extrude-parameter Env Universe_VarId OutlivesOp VarId Parameter))
   ]

  [; P -outlives- ?X (regardless of universe):
   ;    * Reverse of one of the two cases above.
   (outlives/one/substituted Env (Parameter OutlivesOp VarId))
   (outlives/one/substituted Env (Parameter (invert-inequality-op OutlivesOp) VarId))
   (where #t (env-contains-existential-var Env VarId))
   ]

  [; P -outlives- ?X where P in universe(?X):
   ;    * Reverse of the above.
   (outlives/one/substituted Env (Parameter_0 -outlives- Parameter_1))
   (Env Goals)
   (where (Env Goals) (outlives/one/substituted/reduce Env (Parameter_0 -outlives- Parameter_1)))
   ]

  [(outlives/one/substituted Env (Parameter_0 -outlived-by- Parameter_1))
   (Env Goals)
   (where (Env Goals) (outlives/one/substituted/reduce Env (Parameter_1 -outlives- Parameter_0)))
   ]

  )

(define-metafunction formality-ty
  ;; Cases where we can reduce the outlives problem to other subproblems.
  outlives/one/substituted/reduce : Env (Parameter -outlives- Parameter) -> (Env Goals) or Error

  [; R<Pr_0...Pr_n> : P1 if
   ;     ∀i (Pr_i : P1)
   (outlives/one/substituted/reduce Env ((TyRigid RigidName (Parameter ...)) -outlives- Parameter_r))
   (Env ((Parameter -outlives- Parameter_r) ...))
   ]

  [; P : R<Pr_0...Pr_n> if
   ;     ∃i (P0 : Pr_i)
   (outlives/one/substituted/reduce Env (Parameter -outlives- (TyRigid RigidName (Parameter_r ...))))
   (Env ((Any ((Parameter -outlives- Parameter_r) ...))))
   ]

  [; P : R<Pr_0...Pr_n> if
   ;     ∃i (P0 : Pr_i)
   (outlives/one/substituted/reduce Env (Parameter -outlives- (TyRigid RigidName (Parameter_r ...))))
   (Env ((Any ((Parameter -outlives- Parameter_r) ...))))
   ]

  [; !X : T if
   ;    `T1 : T` for any `X : T1` (`X -outlives- T1` or `T1 -outlived-by- X`) from environment.
   (outlives/one/substituted/reduce VarIds_exists Env (VarId -outlives- Parameter))
   (Env ((Any ((Parameter_bound -outlives- Parameter) ...))))

   (where #t (env-contains-placeholder-var Env VarId))
   (where/error (Parameter_bound ...) (known-bounds Env -outlived-by- VarId)) ; * FIXME: need to look through hypotheses
   ]

  [; T : !X if
   ;    `T1 : T` for any `T1 : X` (`T1 -outlives- X`) from environment.
   (outlives/one/substituted/reduce VarIds_exists Env (Parameter -outlives- VarId))
   (Env ((Any ((Parameter -outlives- Parameter_bound) ...))))

   (where #t (env-contains-placeholder-var Env VarId))
   (where/error (Parameter_bound ...) (known-bounds Env -outlives- VarId)) ; * FIXME: need to look through hypotheses
   ]

  [; P0 : ∀ P1 if
   ;     ∀ (P0:P1)
   ;
   ; e.g. `'a : forall<'b> fn(&'b &'a u32)` is true
   ;
   ; e.g. `'a : forall<'b> fn(&'b u32)` is false
   (outlives/one/substituted/reduce Env (Parameter -outlives- (ForAll KindedVarIds Ty)))
   (Env (ForAll KindedVarIds (Parameter -outlives- Ty)))
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
   (outlives/one/substituted/reduce Env ((ForAll KindedVarIds Ty) -outlives- Parameter))
   (Env (Exists KindedVarIds (Parameter -outlives- Ty)))
   ]

  [; P0 : (WC => P1) if WC => (P0 : P1)
   (outlives/one/substituted/reduce Env (Parameter -outlives- (Implies WhereClauses Ty)))
   (Env (Implies (where-clauses->goals WhereClauses) (Parameter -outlives- Ty)))
   ]

  [; (WC => P0) : P1 if WC, (P0 : P1)
   (outlives/one/substituted/reduce Env ((Implies WhereClauses Ty) -outlives- Parameter))
   (Env (flatten (where-clauses->goals WhereClauses) ((Parameter -outlives- Ty))))
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
   (outlives/one/substituted/reduce Env (Parameter -outlives- (Exists KindedVarIds Ty)))
   (Env (Exists KindedVarIds (Parameter -outlives- Ty)))
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
   (outlives/one/substituted/reduce Env ((Exists KindedVarIds Ty) -outlives- Parameter))
   (Env (ForAll KindedVarIds (Ty -outlives- Parameter)))
   ]

  [; P0 : (P1 ensures WC) if
   ;    WC, (P0 : P1)
   (outlives/one/substituted/reduce Env (Parameter -outlives- (Ensures Ty WhereClauses)))
   (Env (flatten (where-clauses->goals WhereClauses) ((Parameter -outlives- Ty))))
   ]

  [; (P0 ensures WC) : P1 if
   ;     WC => (P0 : P1)
   (outlives/one/substituted/reduce Env (Parameter -outlives- (Ensures Ty WhereClauses)))
   (Env (Implies (where-clauses->goals WhereClauses) (Parameter -outlives- Ty)))
   ]
  )