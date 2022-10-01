#lang racket
(require redex/reduction-semantics
         "../logic/env.rkt"
         "../logic/substitution.rkt"
         "grammar.rkt"
         )
(provide orphan-check
         orphan-check-associated-ty
         )

(define-metafunction formality-decl
  orphan-check : DeclProgram Env TraitRef -> boolean

  ;; From https://rust-lang.github.io/rfcs/2451-re-rebalancing-coherence.html:
  ;;
  ;; Given `impl<P1..=Pn> Trait<T1..=Tn> for T0`, an impl is valid only if at least one of the following is true:
  ;;
  ;; - `Trait` is a local trait
  ;; - All of
  ;;  - At least one of the types `T0..=Tn` must be a local type. Let `Ti` be the
  ;;    first such type.
  ;;  - No uncovered type parameters `P1..=Pn` may appear in `T0..Ti` (excluding
  ;;    `Ti`)
  ;;
  ;; Given the following definitions:
  ;;
  ;; Covered Type: A type which appears as a parameter to another type. For example,
  ;; `T` is uncovered, but the `T` in `Vec<T>` is covered. This is only relevant for
  ;; type parameters.
  ;;
  ;; Fundamental Type: A type for which you cannot add a blanket impl backwards
  ;; compatibly. This includes `&`, `&mut`, and `Box`. Any time a type `T` is
  ;; considered local, `&T`, `&mut T`, and `Box<T>` are also considered local.
  ;; Fundamental types cannot cover other types. Any time the term "covered type" is
  ;; used, `&T`, `&mut T`, and `Box<T>` are not considered covered.
  ;;
  ;; Local Type: A struct, enum, or union which was defined in the current crate.
  ;; This is not affected by type parameters. `struct Foo` is considered local, but
  ;; `Vec<Foo>` is not. `LocalType<ForeignType>` is local. Type aliases and trait
  ;; aliases do not affect locality.

  [; "`Trait` is a local trait"
   (orphan-check DeclProgram Env (TraitId Parameters))
   #t
   (where/error (CrateDecls CrateId) DeclProgram)
   (where CrateId (crate-defining-trait-with-id CrateDecls TraitId))
   ]

  [; "`Ti` is a local type and no uncovered type parameters appear in `T0..Ti`"
   (orphan-check DeclProgram Env (_ [Parameter_0 ... Parameter_i Parameter_j ...]))
   #t
   (where #t (is-local-parameter DeclProgram Env Parameter_i))
   (where [#f ...] [(has-uncovered-placeholder Env Parameter_0) ...])
   ]

  [; Anything else fails.
   (orphan-check DeclProgram Env _)
   #f
   ]
  )

(define-metafunction formality-decl
  ;; Given a reference to an associated type `TraitId::AssociatedTyId`, extract out
  ;; the first N parameters (the ones from the trait) and check whether that trait-reference
  ;; passes the orphan check.
  orphan-check-associated-ty : DeclProgram Env TraitId AssociatedTyId Parameters -> boolean

  [(orphan-check-associated-ty DeclProgram Env TraitId AssociatedTyId Parameters)
   (orphan-check DeclProgram Env (TraitId [Parameter_trait ...]))
   (where/error (CrateDecls _) DeclProgram)
   (where/error (trait TraitId [KindedVarId ..._n] where _ _) (trait-with-id CrateDecls TraitId))
   (where/error [Parameter_trait ..._n Parameter_assoc ...] Parameters)
   ]
  )

(define-metafunction formality-decl
  is-local-parameter : DeclProgram Env Parameter -> boolean

  ; Lifetimes are not local.

  [(is-local-parameter _ _ Lt)
   #f
   ]

  ; Scalars and built-in types are local to core.

  [(is-local-parameter (_ core) _ (rigid-ty ScalarId _))
   #t
   ]

  [(is-local-parameter (_ core) _ (rigid-ty (fn-ptr _ _) _))
   #t
   ]

  [(is-local-parameter (_ core) _ (rigid-ty (ref _) _))
   #t
   ]

  ; User-defined types are local if declared in the current crate

  [(is-local-parameter (CrateDecls CrateId) Env (rigid-ty AdtId _))
   #t
   (where CrateId (crate-defining-adt-with-id CrateDecls AdtId))
   ]

  ; Nothing else is local.

  [(is-local-parameter DeclProgram Env Parameter)
   #f
   ]
  )


(define-metafunction formality-decl
  has-uncovered-placeholder : Env Parameter -> boolean

  ; Lifetimes are not considered uncovered placeholders.

  [(has-uncovered-placeholder Env VarId)
   #f
   (where (_ lifetime Quantifier _) (var-binding-in-env Env VarId))
   ]

  [(has-uncovered-placeholder Env static)
   #f
   (where (_ lifetime Quantifier _) (var-binding-in-env Env VarId))
   ]

  ; Mapped existential variables: recurse with their inferred value

  [(has-uncovered-placeholder Env VarId)
   (has-uncovered-placeholder Env (apply-substitution-from-env Env VarId))
   (where #t (env-contains-existential-var Env VarId))
   (where #t (env-maps-var Env VarId))
   ]

  ; Base cases of uncovered placeholders
  ;
  ; * Universal type parameters
  ; * Aliases (FIXME: alias types?)
  ; * Unbound inference variables

  [(has-uncovered-placeholder Env VarId)
   #t
   (where (_ type ∀ _) (var-binding-in-env Env VarId))
   ]

  [(has-uncovered-placeholder Env VarId)
   #t
   (where #t (env-contains-unmapped-existential-var Env VarId))
   ]

  [(has-uncovered-placeholder Env (alias-ty _ _))
   #t
   ]

  ; Rigid types: ADTs cover their parameters, but others do not.
  ;
  ; FIXME: Fundamental types.

  [(has-uncovered-placeholder _ (rigid-ty AdtId _))
   #f
   ]

  [(has-uncovered-placeholder Env (rigid-ty _ [Parameter ...]))
   (any? (has-uncovered-placeholder Env Parameter) ...)
   ]

  ; Predicate types: unfold and recurse.
  ;
  ; FIXME: Pretty sure that these rules are correct for implies/ensures,
  ; but that's worth thinking over.

  [(has-uncovered-placeholder Env (Quantifier KindedVarIds Ty))
   ; FIXME: We should not count these variables as uncovered. We should track universe of Env on entry
   ; so we know we can ignore them.
   (has-uncovered-placeholder Env_1 Ty_1)
   (where/error (Env_1 Ty_1 _) (instantiate-quantified Env (Quantifier KindedVarIds Ty)))
   ]

  [(has-uncovered-placeholder Env (implies Biformulas Ty))
   (has-uncovered-placeholder Env_1 Ty)
   (where/error Env_1 (env-with-hypotheses Env Biformulas))
   ]

  [(has-uncovered-placeholder Env (ensures Ty Biformulas))
   (has-uncovered-placeholder Env Ty)
   ]

  )