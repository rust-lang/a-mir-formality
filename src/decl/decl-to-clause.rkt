#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "where-clauses.rkt"
         "feature-gate.rkt"
         "../logic/env.rkt"
         "../ty/relate.rkt"
         "../ty/could-match.rkt"
         "../ty/user-ty.rkt"
         "../ty/hook.rkt")
(provide env-for-crate-decl
         env-for-crate-decls
         env-for-decl-program
         )

(define-metafunction formality-decl
  ;; Convenience function: add the clauses/hypothesis from a single crate
  ;; into the environment.
  env-for-crate-decl : CrateDecl -> Env

  [(env-for-crate-decl CrateDecl)
   (env-for-crate-decls (CrateDecl) CrateId)
   (where/error (CrateId CrateContents) CrateDecl)
   ]
  )

(define-metafunction formality-decl
  ;; Add the clauses/hypothesis from multiple crates
  ;; into the environment, where CrateId names the current crate.
  env-for-crate-decls : CrateDecls CrateId -> Env

  [(env-for-crate-decls CrateDecls CrateId)
   (env-for-decl-program (CrateDecls CrateId))
   ]
  )

(define-metafunction formality-decl
  ;; Create an environment for the given program.
  env-for-decl-program : DeclProgram -> Env

  [(env-for-decl-program (CrateDecls CrateId))
   (env-with-hook (formality-decl-hook DeclProgram))
   (where/error DeclProgram (CrateDecls CrateId))
   ]
  )


(define-metafunction formality-decl
  ;; The "hook" for a decl program -- given a set of create-decls and a current
  ;; crate, lowers those definitions to program clauses on demand using
  ;; decl-clauses-for-predicate.
  formality-decl-hook : DeclProgram -> Hook

  [(formality-decl-hook DeclProgram)
   (Hook: ,(formality-ty-hook
            (lambda (predicate)
              (term (decl-clauses-for-predicate DeclProgram ,predicate)))
            (lambda ()
              (term (decl-invariants DeclProgram)))
            (lambda (env predicate1 predicate2)
              (term (ty:equate-predicates ,env ,predicate1 ,predicate2)))
            (lambda (env relation)
              (term (ty:relate-parameters ,env ,relation)))
            (lambda (predicate1 predicate2)
              (term (ty:predicates-could-match ,predicate1 ,predicate2)))
            (lambda (goal)
              (term (ty:is-predicate? ,goal)))
            (lambda (goal)
              (term (ty:is-relation? ,goal)))
            (lambda (adt-id)
              (term (generics-for-adt-id DeclProgram ,adt-id)))))
   ]
  )

(define-metafunction formality-decl
  ;; Part of the "hook" for a formality-decl program:
  ;;
  ;; Create the clauses for solving a given predicate
  ;; (right now the predicate is not used).
  decl-clauses-for-predicate : DeclProgram Predicate -> Clauses

  [(decl-clauses-for-predicate DeclProgram Predicate)
   Clauses
   (where/error (Clauses _) (program-rules DeclProgram))]
  )

(define-metafunction formality-decl
  ;; Part of the "hook" for a formality-decl program:
  ;;
  ;; Create the invariants from a given program.
  decl-invariants : DeclProgram -> Invariants

  [(decl-invariants DeclProgram)
   Invariants
   (where/error (_ Invariants) (program-rules DeclProgram))]
  )

(define-metafunction formality-decl
  ;; Return the clauses/hypothesis from multiple crates
  ;; with CrateId as the crate being compiled.
  ;;
  ;; NB: This assumes that we can compile to a complete set of
  ;; clauses. This will eventually not suffice, e.g., with
  ;; auto traits. But this helper is private, so we can refactor
  ;; that later.
  program-rules : DeclProgram -> (Clauses Invariants)

  [(program-rules (CrateDecls CrateId))
   ((flatten (Clauses_c ... Clauses_bi))
    (flatten (Invariants_c ... Invariants_bi)))
   (where (CrateDecl ...) CrateDecls)
   (where/error (Clauses_bi Invariants_bi) (default-rules ()))
   (where/error ((Clauses_c Invariants_c) ...) ((crate-decl-rules CrateDecls CrateDecl CrateId) ...))
   ]
  )

(define-metafunction formality-decl
  ;; Generate the complete set of rules that result from `CrateDecl`
  ;; when checking the crate `CrateId`.
  ;;
  ;; NB: This assumes that we can compile to a complete set of
  ;; clauses. This will eventually not suffice, e.g., with
  ;; auto traits. But this helper is private, so we can refactor
  ;; that later.
  crate-decl-rules : CrateDecls CrateDecl CrateId -> (Clauses Invariants)

  [(crate-decl-rules CrateDecls (CrateId_0 (crate (CrateItemDecl ...))) CrateId_1)
   ((Clause ... ...) (Invariant ... ...))

   (where/error (((Clause ...) (Invariant ...)) ...)
                ((crate-item-decl-rules CrateDecls CrateId_0 CrateItemDecl) ...))
   ]
  )

(define-metafunction formality-decl
  ;; Given:
  ;;
  ;; * a set of crate-declarations
  ;; * the id I of a crate
  ;; * some item in the the crate I
  ;;
  ;; Return a tuple of:
  ;;
  ;; * The clauses that hold in all crates due to this item
  ;; * The invariants that hold
  crate-item-decl-rules : CrateDecls CrateId CrateItemDecl -> (Clauses Invariants)

  [;; For an ADT declaration declared in the crate C, like the following:
   ;;
   ;;     struct Foo<T> where T: Ord { ... }
   ;;
   ;; We generate the following clause
   ;;
   ;;     (∀ ((type T))
   ;;         (well-formed (type (Foo (T)))) :-
   ;;            (well-formed (type T))
   ;;            (is-implemented (Ord T)))
   ;;
   ;; And the following invariants local to the crate C:
   ;;
   ;;     (∀ ((type T))
   ;;         (well-formed (type (Foo (T)))) => (is-implemented (Ord T)))
   ;;
   ;; And the following global invariants:
   ;;
   ;;     (∀ ((type T))
   ;;         (well-formed (type (Foo (T)))) => (well-formed (type T)))
   (crate-item-decl-rules CrateDecls CrateId (AdtKind AdtId KindedVarIds where (WhereClause ...) AdtVariants))
   ((Clause) (Invariant_well-formed ... Invariant_where-clause ...))

   (where/error ((ParameterKind VarId) ...) KindedVarIds)
   (where/error Ty_adt (rigid-ty AdtId (VarId ...)))
   (where/error Clause (∀ KindedVarIds
                          (implies
                           ((well-formed (ParameterKind VarId)) ...
                            (where-clause->goal CrateDecls WhereClause) ...)
                           (well-formed (type Ty_adt)))))
   (where/error (WhereClause_implied ...) (if-crate-has-feature
                                           CrateDecls
                                           CrateId
                                           expanded-implied-bounds
                                           (; with the `expanded-implied-bounds` feature, you get all the where clauses
                                            WhereClause ...)
                                           (; without the `expanded-implied-bounds` feature, you only get the super traits
                                            outlives-clauses (WhereClause ...))
                                           ))
   (where/error (Invariant_where-clause ...) ((∀ KindedVarIds
                                                 (implies
                                                  ((well-formed (type Ty_adt)))
                                                  (where-clause->hypothesis CrateDecls WhereClause_implied)))
                                              ...))
   (where/error (Invariant_well-formed ...) ((∀ KindedVarIds
                                                (implies
                                                 ((well-formed (type Ty_adt)))
                                                 (well-formed (ParameterKind VarId))))
                                             ...))
   ]

  [;; For a trait declaration declared in the crate C, like the following:
   ;;
   ;;     trait Foo<'a, T> where T: Ord { ... }
   ;;
   ;; We generate the following clause that proves `Foo` is implemented
   ;; for some types `(Self 'a T)`. Note that, for `Foo` to be considered
   ;; implemented, all of its input types must be well-formed, it must have
   ;; an impl, and the where-clauses declared on the trait must be met:
   ;;
   ;;     (∀ ((type Self) (lifetime 'a) (type T))
   ;;         (is-implemented (Foo (Self 'a T))) :-
   ;;            (has-impl (Foo (Self 'a T))),
   ;;            (well-formed (type Self)),
   ;;            (well-formed (lifetime 'a)),
   ;;            (well-formed (type T)),
   ;;            (is-implemented (Ord T)))
   ;;
   ;; We also generate the following invariants in the defining crate:
   ;;
   ;;     (∀ ((type Self) (lifetime 'a) (type T))
   ;;         (is-implemented (Foo (Self 'a T))) => (is-implemented (Ord T))
   ;;         (is-implemented (Foo (Self 'a T))) => (well-formed (type Self))
   ;;         (is-implemented (Foo (Self 'a T))) => (well-formed (lifetime 'a))
   ;;         (is-implemented (Foo (Self 'a T))) => (well-formed (type T)))
   (crate-item-decl-rules CrateDecls CrateId  (trait TraitId KindedVarIds where (WhereClause ...) TraitItems))
   ((Clause) (Invariant_well-formed ... Invariant_where-clause ...))

   (where/error ((ParameterKind VarId) ...) KindedVarIds)
   (where/error TraitRef_me (TraitId (VarId ...)))
   (where/error Clause (∀ KindedVarIds
                          (implies
                           ((has-impl TraitRef_me)
                            (well-formed (ParameterKind VarId)) ...
                            (where-clause->goal CrateDecls WhereClause) ...
                            )
                           (is-implemented TraitRef_me))))


   (where/error (WhereClause_implied ...) (if-crate-has-feature
                                           CrateDecls
                                           CrateId
                                           expanded-implied-bounds
                                           (; with the `expanded-implied-bounds` feature, you get all the where clauses
                                            WhereClause ...)
                                           (; without the `expanded-implied-bounds` feature, you only get the super traits
                                            super-where-clauses KindedVarIds (WhereClause ...))
                                           ))
   (where/error (Invariant_where-clause ...)  ((∀ KindedVarIds
                                                  (implies
                                                   ((is-implemented TraitRef_me))
                                                   (where-clause->hypothesis CrateDecls WhereClause_implied))) ...))
   (where/error (Invariant_well-formed ...) ((∀ KindedVarIds
                                                (implies
                                                 ((is-implemented TraitRef_me))
                                                 (well-formed (ParameterKind VarId)))) ...))
   ]

  [;; For an trait impl declared in the crate C, like the followin
   ;;
   ;;     impl<'a, T> Foo<'a, T> for i32 where T: Ord { }
   ;;
   ;; We consider `has-impl` to hold if (a) all inputs are well formed and (b) where
   ;; clauses are satisfied:
   ;;
   ;;     (∀ ((lifetime 'a) (type T))
   ;;         (has-impl (Foo (i32 'a u32))) :-
   ;;             (well-formed (type i32))
   ;;             (well-formed (lifetime 'a))
   ;;             (is-implemented (Ord T)))
   (crate-item-decl-rules CrateDecls CrateId (impl KindedVarIds_impl TraitRef where  WhereClauses_impl ImplItems))
   ((Clause) ())

   (where/error (TraitId (Parameter_trait ...)) TraitRef)
   (where/error (trait TraitId KindedVarIds_trait where _ _) (trait-with-id CrateDecls TraitId))
   (where/error ((ParameterKind_trait _) ...) KindedVarIds_trait)
   (where/error (Goal_wc ...) (where-clauses->goals CrateDecls WhereClauses_impl))
   (where/error Clause (∀ KindedVarIds_impl
                          (implies
                           ((well-formed (ParameterKind_trait Parameter_trait)) ...
                            Goal_wc ...
                            )
                           (has-impl TraitRef))))
   ]

  [;; For a function declared in the crate C, like the following
   ;;
   ;;     fn foo<'a, T>(&'a T) -> &'a T { ... }
   (crate-item-decl-rules CrateDecls CrateId (fn _ KindedVarIds_fn Tys_arg -> Ty_ret where WhereClauses_fn FnBody))
   (() ())
   ]

  [;; For an named constant in the crate C, like the following
   ;;
   ;;    const NAMED<T>: Foo<T> where T: Trait;
   ;;
   ;; we don't create any clauses.
   (crate-item-decl-rules CrateDecls CrateId ConstDecl)
   (() ())
   ]

  [;; For a named static in the crate C, like the following
   ;;
   ;;    static NAMED<T>: Foo<T> where T: Trait;
   ;;
   ;; we don't create any clauses.
   (crate-item-decl-rules CrateDecls CrateId StaticDecl)
   (() ())
   ]

  [;; Feature gates don't introduce any rules
   (crate-item-decl-rules CrateDecls CrateId FeatureDecl)
   (() ())
   ]
  )

(define-metafunction formality-decl
  ;; Given a crate item, return a tuple of:
  ;;
  ;; * The clauses that hold in all crates due to this item
  ;; * The invariants that hold in all crates due to this item
  ;; * The invariants that hold only in the crate that declared this item
  default-rules : () -> (Clauses Invariants)

  ((default-rules ())
   (((well-formed (type (user-ty i32)))
     (well-formed (type (user-ty u32)))
     (well-formed (type (user-ty ())))
     )
    ())
   )

  )

(define-metafunction formality-decl
  super-where-clauses : KindedVarIds WhereClauses -> WhereClauses

  ((super-where-clauses KindedVarIds (WhereClause ...))
   (flatten ((filter-super-where-clauses KindedVarIds WhereClause) ...))
   )

  )

(define-metafunction formality-decl
  filter-super-where-clauses : KindedVarIds WhereClause -> WhereClauses

  (; Keep `Self: Trait` bounds
   (filter-super-where-clauses KindedVarIds (VarId_Self : TraitId Parameters))
   ((VarId_Self : TraitId Parameters))
   (where ((type VarId_Self) _ ...) KindedVarIds)
   )

  (; Keep `Self: 'a` bounds
   (filter-super-where-clauses KindedVarIds (VarId_Self : Parameter))
   ((VarId_Self : Parameter))
   (where ((type VarId_Self) _ ...) KindedVarIds)
   )

  (; Discard others
   (filter-super-where-clauses KindedVarIds WhereClause)
   ()
   )

  )

(define-metafunction formality-decl
  outlives-clauses : WhereClauses -> WhereClauses

  ((outlives-clauses (WhereClause ...))
   (flatten ((filter-outlives-where-clause WhereClause) ...))
   )

  )

(define-metafunction formality-decl
  filter-outlives-where-clause : WhereClause -> WhereClauses

  (; Keep `Self: 'a` bounds
   (filter-outlives-where-clause (Parameter_1 : Parameter_2))
   ((Parameter_1 : Parameter_2))
   )

  (; Discard others
   (filter-outlives-where-clause WhereClause)
   ()
   )

  )

(define-metafunction formality-decl
  ;; Part of the "hook" for a formality-decl program:
  ;;
  ;; Create the clauses for solving a given predicate
  ;; (right now the predicate is not used).
  generics-for-adt-id : DeclProgram AdtId -> Generics

  [(generics-for-adt-id (CrateDecls CrateId) AdtId)
   (((VarId (ParameterKind =)) ...) WhereClauses) ; for now we hardcode `=` (invariance) as the variance
   (where/error (AdtKind AdtId ((ParameterKind VarId) ...) WhereClauses AdtVariants) (adt-with-id CrateDecls AdtId))
   ]
  )
