#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../logic/env.rkt"
         "../ty/unify.rkt"
         "../ty/could-match.rkt"
         "../ty/where-clauses.rkt"
         "../ty/hook.rkt")
(provide env-for-crate-decl
         env-for-crate-decls)

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
            (term (decl-invariants DeclProgram))
            (lambda (env var-ids predicate1 predicate2)
              (term (ty:equate-predicates/vars ,env ,var-ids ,predicate1 ,predicate2)))
            (lambda (env relation)
              (term (ty:relate-parameters ,env ,relation)))
            (lambda (predicate1 predicate2)
              (term (ty:predicates-could-match ,predicate1 ,predicate2)))
            (lambda (goal)
              (term (ty:is-predicate-goal? ,goal)))
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

  [; Rules from crate C to use internally within crate C
   (crate-decl-rules CrateDecls (CrateId (crate (CrateItemDecl ...))) CrateId)
   ((Clause ... ...) (Invariant_all ... ... Invariant_local ... ...))

   (where/error (((Clause ...) (Invariant_all ...) (Invariant_local ...)) ...) ((crate-item-decl-rules CrateDecls CrateItemDecl) ...))
   ]

  [; Rules from crate C to use from other crates -- exclude the invariants, which are
   ; local to crate C, but keep the clauses, which are global.
   (crate-decl-rules CrateDecls (CrateId_0 (crate (CrateItemDecl ...))) CrateId_1)
   ((Clause ... ...) (Invariant_all ... ...))

   (where (CrateId_!_same CrateId_!_same) (CrateId_0 CrateId_1))
   (where/error (((Clause ...) (Invariant_all ...) _) ...) ((crate-item-decl-rules CrateDecls CrateItemDecl) ...))
   ]
  )

(define-metafunction formality-decl
  ;; Given a crate item, return a tuple of:
  ;;
  ;; * The clauses that hold in all crates due to this item
  ;; * The invariants that hold in all crates due to this item
  ;; * The invariants that hold only in the crate that declared this item
  crate-item-decl-rules : CrateDecls CrateItemDecl -> (Clauses Invariants Invariants)

  [;; For an ADT declaration declared in the crate C, like the following:
   ;;
   ;;     struct Foo<T> where T: Ord { ... }
   ;;
   ;; We generate the following clause
   ;;
   ;;     (ForAll ((TyKind T))
   ;;         (WellFormed (TyKind (Foo (T)))) :-
   ;;            (WellFormed (TyKind T))
   ;;            (Implemented (Ord T)))
   ;;
   ;; And the following invariants local to the crate C:
   ;;
   ;;     (ForAll ((TyKind T))
   ;;         (WellFormed Ty) => (Implemented (Ord T)))
   ;;
   ;; And the following global invariants:
   ;;
   ;;     (ForAll ((TyKind T))
   ;;         (WellFormed (Foo (T))) => (WellFormed (T)))
   (crate-item-decl-rules _ (AdtId (AdtKind KindedVarIds (WhereClause ...) AdtVariants)))
   ((Clause) Invariants_wf Invariants_wc)

   (where/error ((ParameterKind VarId) ...) KindedVarIds)
   (where/error Ty_adt (TyRigid AdtId (VarId ...)))
   (where/error Clause (ForAll KindedVarIds
                               (Implies
                                ((WellFormed (ParameterKind VarId)) ...
                                 (where-clause->goal WhereClause) ...)
                                (WellFormed (TyKind Ty_adt)))))
   (where/error Invariants_wc ((ForAll KindedVarIds
                                       (Implies
                                        ((WellFormed (TyKind Ty_adt)))
                                        (where-clause->hypothesis WhereClause)))
                               ...))
   (where/error Invariants_wf ((ForAll KindedVarIds
                                       (Implies
                                        ((WellFormed (TyKind Ty_adt)))
                                        (WellFormed (ParameterKind VarId))))
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
   ;;     (ForAll ((TyKind Self) (LtKind 'a) (TyKind T))
   ;;         (Implemented (Foo (Self 'a T))) :-
   ;;            (HasImpl (Foo (Self T))),
   ;;            (WellFormed (TyKind Self)),
   ;;            (WellFormed (LtKind 'a)),
   ;;            (WellFormed (TyKind T)),
   ;;            (Implemented (Ord T)))
   ;;
   ;; We also generate the following invariants in the defining crate:
   ;;
   ;;     (ForAll ((TyKind Self) (LtKind 'a) (TyKind T))
   ;;         (Implemented (Foo (Self T))) => (Implemented (Ord T))
   ;;         (Implemented (Foo (Self T))) => (WellFormed (TyKind Self))
   ;;         (Implemented (Foo (Self T))) => (WellFormed (LtKind 'a))
   ;;         (Implemented (Foo (Self T))) => (WellFormed (TyKind T)))
   (crate-item-decl-rules _ (TraitId (trait KindedVarIds (WhereClause ...) TraitItems)))
   ((Clause)
    (Hypothesis_wc ...
     Hypothesis_wf ...
     )
    ())

   (where/error ((ParameterKind VarId) ...) KindedVarIds)
   (where/error TraitRef_me (TraitId (VarId ...)))
   (where/error Clause (ForAll KindedVarIds
                               (Implies
                                ((HasImpl TraitRef_me)
                                 (WellFormed (ParameterKind VarId)) ...
                                 (where-clause->goal WhereClause) ...
                                 )
                                (Implemented TraitRef_me))))
   (where/error (Hypothesis_wc ...) ((ForAll KindedVarIds
                                             (Implies
                                              ((Implemented TraitRef_me))
                                              (where-clause->hypothesis WhereClause))) ...))
   (where/error (Hypothesis_wf ...) ((ForAll KindedVarIds
                                             (Implies
                                              ((Implemented TraitRef_me))
                                              (WellFormed (ParameterKind VarId)))) ...))
   ]

  [;; For an trait impl declared in the crate C, like the followin
   ;;
   ;;     impl<'a, T> Foo<'a, T> for i32 where T: Ord { }
   ;;
   ;; We consider `HasImpl` to hold if (a) all inputs are well formed and (b) where
   ;; clauses are satisfied:
   ;;
   ;;     (ForAll ((LtKind 'a) (TyKind T))
   ;;         (HasImpl (Foo (i32 'a u32))) :-
   ;;             (WellFormed (TyKind i32))
   ;;             (WellFormed (TyKind i32))
   ;;             (Implemented (Ord T)))
   (crate-item-decl-rules CrateDecls (impl KindedVarIds_impl TraitRef WhereClauses_impl ImplItems))
   ((Clause) () ())

   (where/error (TraitId (Parameter_trait ...)) TraitRef)
   (where/error (trait KindedVarIds_trait _ _) (item-with-id CrateDecls TraitId))
   (where/error ((ParameterKind_trait _) ...) KindedVarIds_trait)
   (where/error (Goal_wc ...) (where-clauses->goals WhereClauses_impl))
   (where/error Clause (ForAll KindedVarIds_impl
                               (Implies
                                ((WellFormed (ParameterKind_trait Parameter_trait)) ...
                                 Goal_wc ...
                                 )
                                (HasImpl TraitRef))))
   ]

  [;; For an named constant in the crate C, like the following
   ;;
   ;;    const NAMED<T>: Foo<T> where T: Trait;
   ;;
   ;; we don't yet any rules.
   (crate-item-decl-rules CrateDecls ConstDecl)
   (() () ())
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
   (((WellFormed (TyKind (scalar-ty i32)))
     (WellFormed (TyKind (scalar-ty u32)))
     (WellFormed (TyKind TyUnit))
     )
    ())
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
   (where/error AdtContents (item-with-id CrateDecls AdtId))
   (where/error (AdtKind ((ParameterKind VarId) ...) WhereClauses AdtVariants) AdtContents)
   ]
  )