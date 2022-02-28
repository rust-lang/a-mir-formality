#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../ty/grammar.rkt")
(provide env-with-crate-decl
         env-with-crate-decls
         crate-decl-rules)

(define-metafunction formality-decl
  ;; Convenience function: add the clauses/hypothesis from a single crate
  ;; into the environment.
  env-with-crate-decl : Env CrateDecl -> Env

  [(env-with-crate-decl Env CrateDecl)
   (env-with-crate-decls Env (CrateDecl) CrateId)
   (where (CrateId CrateContents) CrateDecl)
   ]
  )

(define-metafunction formality-decl
  ;; Add the clauses/hypothesis from multiple crates
  ;; into the environment, where CrateId names the current crate.
  env-with-crate-decls : Env CrateDecls CrateId -> Env

  [(env-with-crate-decls Env CrateDecls CrateId)
   (env-with-clauses-and-hypotheses Env
                                    (flatten (Clauses_c ... Clauses_bi))
                                    (flatten (Hypotheses_c ... Hypotheses_bi)))
   (where (CrateDecl ...) CrateDecls)
   (where/error (Clauses_bi Hypotheses_bi) (default-rules ()))
   (where/error ((Clauses_c Hypotheses_c) ...) ((crate-decl-rules CrateDecls CrateDecl CrateId) ...))
   ]
  )

(define-metafunction formality-decl
  ;; Generate the complete set of rules that result from `CrateDecl`
  ;; when checking the crate `CrateId`.
  crate-decl-rules : CrateDecls CrateDecl CrateId -> (Clauses Hypotheses)

  [; Rules from crate C to use internally within crate C
   (crate-decl-rules CrateDecls (CrateId (crate (CrateItemDecl ...))) CrateId)
   ((Clause ... ...) (Hypothesis_all ... ... Hypothesis_local ... ...))

   (where/error (((Clause ...) (Hypothesis_all ...) (Hypothesis_local ...)) ...) ((crate-item-decl-rules CrateDecls CrateItemDecl) ...))
   ]

  [; Rules from crate C to use from other crates -- exclude the hypotheses, which are
   ; local to crate C, but keep the clauses, which are global.
   (crate-decl-rules CrateDecls (crate CrateId_0 ((CrateItemDecl ...))) CrateId_1)
   ((Clause ...) (Hypothesis_all ...))

   (where (CrateId_!_same CrateId_!_same) (CrateId_0 CrateId_1))
   (where/error (((Clause ...) (Hypothesis_all ...) _)) ((crate-item-decl-rules CrateDecls CrateItemDecl) ...))
   ]
  )

(define-metafunction formality-decl
  ;; Given a crate item, return a tuple of:
  ;;
  ;; * The clauses that hold in all crates due to this item
  ;; * The hypotheses that hold in all crates due to this item
  ;; * The hypotheses that hold only in the crate that declared this item
  crate-item-decl-rules : CrateDecls CrateItemDecl -> (Clauses Hypotheses Hypotheses)

  [;; For an ADT declaration declared in the crate C, like the following:
   ;;
   ;;     struct Foo<T> where T: Ord { ... }
   ;;
   ;; We generate the following clause
   ;;
   ;;     (ForAll ((TyKind T))
   ;;         (WellFormedTy Ty) :-
   ;;            (Implemented (Ord T)))
   ;;
   ;; And the following hypotheses global to the crate C:
   ;;
   ;;     (ForAll ((TyKind T))
   ;;         (Hypothesized (WellFormed Ty)) => (Implemented (Ord T)))
   (crate-item-decl-rules _ (AdtId (AdtKind KindedVarIds (WhereClause ...) AdtVariants)))
   ((Clause) () Hypotheses)

   (where/error ((ParameterKind VarId) ...) KindedVarIds)
   (where/error Ty_adt (TyApply AdtId (VarId ...)))
   (where/error Clause (ForAll KindedVarIds
                               (Implies
                                ((where-clause-to-goal WhereClause) ...)
                                (WellFormed (TyKind Ty_adt)))))
   (where/error Hypotheses ((ForAll KindedVarIds
                                    (Implies
                                     ((Hypothesized (WellFormed (TyKind Ty_adt))))
                                     (where-clause-to-hypothesis WhereClause)))
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
   ;; We also generate the following hypotheses in any crate:
   ;;
   ;;     (ForAll ((TyKind Self) (LtKind 'a) (TyKind T))
   ;;         (Hypothesized (Implemented (Foo (Self T)))) => (Implemented (Ord T))
   ;;         (Hypothesized (Implemented (Foo (Self T)))) => (WellFormedTy Self)
   ;;         (Hypothesized (Implemented (Foo (Self T)))) => (WellFormedTy T))
   (crate-item-decl-rules _ (TraitId (trait KindedVarIds (WhereClause ...) TraitItems)))
   ((Clause) (Hypothesis_wc ... Hypothesis_wf ...) ())

   (where/error ((ParameterKind VarId) ...) KindedVarIds)
   (where/error TraitRef_me (TraitId (VarId ...)))
   (where/error Clause (ForAll KindedVarIds
                               (Implies
                                ((HasImpl TraitRef_me)
                                 (WellFormed (ParameterKind VarId)) ...
                                 (where-clause-to-goal WhereClause) ...
                                 )
                                (Implemented TraitRef_me))))
   (where/error (Hypothesis_wc ...) ((ForAll KindedVarIds
                                             (Implies
                                              ((Hypothesized (Implemented TraitRef_me)))
                                              (where-clause-to-hypothesis WhereClause))) ...))
   (where/error (Hypothesis_wf ...) ((ForAll KindedVarIds
                                             (Implies
                                              ((Hypothesized (Implemented TraitRef_me)))
                                              (WellFormed (ParameterKind VarId)))) ...))
   ]

  [;; For an trait impl declared in the crate C, like the following:
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
   (where/error (Goal_wc ...) (where-clauses-to-goals WhereClauses_impl))
   (where/error Clause (ForAll KindedVarIds_impl
                               (Implies
                                ((WellFormed (ParameterKind_trait Parameter_trait)) ...
                                 Goal_wc ...
                                 )
                                (HasImpl TraitRef))))
   ]
  )

(define-metafunction formality-decl
  ;; Given a crate item, return a tuple of:
  ;;
  ;; * The clauses that hold in all crates due to this item
  ;; * The hypotheses that hold in all crates due to this item
  ;; * The hypotheses that hold only in the crate that declared this item
  default-rules : () -> (Clauses Hypotheses)

  ((default-rules ())
   (((WellFormed (TyKind (scalar-ty i32)))
     (WellFormed (TyKind (scalar-ty u32)))
     (WellFormed (TyKind TyUnit))
     )
    ())
   )

  )

(define-metafunction formality-decl
  ;; Convert a set of where clauses into a set of goals that prove
  ;; those where clauses hold.
  where-clauses-to-goals : WhereClauses -> Goals

  ((where-clauses-to-goals (WhereClause ...))
   ((where-clause-to-goal WhereClause) ...)
   )
  )

(define-metafunction formality-decl
  ;; Convert a where clause `W` into a goal that proves `W` is true.
  where-clause-to-goal : WhereClause -> Goal

  ((where-clause-to-goal (Implemented TraitRef))
   (Implemented TraitRef)
   )

  ((where-clause-to-goal (ForAll KindedVarIds WhereClause))
   (ForAll KindedVarIds Goal)
   (where/error Goal (where-clause-to-goal WhereClause))
   )

  ; FIXME: Support lifetimes, projections
  )

(define-metafunction formality-decl
  ;; Convert a where clause `W` into a hypothesis that code which is
  ;; implied by `W` can assume to be true.
  where-clause-to-hypothesis : WhereClause -> Hypothesis

  ((where-clause-to-hypothesis (Implemented TraitRef))
   (Implemented TraitRef)
   )

  ((where-clause-to-hypothesis (ForAll KindedVarIds WhereClause))
   (ForAll KindedVarIds Goal)
   (where/error Goal (where-clause-to-goal WhereClause))
   )

  ; FIXME: Support lifetimes, projections
  )