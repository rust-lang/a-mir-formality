#lang racket
(require redex/reduction-semantics
         "grammar.rkt")
(provide )

(define-metafunction formality-decl
  where-clauses-to-goals : WhereClauses -> Goals

  ((where-clauses-to-goals (WhereClause ...))
   ((where-clause-to-goal WhereClause) ...)
   )
  )

(define-metafunction formality-decl
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

(define-metafunction formality-decl
  ;; For an ADT declaration declared in the crate C, like the following:
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
  ;;         (WellFormed Ty) => (Implemented (Ord T)))
  adt-decl-rules : CrateDecl AdtDecl -> (Clauses Hypotheses)

  [(adt-decl-rules _ (AdtKind AdtId KindedVarIds (WhereClause ...) AdtVariants))
   ((Clause) Hypotheses)

   (where/error ((ParameterKind VarId) ...) KindedVarIds)
   (where/error Ty_adt (TyApply AdtId (VarId ...)))
   (where/error Clause (ForAll KindedVarIds
                               (Implies
                                ((where-clause-to-goal WhereClause) ...)
                                (WellFormed (TyKind Ty_adt)))))
   (where/error Hypotheses ((ForAll KindedVarIds
                                    (Implies
                                     ((WellFormed (TyKind Ty_adt)))
                                     (where-clause-to-hypothesis WhereClause)))
                            ...))
   ]
  )

(define-metafunction formality-decl
  ;; For a trait declaration declared in the crate C, like the following:
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
  ;; We also generate the following hypotheses global to the crate C:
  ;;
  ;;     (ForAll ((TyKind Self) (LtKind 'a) (TyKind T))
  ;;         (Implemented (Foo (Self T))) => (Implemented (Ord T))
  ;;         (Implemented (Foo (Self T))) => (WellFormedTy Self)
  ;;         (Implemented (Foo (Self T))) => (WellFormedTy T))
  trait-decl-rules : CrateDecl TraitDecl -> (Clauses Hypotheses)

  [(trait-decl-rules _ (trait TraitId KindedVarIds (WhereClause ...) TraitItems))
   ((Clause) Hypotheses)

   (where/error ((ParameterKind VarId) ...) KindedVarIds)
   (where/error TraitRef_me (TraitId (VarId ...)))
   (where/error Clause (ForAll KindedVarIds
                               (Implies
                                ((HasImpl TraitRef_me)
                                 (WellFormed (ParameterKind VarId)) ...
                                 (where-clause-to-goal WhereClause) ...
                                 )
                                (Implemented TraitRef_me))))
   (where/error Hypotheses ((ForAll KindedVarIds
                                    (Implies
                                     ((Implemented (TraitRef_me)))
                                     (where-clause-to-hypothesis WhereClause)))
                            ...
                            (ForAll KindedVarIds
                                    (Implies
                                     ((Implemented (TraitRef_me)))
                                     (WellFormed (ParameterKind VarId))))
                            ...))
   ]
  )


(define-metafunction formality-decl
  ;; For an trait impl declared in the crate C, like the following:
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
  impl-decl-rules : CrateDecl ImplDecl -> (Clauses Hypotheses)

  [(impl-decl-rules CrateDecl (impl KindedVarIds_impl TraitRef WhereClauses_impl ImplItems))
   ((Clause) Hypotheses)

   (where/error (TraitId (Parameter_trait ...)) TraitRef)
   (where/error (trait TraitId KindedVarIds_trait _ _)
                (trait-decl-with-id-from-crate CrateDecl TraitId))
   (where/error ((ParameterKind_trait _) ...) KindedVarIds_trait)
   (where/error Clause (ForAll KindedVarIds_impl
                               (Implies
                                ((WellFormed (ParameterKind_trait Parameter_trait)) ...
                                 (where-clauses-to-goals WhereClauses_impl)
                                 )
                                (HasImpl TraitRef))))
   ]
  )