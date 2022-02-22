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
  ;;     (WellFormedTy Ty) :-
  ;;        (Implemented (Ord T))
  ;;
  ;; And the following hypotheses global to the crate C:
  ;;
  ;;     (WellFormed Ty) => (Implemented (Ord T))
  adt-decl-rules : AdtDecl -> (Clauses Hypotheses)

  [(adt-decl-rules (AdtKind AdtId KindedVarDecls (WhereClause ...) AdtVariants))
   ((Clause) Hypotheses)

   (where/error ((ParameterKind VarId) ...) KindedVarDecls)
   (where/error Ty_adt (TyApply AdtId (VarId ...)))
   (where/error Clause (ForAll KindedVarDecls
                               (Implies
                                ((where-clause-to-goal WhereClause) ...)
                                (WellFormed (TyKind Ty_adt)))))
   (where/error Hypotheses ((ForAll KindedVarDecls
                                    (Implies
                                     ((WellFormed (TyKind Ty_adt)))
                                     (where-clause-to-hypothesis WhereClause)))
                            ...))
   ]
  )

(define-metafunction formality-decl
  ;; For an ADT declaration declared in the crate C, like the following:
  ;;
  ;;     trait Foo<'a, T> where T: Ord { ... }
  ;;
  ;; We generate the following clause that proves `Foo` is implemented
  ;; for some types `(Self T)`. Note that, for `Foo` to be considered
  ;; implemented, all of its input types must be well-formed, it must have
  ;; an impl, and the where-clauses declared on the trait must be met:
  ;;
  ;;     (Implemented (Foo (Self 'a T))) :-
  ;;        (HasImpl (Foo (Self T))),
  ;;        (WellFormed (TyKind Self)),
  ;;        (WellFormed (LtKind 'a)),
  ;;        (WellFormed (TyKind T)),
  ;;        (Implemented (Ord T)).
  ;;
  ;; We also generate the following hypotheses global to the crate C:
  ;;
  ;;     (Implemented (Foo (Self T))) => (Implemented (Ord T))
  ;;     (Implemented (Foo (Self T))) => (WellFormedTy Self)
  ;;     (Implemented (Foo (Self T))) => (WellFormedTy T)
  trait-decl-rules : TraitDecl -> (Clauses Hypotheses)

  [(trait-decl-rules (trait TraitId KindedVarDecls (WhereClause ...) TraitItems))
   ((Clause) Hypotheses)

   (where/error ((ParameterKind VarId) ...) KindedVarDecls)
   (where/error TraitRef_me (TraitId (VarId ...)))
   (where/error Clause (ForAll KindedVarDecls
                               (Implies
                                ((HasImpl TraitRef_me)
                                 (WellFormed (ParameterKind VarId)) ...
                                 (where-clause-to-goal WhereClause) ...
                                 )
                                (Implemented TraitRef_me))))
   (where/error Hypotheses ((ForAll KindedVarDecls
                                    (Implies
                                     ((Implemented (TraitRef_me)))
                                     (where-clause-to-hypothesis WhereClause)))
                            ...
                            (ForAll KindedVarDecls
                                    (Implies
                                     ((Implemented (TraitRef_me)))
                                     (WellFormed (ParameterKind VarId))))
                            ...))
   ]
  )