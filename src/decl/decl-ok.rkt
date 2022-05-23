#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../ty/grammar.rkt"
         "../ty/where-clauses.rkt"
         "decl-from-crate.rkt"
         "../logic/env.rkt"
         "decl-wf-where-clause.rkt"
         )
(provide crate-item-ok-goal crate-ok-goal)

(define-metafunction formality-decl
  ;; Given a set of crates and the decl for the current crate,
  ;; generate the goal that proves all declarations in the current crate are
  ;; "ok". Other crates are assumed to be "ok".
  crate-ok-goal : CrateDecls_cs CrateDecl_c -> Goal

  #:pre (in? CrateDecl_c CrateDecls_cs)

  [(crate-ok-goal CrateDecls (CrateId (crate (CrateItemDecl ...))))
   (&& (Goal_regular ... Goal_lang-item ... ...))

   (where/error (Goal_regular ...) ((crate-item-ok-goal CrateDecls CrateItemDecl) ...))
   (where/error ((Goal_lang-item ...) ...) ((lang-item-ok-goals CrateDecls CrateItemDecl) ...))
   ]
  )

(define-metafunction formality-decl
  ;; Given a crate item, return a Goal that, if proven, means that the
  ;; declaration is "ok" (well-formed).
  crate-item-ok-goal : CrateDecls CrateItemDecl -> Goal

  [;; For an ADT declaration declared in the crate C, like the following:
   ;;
   ;;     struct Foo<T> where T: Ord { ... f: Vec<T> ... }
   ;;
   ;; We generate the following goal, which specifies that -- assuming the generics
   ;; are well formed and the where-clauses hold -- the field types are well-formed:
   ;;
   ;;     (∀ ((type T))
   ;;         (implies ((well-formed (type T))
   ;;                   (is-implemented (Ord T)))
   ;;           (well-formed (type Vec<T>)) ...))
   (crate-item-ok-goal CrateDecls (AdtId (AdtKind KindedVarIds (WhereClause ...) AdtVariants)))
   Goal_wf

   (where/error (KindedVarId ...) KindedVarIds)
   (where/error ((VariantId ((FieldId Ty) ...)) ...) AdtVariants)
   (where/error Goal_wf (∀ KindedVarIds
                           (implies
                            ((well-formed KindedVarId) ... (where-clause->hypothesis WhereClause) ...)
                            (&& ((well-formed (type Ty))
                                 ... ...
                                 (well-formed-where-clause-goal CrateDecls WhereClause)
                                 ...))
                            )))
   ]

  [;; For a fn declaration declared in the crate C, like the following:
   ;;
   ;;     fn foo<'a, T>(t: &'a T) -> u32 where T: Ord { ... }
   ;;
   ;; We generate the following goal, which specifies that -- assuming the generics
   ;; are well formed and the where-clauses hold -- the field types are well-formed:
   ;;
   ;;     (∀ ((lifetime A) (type T))
   ;;         (implies ((well-formed (lifetime A))
   ;;                   (well-formed (type T))
   ;;                   (well-formed (type (rigid-ty (ref ()) (A (rigid-ty T ()))))))
   ;;           (well-formed-where-clause-goal (T : Trait_Ord ()))))
   ;;
   ;; FIXME: Actually implement that, along with for the other items
   (crate-item-ok-goal _ (FnId (fn-decl KindedVarIds Tys_arg Ty_ret (WhereClause ...))))
   (&& ((well-formed-where-clause-goal WhereClause) ...))
   ]

  [;; For a trait declaration declared in the crate C, like the following:
   ;;
   ;;     trait Foo<'a, T> where T: Ord { ... }
   ;;
   ;; we require that all the trait-item WF goals are met.
   (crate-item-ok-goal CrateDecls (TraitId (trait KindedVarIds (WhereClause ...) (TraitItem ...))))
   (∀ KindedVarIds
      (implies ((well-formed KindedVarId) ... (where-clause->hypothesis WhereClause) ...)
               (&& (Goal_trait-item ...
                    (well-formed-where-clause-goal CrateDecls WhereClause) ...))))

   (where/error (Goal_trait-item ...) ((trait-item-ok-goal TraitItem) ...))
   (where/error (KindedVarId ...) KindedVarIds)
   ]

  [;; For a trait impl declared in the crate C, like the following:
   ;;
   ;;     impl<'a, T> Foo<'a, T> for u32 { }
   ;;
   ;; we require that the trait is implemented, given that all generics are WF,
   ;; all inputs are WF, and where-clauses are satisfied.
   (crate-item-ok-goal CrateDecls (impl KindedVarIds_impl (TraitId (Parameter_trait ...)) WhereClauses_impl ImplItems))
   (∀ KindedVarIds_impl
      (implies
       (; assuming all generic parameters are WF...
        (well-formed KindedVarId_impl) ...
        ; ...all inputs are WF...
        (well-formed (ParameterKind_trait Parameter_trait)) ...
        ; ...where-clauses are satisfied...
        (where-clause->hypothesis WhereClause_impl) ...)
       ( && ((; ... then the trait must be implemented
              is-implemented (TraitId (Parameter_trait ...)))
             (well-formed-where-clause-goal CrateDecls WhereClause_impl) ...))))

   (where/error (TraitId (trait ((ParameterKind_trait _) ...) _ _)) (trait-decl-with-id CrateDecls TraitId))
   (where/error (KindedVarId_impl ...) KindedVarIds_impl)
   (where/error (WhereClause_impl ...) WhereClauses_impl)
   ]

  [;; For a constant declared in the crate C, like the following:
   ;;
   ;;     const NAMED<T>: Foo<T> where T: Trait;
   ;;
   ;; we require that the type is well formed assuming the where clauses are satisfied.
   (crate-item-ok-goal CrateDecls (ConstId (const KindedVarIds WhereClauses Ty)))
   (∀ KindedVarIds
      (implies
       (; assuming all generic parameters are WF...
        (well-formed KindedVarId) ...
        ; ...where-clauses are satisfied...
        (where-clause->hypothesis WhereClause) ...)
       (&& ((; ... then the trait must be implemented
             well-formed (type Ty))
            (well-formed-where-clause-goal CrateDecls WhereClause) ...))))

   (where/error (KindedVarId ...) KindedVarIds)
   (where/error (WhereClause ...) WhereClauses)
   ]

  [;; Features are always ok.
   (crate-item-ok-goal CrateDecls FeatureDecl)
   (&& ())]
  )

(define-metafunction formality-decl
  trait-item-ok-goal : TraitItemDecl -> Goal
  )

(define-metafunction formality-decl
  ;; Given a crate item that may be a lang item (i.e., some well-known trait like `Drop`),
  ;; returns extra goals that must be met.
  ;;
  ;; If this is not a lang item, or this lang item
  ;; has no extra goals associated with it beyond the basic Rust type system rules,
  ;; returns the empty list.
  lang-item-ok-goals : CrateDecls CrateItemDecl -> (Goal ...)

  [; Impl of the Drop trait for an ADT type:
   ;
   ; When you implement the Drop trait for some struct `Foo<T>`, you must cover *all* possible
   ; instances of `Foo<T>`. e.g., you cannot have `impl Drop for Foo<i32>` nor can you have
   ; `impl<U> Drop for Foo<U> where U: Ord`, unless `T: Ord` was a where clause declared on
   ; `Foo`.
   ;
   ; ```
   ; struct Foo<T> where T: Debug { }
   ; impl<U> Drop for Foo<U> where U: Debug + Eq { }
   ; ```
   ;
   ; would generate a goal like
   ;
   ; ∀T. (is-implemented (T: Debug)) =>
   ;     ∃U. ((Foo<U> = Foo<T>)
   ;          (is-implemented (U: Debug))
   ;          (is-implemented (U: Eq)))
   ;
   ; when this goal is given to the solver, it would be rejected because `T: Eq` is not provable
   ; (we only know that `T: Debug`).
   (lang-item-ok-goals CrateDecls (impl KindedVarIds_impl (rust:Drop (Ty_impl)) (WhereClause_impl ...) _))
   ((∀ KindedVarIds_adt
       (implies (where-clauses->hypotheses WhereClauses_adt)
                (∃ KindedVarIds_impl
                   (&& ((Ty_impl == Ty_adt)
                        (where-clause->goal WhereClause_impl) ...
                        ))
                   ))))

   (where (rigid-ty AdtId Parameters) Ty_impl)
   (where (AdtKind KindedVarIds_adt WhereClauses_adt _) (item-with-id CrateDecls AdtId))
   (where/error ((ParameterKind_adt VarId_adt) ...) KindedVarIds_adt)
   (where/error Ty_adt (rigid-ty AdtId (VarId_adt ...)))
   ]

  [; Impl of the Drop trait for something that is not an ADT -- always an error.
   (lang-item-ok-goals CrateDecls (impl KindedVarIds_impl (rust:Drop (_ ...)) WhereClauses_impl _))
   ((|| ())) ; unprovable goal
   ]

  [; Impl of the Copy trait for an ADT type:
   ;
   ; When you implement the Copy trait for some struct `Foo<T>`, all the fields in that struct
   ; (or enum, union, etc) must also be `Copy`.
   ;
   ; So for this example...
   ;
   ; ```
   ; struct Foo<T> {
   ;    field: Vec<T>
   ; }
   ; impl<U> Copy for Foo<U> where U: Debug { }
   ; ```
   ;
   ; would generate a goal like
   ;
   ; ∀U. (is-implemented (U: Debug)) =>
   ;     ∃T. ((Foo<U> = Foo<T>) ∧
   ;          (is-implemented (Vec<T>: Copy)))
   ;
   ; of course, in this case, it is not provable because `Vec<T>: Copy` is not true for any `T`.
   (lang-item-ok-goals CrateDecls (impl KindedVarIds_impl (rust:Copy (Ty_impl)) (WhereClause_impl ...) ()))
   ((∀ KindedVarIds_impl
       (implies ((where-clause->hypothesis WhereClause_impl) ...)
                (∃ KindedVarIds_adt
                   (&& ((Ty_impl == Ty_adt)
                        (is-implemented (rust:Copy (Ty_field))) ... ...
                        )
                       )))))

   (where (rigid-ty AdtId Parameters) Ty_impl)
   (where (AdtKind KindedVarIds_adt _ AdtVariants) (item-with-id CrateDecls AdtId))
   (where/error ((ParameterKind_adt VarId_adt) ...) KindedVarIds_adt)
   (where/error Ty_adt (rigid-ty AdtId (VarId_adt ...)))
   (where/error ((VariantId ((FieldId Ty_field) ...)) ...) AdtVariants)
   ]

  [; Base case: this is not a special item, or it has no special rules: return empty list.
   (lang-item-ok-goals CrateDecls _) ()]
  )
