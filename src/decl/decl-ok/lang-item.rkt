#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../where-clauses.rkt"
         "../feature-gate.rkt"
         "../../logic/env.rkt"
         "../../ty/user-ty.rkt"
         )
(provide lang-item-ok-goals
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
   (lang-item-ok-goals CrateDecls (impl KindedVarIds_impl rust:Drop () for UserTy_impl where (WhereClause_impl ...) _))
   ((∀ KindedVarIds_adt
       (implies (where-clauses->hypotheses CrateDecls WhereClauses_adt)
                (∃ KindedVarIds_impl
                   (&& ((Ty_impl == Ty_adt)
                        (where-clause->goal CrateDecls WhereClause_impl) ...
                        ))
                   ))))

   (where (rigid-ty AdtId Parameters) (user-ty UserTy_impl))
   (where (AdtKind AdtId KindedVarIds_adt where WhereClauses_adt _) (adt-with-id CrateDecls AdtId))
   (where/error ((ParameterKind_adt VarId_adt) ...) KindedVarIds_adt)
   (where/error Ty_adt (rigid-ty AdtId (VarId_adt ...)))
   ]

  [; Impl of the Drop trait for something that is not an ADT -- always an error.
   (lang-item-ok-goals CrateDecls (impl KindedVarIds_impl rust:Drop () for _ where WhereClauses_impl _))
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
   (lang-item-ok-goals CrateDecls (impl KindedVarIds_impl rust:Copy () for UserTy_impl where (WhereClause_impl ...) ()))
   ((∀ KindedVarIds_impl
       (implies ((where-clause->hypothesis CrateDecls WhereClause_impl) ...)
                (∃ KindedVarIds_adt
                   (&& ((Ty_impl == Ty_adt)
                        (is-implemented (rust:Copy (Ty_field))) ... ...
                        )
                       )))))

   (where (rigid-ty AdtId Parameters) (user-ty Ty_impl))
   (where (AdtKind AdtId KindedVarIds_adt where _ AdtVariants) (adt-with-id CrateDecls AdtId))
   (where/error ((ParameterKind_adt VarId_adt) ...) KindedVarIds_adt)
   (where/error Ty_adt (rigid-ty AdtId (VarId_adt ...)))
   (where/error ((VariantId ((FieldId Ty_field) ...)) ...) AdtVariants)
   ]

  [; Base case: this is not a special item, or it has no special rules: return empty list.
   (lang-item-ok-goals CrateDecls _) ()]
  )