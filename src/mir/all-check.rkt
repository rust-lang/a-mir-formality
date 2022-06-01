#lang racket
(require redex/reduction-semantics
         "grammar-extended.rkt"
         "type-check-goal.rkt"
         "well-formed-mir.rkt"
         "unsafe-check.rkt"
         "borrow-check.rkt"
         "../decl/decl-ok.rkt"
         "prove-goal.rkt"
         )
(provide ✅-Program
         )

(define-judgment-form
  formality-mir-extended

  ;; Check an entire program for correctness.
  ;;
  ;; Corresponds to the rust compiler "up to" the point where LLVM is generated.

  #:mode (✅-Program I)
  #:contract (✅-Program DeclProgram)

  [(where/error (CrateDecls CrateId) DeclProgram)
   (where/error CrateDecl_current (crate-decl-with-id CrateDecls CrateId))
   (where/error (_ (crate (CrateItemDecl ...))) CrateDecl_current)
   (✅-CrateItemDecl DeclProgram CrateItemDecl) ...
   ----------------------------------------
   (✅-Program DeclProgram)
   ]
  )

(define-judgment-form
  formality-mir-extended

  ;; Check a particular item from within the current crate for correctness.


  #:mode (✅-CrateItemDecl I I)
  #:contract (✅-CrateItemDecl DeclProgram CrateItemDecl)

  [; #![feature(...)]
   ----------------------------------------
   (✅-CrateItemDecl DeclProgram (feature expanded-implied-bounds))
   ]

  [; structs/enum/unions
   (prove-crate-item-ok DeclProgram AdtDecl)
   ----------------------------------------
   (✅-CrateItemDecl DeclProgram AdtDecl)
   ]

  [; trait declarations
   (prove-crate-item-ok DeclProgram TraitDecl)
   ; FIXME -- need to check the trait items
   ----------------------------------------
   (✅-CrateItemDecl DeclProgram TraitDecl)
   ]

  [; impl declarations
   (prove-crate-item-ok DeclProgram TraitImplDecl)
   ; FIXME -- need to check the impl items
   ----------------------------------------
   (✅-CrateItemDecl DeclProgram TraitImplDecl)
   ]

  [; constants
   (prove-crate-item-ok DeclProgram ConstDecl)
   (where/error (_ (const KindedVarIds WhereClauses Ty FnBody)) ConstDecl)
   (✅-FnBody DeclProgram (∀ KindedVarIds (() -> Ty where WhereClauses FnBody)))
   ----------------------------------------
   (✅-CrateItemDecl DeclProgram ConstDecl)
   ]

  [; statics
   (prove-crate-item-ok DeclProgram StaticDecl)
   (where/error (_ (static KindedVarIds WhereClauses Ty FnBody)) StaticDecl)
   (✅-FnBody DeclProgram (∀ KindedVarIds (() -> Ty where WhereClauses FnBody)))
   ----------------------------------------
   (✅-CrateItemDecl DeclProgram StaticDecl)
   ]

  [; function items
   (prove-crate-item-ok DeclProgram FnDecl)
   (where/error (FnId (fn-decl KindedVarIds Tys Ty WhereClauses FnBody)) FnDecl)
   (✅-FnBody DeclProgram (∀ KindedVarIds (Tys -> Ty where WhereClauses FnBody)))
   ----------------------------------------
   (✅-CrateItemDecl DeclProgram FnDecl)
   ]
  )

(define-judgment-form
  formality-mir-extended

  ;; The "all-check" is the master checking rule that checks all correctness criteria
  ;; for a fn body.
  ;;
  ;; Example, given this Rust function:
  ;;
  ;; ```rust
  ;; fn foo<T>(data1: T, data2: T) -> (T, T)
  ;; where
  ;;     T: Debug
  ;; {
  ;;     (data1, data2)
  ;; }
  ;; ```
  ;;
  ;; this judgment would eventually be invoked like:
  ;;
  ;; ```
  ;; ✅-FnBody some-program (∀ ((type T)) ((T T) -> (ty-tuple (T T)) where ((T: Debug())) mir))
  ;;                            -------     - -     ----------------        ------------
  ;;                     type-parameters  arguments   return type           where-clauses
  ;; ```
  ;;
  ;; and our job would be to check (among other things) that `(data1, data2)` is a value of type
  ;; `(T, T)`, as was declared in the function signature (but also that we can borrow check
  ;; the fn body, meaning that each value is moved at most once, etc).
  #:mode (✅-FnBody I I)
  #:contract (✅-FnBody DeclProgram (∀ KindedVarIds (Tys -> Ty where WhereClauses MirBody)))

  [(where/error Γ (DeclProgram KindedVarIds (Tys -> Ty where WhereClauses) MirBody))
   (well-formed/Γ Γ)
   (unsafe-check Γ)
   (type-check-goal/Γ Γ GoalAtLocations)
   (borrow-check Γ GoalAtLocations)
   ----------------------------------------
   (✅-FnBody DeclProgram (∀ KindedVarIds (Tys -> Ty where WhereClauses MirBody)))
   ]
  )
