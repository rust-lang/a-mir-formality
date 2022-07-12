#lang racket
(require redex/reduction-semantics
         "../logic/instantiate.rkt"
         "../decl/decl-ok.rkt"
         "../decl/env.rkt"
         "../decl/grammar.rkt"
         "../body/type-check-goal.rkt"
         "../body/well-formed-mir.rkt"
         "../body/borrow-check.rkt"
         "grammar.rkt"
         "unsafe-check.rkt"
         "coherence-orphan.rkt"
         "prove-goal.rkt"
         )
(provide ✅-Program
         )

(define-judgment-form
  formality-check

  ;; Check an entire program for correctness.
  ;;
  ;; Corresponds to the rust compiler "up to" the point where LLVM is generated.

  #:mode (✅-Program I)
  #:contract (✅-Program DeclProgram)

  [(where/error (CrateDecls CrateId) DeclProgram)
   (where/error CrateDecl_current (crate-decl-with-id CrateDecls CrateId))
   (where/error (crate _ {CrateItemDecl ...}) CrateDecl_current)
   (✅-CrateItemDecl DeclProgram CrateItemDecl) ...
   ----------------------------------------
   (✅-Program DeclProgram)
   ]
  )

(define-judgment-form
  formality-check

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
   (✅-OrphanRules DeclProgram TraitImplDecl)
   ; FIXME -- need to check the impl items
   ----------------------------------------
   (✅-CrateItemDecl DeclProgram TraitImplDecl)
   ]

  [; constants
   (prove-crate-item-ok DeclProgram ConstDecl)
   (where/error (const _ KindedVarIds where Biformulas : Ty = FnBody) ConstDecl)
   (✅-FnBody DeclProgram (∀ KindedVarIds (() -> Ty where Biformulas FnBody)))
   ----------------------------------------
   (✅-CrateItemDecl DeclProgram ConstDecl)
   ]

  [; statics
   (prove-crate-item-ok DeclProgram StaticDecl)
   (where/error (static _ KindedVarIds where Biformulas : Ty = FnBody) StaticDecl)
   (✅-FnBody DeclProgram (∀ KindedVarIds (() -> Ty where Biformulas FnBody)))
   ----------------------------------------
   (✅-CrateItemDecl DeclProgram StaticDecl)
   ]

  [; function items
   (prove-crate-item-ok DeclProgram FnDecl)
   (where/error (fn FnId KindedVarIds Tys -> Ty where Biformulas FnBody) FnDecl)
   (✅-FnBody DeclProgram (∀ KindedVarIds (Tys -> Ty where Biformulas FnBody)))
   ----------------------------------------
   (✅-CrateItemDecl DeclProgram FnDecl)
   ]
  )

(define-judgment-form
  formality-check

  ;; The "all-check" is the master checking rule that checks all correctness criteria
  ;; for a fn body.
  ;;
  ;;
  ;; and our job would be to check (among other things) that `(data1, data2)` is a value of type
  ;; `(T, T)`, as was declared in the function signature (but also that we can borrow check
  ;; the fn body, meaning that each value is moved at most once, etc).
  #:mode (✅-FnBody I I)
  #:contract (✅-FnBody DeclProgram MirBodySig)

  [;; base environment 0: just the program declarations
   (where/error Env_0 (env-for-decl-program DeclProgram))

   ;; environment 1: instantiate the generic arguments as universal placeholders and tag
   (where (Env_1 (Tys -> Ty where Biformulas (∃ KindedVarIds_1 LocalsAndBlocks_1)) VarIds_∀)
          (instantiate-quantified Env_0 MirBodySig))

   ;; environment 2: instantiate the existential inference variables within the mir body (lifetimes, typically)
   (where/error (Env_2 LocalsAndBlocks_2 VarIds_∃)
                (instantiate-quantified Env_1 (∃ KindedVarIds_1 LocalsAndBlocks_1)))

   ;; construct the typing environment
   (where/error (CrateDecls _) DeclProgram)
   (where/error Γ (CrateDecls VarIds_∀ (Tys -> Ty where Biformulas) LocalsAndBlocks_2))

   ;; run the checks
   (well-formed/Γ Γ)
   (unsafe-check Γ)
   (type-check-goal/Γ Γ GoalAtLocations)
   (borrow-check Γ GoalAtLocations)
   ---------------------------------------- "mir-fn-body"
   (✅-FnBody DeclProgram MirBodySig)
   ]

  [---------------------------------------- "trusted-fn-body"
   (✅-FnBody DeclProgram (∀ _ (_ -> _ where _ {trusted-fn-body})))
   ]
  )
