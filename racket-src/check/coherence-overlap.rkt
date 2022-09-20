#lang racket
(require redex/reduction-semantics
         "../logic/instantiate.rkt"
         "../decl/decl-ok.rkt"
         "../decl/decl-to-clause.rkt"
         "../decl/grammar.rkt"
         "../decl/env.rkt"
         "../body/type-check-goal.rkt"
         "../body/well-formed-mir.rkt"
         "../body/borrow-check.rkt"
         "grammar.rkt"
         "unsafe-check.rkt"
         "prove-goal.rkt"
         )
(provide ✅-OverlapRules
         )

(define-judgment-form
  formality-check

  ;; The conditions under which an impl passes the orphan rules.

  #:mode (✅-OverlapRules I I)
  #:contract (✅-OverlapRules DeclProgram TraitImplDecl)

  [(where/error (impl _ (TraitId _) where _ _) TraitImplDecl)
   (where/error [TraitImplDecl_all ...] (all-impls-of-trait DeclProgram TraitId))
   (where #f (impl-appears-more-than-once [TraitImplDecl_all ...] TraitImplDecl))
   (✅-ImplsEqualOrNotOverlap DeclProgram TraitImplDecl TraitImplDecl_all) ...
   -------------------------------
   (✅-OverlapRules DeclProgram TraitImplDecl)]


  )

(define-judgment-form
  formality-check

  ;; The conditions under which an impl passes the orphan rules.

  #:mode (✅-ImplsEqualOrNotOverlap I I I)
  #:contract (✅-ImplsEqualOrNotOverlap DeclProgram TraitImplDecl TraitImplDecl)

  ;; Accept two impls if they are exactly equal:
  ;;
  ;; This is a bit surprising because two distinct, exactly equal impls overlap by definition.
  ;; The reason is that we are checking the 1st impl against every other impl in every crate
  ;; *including itself*. So if we see a comparison of two equal impls, we assume it is the
  ;; same impl being compared against itself for now. There is a separate check that you
  ;; cannot have two exactly equal impls (FIXME).
  [-------------------------------
   (✅-ImplsEqualOrNotOverlap DeclProgram TraitImplDecl TraitImplDecl)]

  [; we want to prove that `TraitImplDecl_1` and `TraitImplDecl_2` do not overlap
   ;
   ; Example: imagine impl 1 is...
   ;
   ; impl<T> Debug for Vec<T> where T: Debug
   ;
   ; * `KindedVarIds_1` will be `[(type T)]`
   ; * `TraitId` will be `Debug`
   ; * `Parameters_1` will be `[Vec<T>]`
   ; * `Biformulas_1` will be `T: Debug`
   (where/error (impl KindedVarIds_1 (TraitId Parameters_1) where Biformulas_1 _) TraitImplDecl_1)
   (where/error (impl KindedVarIds_2 (TraitId Parameters_2) where Biformulas_2 _) TraitImplDecl_2)

   ; get the base environment for the program
   (where/error Env_0 (env-for-decl-program DeclProgram))

   ; instantiate with inference variables
   ;
   ; for our example, we will create a inference variable `?T` in the the environment `Env_1`
   ; and get back...
   ;
   ; * `Parameters_1inf` will be `[Vec<?T>]`
   ; * `Biformulas_1` will be `?T: Debug`
   (where/error (Env_1 ([Parameter_1inf ...] [Biformula_1inf ...]) _)
                (instantiate-quantified Env_0 (∃ KindedVarIds_1 (Parameters_1 Biformulas_1))))

   ; ...same for impl 2.
   (where/error (Env_2 ([Parameter_2inf ...] [Biformula_2inf ...]) _)
                (instantiate-quantified Env_1 (∃ KindedVarIds_2 (Parameters_2 Biformulas_2))))

   ; and require that the parameters are equal
   (where/error Goal_unified (&& [(Parameter_1inf == Parameter_2inf) ...
                                  Biformula_1inf ...
                                  Biformula_2inf ...
                                  ]))

   ; check that we cannot make them equal
   (cannot-prove-goal-in-env Env_2 Goal_unified)
   -------------------------------
   (✅-ImplsEqualOrNotOverlap DeclProgram TraitImplDecl_1 TraitImplDecl_2)
   ]

  ; FIXME: add the negative impl cases etc

  )

(define-metafunction formality-check
  ;; Finds all impls of the given trait in any crate.
  all-impls-of-trait : DeclProgram TraitId -> [TraitImplDecl ...]

  [(all-impls-of-trait DeclProgram TraitId)
   [TraitImplDecl ... ...]

   ; get a list of all the crate items in any crate
   (where/error ([(crate _ [CrateItemDecl ...]) ...] _) DeclProgram)
   (where/error [CrateItemDecl_all ...] [CrateItemDecl ... ...])

   ; filter that down to just the trait impls
   (where/error [[TraitImplDecl ...] ...] [(select-trait-item CrateItemDecl_all TraitId) ...])
   ]
  )

(define-metafunction formality-check
  ;; Given a crate item, if the item is an impl of the given trait, return a 1-element list.
  ;; Else return an empty list.
  ;; Used for filtering.
  select-trait-item : CrateItemDecl TraitId -> [TraitImplDecl ...]

  [(select-trait-item TraitImplDecl TraitId)
   [TraitImplDecl]
   (where (impl _ (TraitId _) where _ _) TraitImplDecl)
   ]

  [(select-trait-item CrateItemDecl TraitId)
   []
   ]
  )

(define-metafunction formality-check
  ;; Given an impl decl and a list of existing impl decls, return true if that impl decl shows up more than once in the list.
  ;; Else return false.
  impl-appears-more-than-once : TraitImplDecls TraitImplDecl -> boolean

  [(impl-appears-more-than-once [_ ... TraitImplDecl _ ... TraitImplDecl _ ...] TraitImplDecl)
   #t
   ]
  [(impl-appears-more-than-once [_ ...] TraitImplDecl)
   #f
   ]
  )
