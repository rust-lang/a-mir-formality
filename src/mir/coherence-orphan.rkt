#lang racket
(require redex/reduction-semantics
         "grammar-extended.rkt"
         "type-check-goal.rkt"
         "well-formed-mir.rkt"
         "unsafe-check.rkt"
         "borrow-check.rkt"
         "../decl/decl-ok.rkt"
         "../decl/decl-to-clause.rkt"
         "../decl/grammar.rkt"
         "../ty/user-ty.rkt"
         "../logic/instantiate.rkt"
         "prove-goal.rkt"
         )
(provide ✅-OrphanRules
         )

(define-judgment-form
  formality-mir-extended

  ;; The conditions under which an impl passes the orphan rules.

  #:mode (✅-OrphanRules I I)
  #:contract (✅-OrphanRules DeclProgram TraitImplDecl)

  [(where/error (CrateDecls CrateId) DeclProgram)
   (where/error (impl _ TraitId _ for _ where _ _) TraitImplDecl)
   (where CrateId (crate-defining-trait-with-id CrateDecls TraitId))
   ------------------------------- "orphan--trait-is-in-current-crate"
   (✅-OrphanRules DeclProgram TraitImplDecl)]

  [(where/error (impl KindedVarIds _ _ for UserTy_self where WhereClauses _) TraitImplDecl)
   (is-local-parameter DeclProgram (user-ty UserTy_self))
   ------------------------------- "orphan--self-is-local"
   (✅-OrphanRules DeclProgram TraitImplDecl)]

  ; FIXME there are more rules

  )


(define-judgment-form
  formality-mir-extended

  ;; Defines when a parameter (type/lifetime) is considered "local" to the current crate.

  #:mode (is-local-parameter I I)
  #:contract (is-local-parameter DeclProgram Parameter)

  [------------------------------- "orphan--scalars-are-local-to-care"
   (is-local-parameter (_ core) (rigid-ty ScalarId _))]

  [------------------------------- "orphan--refs-are-local-to-core"
   (is-local-parameter (_ core) (rigid-ty (ref _) _))]

  [------------------------------- "orphan--fns-are-local-to-core"
   (is-local-parameter (_ core) (rigid-ty (fn-ptr _ _) _))]

  [(where CrateId (crate-defining-adt-with-id CrateDecls AdtId))
   ------------------------------- "orphan--adts"
   (is-local-parameter (CrateDecls CrateId) (rigid-ty AdtId _))]

  ; FIXME -- there are more rules we could add

  )