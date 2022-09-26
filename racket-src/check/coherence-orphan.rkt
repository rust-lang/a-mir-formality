#lang racket
(require redex/reduction-semantics
         "../logic/instantiate.rkt"
         "../decl/orphan-rules.rkt"
         "../decl/env.rkt"
         "grammar.rkt"
         )
(provide ✅-OrphanRules
         )

(define-judgment-form
  formality-check

  ;; The conditions under which an impl passes the orphan rules.

  #:mode (✅-OrphanRules I I)
  #:contract (✅-OrphanRules DeclProgram TraitImplDecl)


  [(where/error (impl KindedVarIds TraitRef where Biformulas _) TraitImplDecl)
   (where/error Env_0 (env-for-decl-program DeclProgram))
   (where/error (Env_1 TraitRef_1 _) (instantiate-quantified Env_0 (∀ KindedVarIds TraitRef)))
   (where #t (orphan-check DeclProgram Env_1 TraitRef_1))
   ------------------------------- "orphan"
   (✅-OrphanRules DeclProgram TraitImplDecl)]

  )
