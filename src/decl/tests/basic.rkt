#lang racket
(require redex/reduction-semantics
         "../decl-to-clause.rkt"
         "../grammar.rkt"
         "../../ty/grammar.rkt"
         "../../ty/solve.rkt")

(module+ test
  ;; Program:
  ;;
  ;; trait Debug { }
  ;; impl Debug for i32 { }
  (current-traced-metafunctions '())
  (redex-let*
   formality-decl

   ((TraitDecl (term (Debug (trait ((TyKind Self)) () ()))))
    (TraitImplDecl (term (impl () (Debug (i32)) () ())))
    (CrateDecl (term (TheCrate (crate (TraitDecl TraitImplDecl)))))
    (Env (term (env-with-crate-decl EmptyEnv CrateDecl)))
    )

   (test-equal
    (judgment-holds (prove Env
                           (Implies ((WellFormed (TyKind i32)))
                                    (Implemented (Debug (i32))))
                           EnvSubstitution)
                    EnvSubstitution)
    (term ((Env ()))))
   )
  (current-traced-metafunctions '())
  )
