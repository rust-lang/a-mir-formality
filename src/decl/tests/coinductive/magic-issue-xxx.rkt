#lang racket
(require redex/reduction-semantics
         "../../decl-to-clause.rkt"
         "../../grammar.rkt"
         "../../../ty/grammar.rkt"
         "../../../ty/solve.rkt"
         "../../../util.rkt")

(module+ test
  ;; Program:
  ;;
  ;; trait Eq: PartialEq { }
  ;; impl Debug for i32 { }
  (redex-let*
   formality-decl

   ((; trait Magic: Copy { }
     TraitDecl_Magic (term (Magic (trait ((TyKind Self)) ((Implemented (Copy (Self)))) ()))))

    (; trait Copy { }
     TraitDecl_Copy (term (Copy (trait ((TyKind Self)) () ()))))

    (; impl<T> Magic for T where T: Magic { }
     TraitImplDecl_Magic (term (impl ((TyKind T)) (Magic (T)) ((Implemented (Magic (T)))) ())))

    (; crate TheCrate { ... }
     CrateDecl (term (TheCrate (crate (TraitDecl_Magic TraitDecl_Copy TraitImplDecl_Magic)))))

    (Env (term (env-with-crate-decl EmptyEnv CrateDecl)))
    )

   (traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (Implemented (Magic ((scalar-ty i32))))
                             EnvSubstitution)
                            EnvSubstitution)
            (term ())))

   (traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (Implemented (Copy ((scalar-ty i32))))
                             EnvSubstitution)
                            EnvSubstitution)
            (term ())))

   )
  )