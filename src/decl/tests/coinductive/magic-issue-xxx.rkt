#lang racket
(require redex/reduction-semantics
         "../../decl-to-clause.rkt"
         "../../grammar.rkt"
         "../../../ty/grammar.rkt"
         "../../../ty/solve.rkt"
         "../../../util.rkt")

(module+ test
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

   (; We cannot prove that `i32: Magic` because `i32: Copy` does not hold
    traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (Implemented (Magic ((scalar-ty i32))))
                             EnvSubstitution)
                            EnvSubstitution)
            (term ())))


   (; Also cannot prove that `i32: Copy`
    traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (Implemented (Copy ((scalar-ty i32))))
                             EnvSubstitution)
                            EnvSubstitution)
            (term ())))

   )

  (redex-let*
   formality-decl

   ((; trait Magic: Copy { }
     TraitDecl_Magic (term (Magic (trait ((TyKind Self)) ((Implemented (Copy (Self)))) ()))))

    (; trait Copy: Magic { }
     TraitDecl_Copy (term (Copy (trait ((TyKind Self)) ((Implemented (Magic (Self)))) ()))))

    (; impl<T> Magic for T where T: Magic { }
     TraitImplDecl_Magic (term (impl ((TyKind T)) (Magic (T)) ((Implemented (Magic (T)))) ())))

    (; crate TheCrate { ... }
     CrateDecl (term (TheCrate (crate (TraitDecl_Magic TraitDecl_Copy TraitImplDecl_Magic)))))

    (Env (term (env-with-crate-decl EmptyEnv CrateDecl)))
    )

   (; Cannot prove that `i32: Magic`
    traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (Implemented (Magic ((scalar-ty i32))))
                             EnvSubstitution)
                            EnvSubstitution)
            (term ())))


   (; And cannot prove that `i32: Copy`
    traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (Implemented (Copy ((scalar-ty i32))))
                             EnvSubstitution)
                            EnvSubstitution)
            (term ())))

   )

  (redex-let*
   formality-decl

   ((; trait Magic: Copy { }
     TraitDecl_Magic (term (Magic (trait ((TyKind Self)) ((Implemented (Copy (Self)))) ()))))

    (; trait Copy { }
     TraitDecl_Copy (term (Copy (trait ((TyKind Self)) () ()))))

    (; impl<T> Magic for T where T: Magic { }
     TraitImplDecl_Magic (term (impl ((TyKind T)) (Magic (T)) ((Implemented (Magic (T)))) ())))

    (; impl Copy for i32 { }
     TraitImplDecl_Copy (term (impl () (Copy ((scalar-ty i32))) () ())))

    (; crate TheCrate { ... }
     CrateDecl (term (TheCrate (crate (TraitDecl_Magic TraitDecl_Copy TraitImplDecl_Magic)))))

    (Env (term (env-with-crate-decl EmptyEnv CrateDecl)))
    )

   (; We can prove that `i32: Magic` because `i32: Copy` does holds
    traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (Implemented (Magic ((scalar-ty i32))))
                             EnvSubstitution)
                            EnvSubstitution)
            (term ())))

   (; But not `u32: Magic` because `u32: Copy` does not hold
    traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (Implemented (Magic ((scalar-ty u32))))
                             EnvSubstitution)
                            EnvSubstitution)
            (term ())))

   )
  )