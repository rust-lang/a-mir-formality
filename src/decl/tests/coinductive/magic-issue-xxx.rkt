#lang racket
(require redex/reduction-semantics
         "../../decl-to-clause.rkt"
         "../../decl-ok.rkt"
         "../../grammar.rkt"
         "../../../ty/grammar.rkt"
         "../../../ty/cosld-solve.rkt"
         "../../../util.rkt")

(module+ test
  (; Magic trait, implemented in terms of itself, that extends Copy
   redex-let*
   formality-decl

   ((; struct Foo { counter: i32 }
     AdtDecl_Foo (term (Foo (struct () () ((struct-variant ((counter (scalar-ty i32)))))))))

    (; reference to `Foo`
     Ty_Foo (term (TyApply Foo ())))

    (; trait Magic: Copy { }
     TraitDecl_Magic (term (Magic (trait ((TyKind Self)) ((Implemented (Copy (Self)))) ()))))

    (; trait Copy { }
     TraitDecl_Copy (term (Copy (trait ((TyKind Self)) () ()))))

    (; impl<T> Magic for T where T: Magic { }
     TraitImplDecl_Magic (term (impl ((TyKind T)) (Magic (T)) ((Implemented (Magic (T)))) ())))

    (; crate TheCrate { ... }
     CrateDecl (term (TheCrate (crate (TraitDecl_Magic TraitDecl_Copy TraitImplDecl_Magic)))))

    (Env (term (env-for-crate-decl CrateDecl)))
    )

   (; All decls in crate are considered 'ok'. In particular, the impl is considered 'ok',
    ; since its where clauses allow it to locally prove that `Self: Copy`.
    traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (crate-ok-goal (CrateDecl) CrateDecl)
                             EnvSubstitution)
                            EnvSubstitution)
            (term ((Env ())))))

   (; ...but when we try to use it, we cannot prove that `i32: Magic`
    ; because `i32: Copy` does not hold...
    traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (Implemented (Magic (Ty_Foo)))
                             EnvSubstitution)
                            EnvSubstitution)
            (term ())))


   (; ...also cannot prove that `i32: Copy`, of course.
    traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (Implemented (Copy (Ty_Foo)))
                             EnvSubstitution)
                            EnvSubstitution)
            (term ())))

   )

  (; Mutual recursion between Magic and Copy, with Magic implemented in terms of itself,
   ; but no impl of Copy
   redex-let*
   formality-decl

   ((; struct Foo { counter: i32 }
     AdtDecl_Foo (term (Foo (struct () () ((struct-variant ((counter (scalar-ty i32)))))))))

    (; reference to `Foo`
     Ty_Foo (term (TyApply Foo ())))

    (; trait Magic: Copy { }
     TraitDecl_Magic (term (Magic (trait ((TyKind Self)) ((Implemented (Copy (Self)))) ()))))

    (; trait Copy: Magic { }
     TraitDecl_Copy (term (Copy (trait ((TyKind Self)) ((Implemented (Magic (Self)))) ()))))

    (; impl<T> Magic for T where T: Magic { }
     TraitImplDecl_Magic (term (impl ((TyKind T)) (Magic (T)) ((Implemented (Magic (T)))) ())))

    (; crate TheCrate { ... }
     CrateDecl (term (TheCrate (crate (TraitDecl_Magic TraitDecl_Copy TraitImplDecl_Magic)))))

    (Env (term (env-for-crate-decl CrateDecl)))
    )

   (; All decls in crate are considered 'ok'.
    traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (crate-ok-goal (CrateDecl) CrateDecl)
                             EnvSubstitution)
                            EnvSubstitution)
            (term ((Env ())))))

   (; Cannot prove that `i32: Magic`
    traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (Implemented (Magic (Ty_Foo)))
                             EnvSubstitution)
                            EnvSubstitution)
            (term ())))


   (; And cannot prove that `i32: Copy`
    traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (Implemented (Copy (Ty_Foo)))
                             EnvSubstitution)
                            EnvSubstitution)
            (term ())))

   )

  (; Mutual recursion between Magic and Copy, with Magic implemented in terms of itself
   ; *and* Copy implemented
   redex-let*
   formality-decl

   ((; struct Foo { counter: i32 }
     AdtDecl_Foo (term (Foo (struct () () ((struct-variant ((counter (scalar-ty i32)))))))))

    (; reference to `Foo`
     Ty_Foo (term (TyApply Foo ())))

    (; struct Bar { counter: i32 }
     AdtDecl_Foo (term (Foo (struct () () ((struct-variant ((counter (scalar-ty i32)))))))))

    (; reference to `Bar`
     Ty_Bar (term (TyApply Bar ())))

    (; trait Magic: Copy { }
     TraitDecl_Magic (term (Magic (trait ((TyKind Self)) ((Implemented (Copy (Self)))) ()))))

    (; trait Copy { }
     TraitDecl_Copy (term (Copy (trait ((TyKind Self)) () ()))))

    (; impl<T> Magic for T where T: Magic { }
     TraitImplDecl_Magic (term (impl ((TyKind T)) (Magic (T)) ((Implemented (Magic (T)))) ())))

    (; impl Copy for Foo { }
     TraitImplDecl_Copy (term (impl () (Copy (Ty_Foo)) () ())))

    (; crate TheCrate { ... }
     CrateDecl (term (TheCrate (crate (TraitDecl_Magic TraitDecl_Copy TraitImplDecl_Magic)))))

    (Env (term (env-for-crate-decl CrateDecl)))
    )

   (; All decls in crate are considered 'ok'.
    traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (crate-ok-goal (CrateDecl) CrateDecl)
                             EnvSubstitution)
                            EnvSubstitution)
            (term ((Env ())))))

   (; We can prove that `Foo Magic` because `Foo Copy` does holds
    traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (Implemented (Magic (Ty_Foo)))
                             EnvSubstitution)
                            EnvSubstitution)
            (term ())))

   (; But not `Bar: Magic` because `Bar: Copy` does not hold
    traced '()
           (test-equal
            (judgment-holds (prove-top-level-goal
                             Env
                             (Implemented (Magic (Ty_Bar)))
                             EnvSubstitution)
                            EnvSubstitution)
            (term ())))

   )
  )