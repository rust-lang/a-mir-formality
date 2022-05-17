#lang racket
(require redex/reduction-semantics
         "../../decl-to-clause.rkt"
         "../../decl-ok.rkt"
         "../../grammar.rkt"
         "../../prove.rkt"
         "../../../util.rkt")

(module+ test
  (; Magic trait, implemented in terms of itself, that extends Copy
   redex-let*
   formality-decl

   ((; struct Foo { counter: i32 }
     AdtDecl_Foo (term (Foo (struct () () ((struct-variant ((counter (scalar-ty i32)))))))))

    (; reference to `Foo`
     Ty_Foo (term (rigid-ty Foo ())))

    (; trait Magic: Copy { }
     TraitDecl_Magic (term (Magic (trait ((type Self)) ((is-implemented (Copy (Self)))) ()))))

    (; trait Copy { }
     TraitDecl_Copy (term (Copy (trait ((type Self)) () ()))))

    (; impl<T> Magic for T where T: Magic { }
     TraitImplDecl_Magic (term (impl ((type T)) (Magic (T)) ((is-implemented (Magic (T)))) ())))

    (; crate TheCrate { ... }
     CrateDecl (term (TheCrate (crate (TraitDecl_Magic TraitDecl_Copy TraitImplDecl_Magic)))))

    (Env (term (env-for-crate-decl CrateDecl)))
    )

   (; All decls in crate are considered 'ok'. In particular, the impl is considered 'ok',
    ; since its where clauses allow it to locally prove that `Self: Copy`.
    traced '()
           (decl:test-can-prove
            Env
            (crate-ok-goal (CrateDecl) CrateDecl)))

   (; ...but when we try to use it, we cannot prove that `i32: Magic`
    ; because `i32: Copy` does not hold...
    traced '()
           (decl:test-cannot-prove
            Env
            (is-implemented (Magic (Ty_Foo)))))

   (; ...also cannot prove that `i32: Copy`, of course.
    traced '()
           (decl:test-cannot-prove
            Env
            (is-implemented (Copy (Ty_Foo)))))
   )

  (; Mutual recursion between Magic and Copy, with Magic implemented in terms of itself,
   ; but no impl of Copy
   redex-let*
   formality-decl

   ((; struct Foo { counter: i32 }
     AdtDecl_Foo (term (Foo (struct () () ((struct-variant ((counter (scalar-ty i32)))))))))

    (; reference to `Foo`
     Ty_Foo (term (rigid-ty Foo ())))

    (; trait Magic: Copy { }
     TraitDecl_Magic (term (Magic (trait ((type Self)) ((is-implemented (Copy (Self)))) ()))))

    (; trait Copy: Magic { }
     TraitDecl_Copy (term (Copy (trait ((type Self)) ((is-implemented (Magic (Self)))) ()))))

    (; impl<T> Magic for T where T: Magic { }
     TraitImplDecl_Magic (term (impl ((type T)) (Magic (T)) ((is-implemented (Magic (T)))) ())))

    (; crate TheCrate { ... }
     CrateDecl (term (TheCrate (crate (TraitDecl_Magic TraitDecl_Copy TraitImplDecl_Magic)))))

    (Env (term (env-for-crate-decl CrateDecl)))
    )

   (; All decls in crate are considered 'ok'.
    traced '()
           (decl:test-can-prove
            Env
            (crate-ok-goal (CrateDecl) CrateDecl)))

   (; Cannot prove that `i32: Magic`
    traced '()
           (decl:test-cannot-prove
            Env
            (is-implemented (Magic (Ty_Foo)))))

   (; And cannot prove that `i32: Copy`
    traced '()
           (decl:test-cannot-prove
            Env
            (is-implemented (Copy (Ty_Foo)))))

   )

  (; Mutual recursion between Magic and Copy, with Magic implemented in terms of itself
   ; *and* Copy implemented
   redex-let*
   formality-decl

   ((; struct Foo { counter: i32 }
     AdtDecl_Foo (term (Foo (struct () () ((struct-variant ((counter (scalar-ty i32)))))))))

    (; reference to `Foo`
     Ty_Foo (term (rigid-ty Foo ())))

    (; struct Bar { counter: i32 }
     AdtDecl_Foo (term (Foo (struct () () ((struct-variant ((counter (scalar-ty i32)))))))))

    (; reference to `Bar`
     Ty_Bar (term (rigid-ty Bar ())))

    (; trait Magic: Copy { }
     TraitDecl_Magic (term (Magic (trait ((type Self)) ((is-implemented (Copy (Self)))) ()))))

    (; trait Copy { }
     TraitDecl_Copy (term (Copy (trait ((type Self)) () ()))))

    (; impl<T> Magic for T where T: Magic { }
     TraitImplDecl_Magic (term (impl ((type T)) (Magic (T)) ((is-implemented (Magic (T)))) ())))

    (; impl Copy for Foo { }
     TraitImplDecl_Copy (term (impl () (Copy (Ty_Foo)) () ())))

    (; crate TheCrate { ... }
     CrateDecl (term (TheCrate (crate (TraitDecl_Magic TraitDecl_Copy TraitImplDecl_Magic)))))

    (Env (term (env-for-crate-decl CrateDecl)))
    )

   (; All decls in crate are considered 'ok'.
    traced '()
           (decl:test-can-prove
            Env
            (crate-ok-goal (CrateDecl) CrateDecl)))

   (; We can prove that `Foo Magic` because `Foo Copy` does holds
    traced '()
           (decl:test-cannot-prove
            Env
            (is-implemented (Magic (Ty_Foo)))))

   (; But not `Bar: Magic` because `Bar: Copy` does not hold
    traced '()
           (decl:test-cannot-prove
            Env
            (is-implemented (Magic (Ty_Bar)))))

   )
  )