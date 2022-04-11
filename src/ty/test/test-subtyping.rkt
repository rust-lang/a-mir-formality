#lang racket
(require redex/reduction-semantics
         "hook.rkt"
         "../grammar.rkt"
         "../../util.rkt"
         )

(module+ test
  (redex-let*
   formality-ty

   ((; Env that justs ignore WellFormed rules, not interesting for testing subtyping
     Env (term (env-with-clauses-invariants-and-generics
                ((ForAll ((TyKind T)) (WellFormed (TyKind T))))
                ()
                ()
                )))
    )

   (traced '()
           (ty:test-can-prove Env ((ForAll ((TyKind T)) T) <= (scalar-ty u32)))
           )

   (traced '()
           (ty:test-can-prove Env ((scalar-ty u32) >= (ForAll ((TyKind T)) T)))
           )

   (traced '()
           (ty:test-cannot-prove Env ((ForAll ((TyKind T)) T) >= (scalar-ty u32)))
           )

   )
  )
