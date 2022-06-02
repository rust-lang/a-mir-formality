#lang racket
(require redex/reduction-semantics
         "../all-check.rkt"
         "../grammar.rkt"
         "../../ty/grammar.rkt"
         "../../ty/user-ty.rkt"
         "../../util.rkt"
         )

(module+ test
  ;; Program:
  ;;
  ;; fn foo(x: i32) -> i32 {
  ;;     x
  ;; }
  (redex-let*
   formality-mir

   ((LocalDecls_foo (term ((_0 (user-ty i32) mut)
                           (_1 (user-ty i32) ())
                           )))
    (BasicBlockData_bb0 (term (((_0 = (use (copy _1)))) return)))
    (BasicBlockDecl_bb0 (term (bb0 BasicBlockData_bb0)))
    (FnBody_foo (term (; fn foo(_1: i32) -> i32 {
                       ;     debug x => _1;                       // in scope 0 at src/lib.rs:1:8: 1:9
                       ;     let mut _0: i32;                     // return place in scope 0 at src/lib.rs:1:19: 1:22
                       ;
                       ;     bb0: {
                       ;         _0 = _1;                         // scope 0 at src/lib.rs:2:5: 2:6
                       ;         return;                          // scope 0 at src/lib.rs:3:2: 3:2
                       ;     }
                       ; }
                       ∃ [] (
                             LocalDecls_foo

                             (BasicBlockDecl_bb0
                              )
                             )
                         )))
    (FnDecl_foo (term (foo (fn-decl ()
                                    ((user-ty i32) (user-ty i32))
                                    (user-ty i32)
                                    ()
                                    FnBody_foo))))
    (CrateDecl_the-crate (term (the-crate (crate (FnDecl_foo)))))
    (DeclProgram (term ((CrateDecl_the-crate) the-crate)))
    )

   (traced '()
           (test-judgment-holds (✅-Program DeclProgram)))
   )
  )
