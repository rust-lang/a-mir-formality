#lang racket
(require redex/reduction-semantics
         "../env.rkt"
         "../decl-ok.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         "../../util.rkt")

;; Various tests that check the requirements that where clauses be well-formed.

(module+ test

  (redex-let*
   formality-decl

   ((; trait Foo<T> where Self: Bar, T: Bar { }
     TraitDecl_Foo (term (trait Foo ((type Self) (type T))
                                where ((T : Bar()) (Self : Bar()))
                                {})))

    (; trait Bar { }
     TraitDecl_Bar (term (trait Bar ((type Self)) where () {})))

    (Env (term (env-for-crate-decl (crate C {TraitDecl_Foo
                                             TraitDecl_Bar}))))

    (Env_expanded (term (env-for-crate-decl (crate C {TraitDecl_Foo
                                                      TraitDecl_Bar
                                                      (feature expanded-implied-bounds)}))))
    )

   (; Knowing that `A: Foo<B>` implies that `A: Bar`
    decl:test-can-prove
    Env
    (∀ ((type A) (type B))
       (implies ((is-implemented (Foo (A B))))
                (is-implemented (Bar (A)))
                ))
    )

   (; Knowing that `A: Foo<B>` does not imply `B: Bar`
    decl:test-cannot-prove
    Env
    (∀ ((type A) (type B))
       (implies ((is-implemented (Foo (A B))))
                (is-implemented (Bar (B)))
                ))
    )

   (; Knowing that `A: Foo<B>` implies `B: Bar` w/ expanded-implied-bounds
    decl:test-can-prove
    Env_expanded
    (∀ ((type A) (type B))
       (implies ((is-implemented (Foo (A B))))
                (is-implemented (Bar (B)))
                ))
    )
   )

  (redex-let*
   formality-decl

   ((; trait Foo<'l, T> where Self: 'a, T: 'a { }
     TraitDecl_Foo (term (trait Foo ((type Self) (lifetime l) (type T))
                                where (((type T) : (lifetime l))
                                       ((type Self) : (lifetime l)))
                                {})))

    (Env (term (env-for-crate-decl (crate C {TraitDecl_Foo}))))

    (Env_expanded (term (env-for-crate-decl (crate C {TraitDecl_Foo
                                                      (feature expanded-implied-bounds)}))))
    )

   (; Knowing that `A: Foo<'a, B>` implies that `A: 'a`
    decl:test-can-prove
    Env
    (∀ ((type A) (lifetime a) (type B))
       (implies ((is-implemented (Foo (A a B))))
                (A -outlives- a)
                ))
    )

   (; Knowing that `A: Foo<'a, B>` does not imply that `B: 'a`
    decl:test-cannot-prove
    Env
    (∀ ((type A) (lifetime a) (type B))
       (implies ((is-implemented (Foo (A a B))))
                (B -outlives- a)
                ))
    )

   (; Knowing that `A: Foo<'a, B>` implies `B: 'a` w/ expanded-implied-bounds
    decl:test-can-prove
    Env_expanded
    (∀ ((type A) (lifetime a) (type B))
       (implies ((is-implemented (Foo (A a B))))
                (B -outlives- a)
                ))
    )
   )
  )