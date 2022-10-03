#lang racket
(require redex/reduction-semantics
         "../../util.rkt"
         "../../ty/user-ty.rkt"
         "../grammar.rkt"
         "../prove.rkt"
         )

(module+ test
  ;; Program:
  ;;
  ;; fn foo(x: &()) {}
  ;; 
  ;; fn main() {
  ;;     let f: fn(&'static ()) = foo;
  ;; }
  (traced '()
          (test-equal
           #f
           (term (rust:is-core-crate-ok
                  [(fn foo[(lifetime %a)] ((& %a ())) -> ()
                       where []
                       (∃ [(lifetime ?0)] {
                                           [(_0 (mf-apply user-ty ()) mut)
                                            (_1 (mf-apply user-ty (& ?0 ())) mut)]

                                           [(bb0 {
                                                  [(_0 = (use (const (tuple []))))]
                                                  return
                                                  })]
                                           }))
                   (fn main[] () -> ()
                       where []
                       (∃ [(lifetime ?0)
                           (lifetime ?1)
                           (lifetime ?2)] {
                                           [(_0 (mf-apply user-ty ()) mut)
                                            (_1 (mf-apply user-ty (fn ((& ?0 ())) -> ())) ())]

                                           [(bb0 {
                                                  [(storage-live _1)
                                                   (_1 = (cast (const (fn-ptr foo [])) as (mf-apply user-ty (for[(lifetime %a)] (fn ((& %a ())) -> ())))))
                                                   (_0 = (use (const (tuple []))))
                                                   (storage-dead _1)]
                                                  return
                                                  })]
                                           }))
                   ]))))
  )
