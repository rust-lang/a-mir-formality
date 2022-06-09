#lang racket
(require redex/reduction-semantics
         "hook.rkt"
         "../grammar.rkt"
         "../elaborate.rkt"
         "../env.rkt"
         "../../util.rkt"
         )

(module+ test
  (redex-let*
   formality-logic
   [(Env (term (env-with-clauses-and-invariants ()
                                                ((∀ ((type T)) (implies ((is-implemented (Eq (T)))) (is-implemented (PartialEq (T)))))
                                                 (∀ ((type T)) (implies ((is-implemented (Ord (T)))) (is-implemented (PartialOrd (T)))))
                                                 (∀ ((type T)) (implies ((is-implemented (Ord (T)))) (is-implemented (Eq (T)))))
                                                 (∀ ((type T)) (implies ((is-implemented (PartialOrd (T)))) (is-implemented (PartialEq (T)))))
                                                 ))))
    ]

   (traced '()
           (test-equal
            (term (elaborate-hypothesis-one-step Env (is-implemented (Ord ((user-ty u32))))))
            (term ((is-implemented (Eq ((user-ty u32))))
                   (is-implemented (PartialOrd ((user-ty u32))))))))

   (traced '()
           (test-equal
            (term (env-hypotheses (elaborate-hypotheses (env-with-hypotheses Env ((is-implemented (Ord ((user-ty u32)))))))))
            (term ((is-implemented (Ord ((user-ty u32))))
                   (is-implemented (Eq ((user-ty u32))))
                   (is-implemented (PartialOrd ((user-ty u32))))
                   (is-implemented (PartialEq ((user-ty u32))))))))

   (traced '()
           (test-alpha-equivalent
            formality-logic
            (term (env-hypotheses (elaborate-hypotheses
                                   (env-with-hypotheses Env
                                                        ((∀ ((type T)) (is-implemented (Ord (T)))))))))
            (term ((∀ ((type T)) (is-implemented (Ord (T))))
                   (∀ ((type T)) (is-implemented (Eq (T))))
                   (∀ ((type T)) (is-implemented (PartialOrd (T))))
                   (∀ ((type T)) (is-implemented (PartialEq (T)))))))

           )
   )
  )