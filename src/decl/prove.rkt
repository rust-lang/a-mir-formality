#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../logic/cosld-solve.rkt"
         )
(provide decl:prove-top-level-goal/cosld
         )

(define-extended-judgment-form formality-decl logic:prove-top-level-goal/cosld
  ;; Prove a "top-level" goal is true in the given environment
  ;; using the cosld solver. cosld is a basic [SLD] solving algorithm,
  ;; enriched to handle [FOHH] predicates as well as to
  ;; support a simple form of coinduction.
  ;;
  ;; [SLD]: https://en.wikipedia.org/wiki/SLD_resolution
  ;; [FOHH]: https://en.wikipedia.org/wiki/Harrop_formula
  #:mode (decl:prove-top-level-goal/cosld I I O)
  #:contract (decl:prove-top-level-goal/cosld Env Goal Env)

  )
