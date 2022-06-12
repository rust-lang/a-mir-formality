#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "../where-clauses.rkt"
         "../feature-gate.rkt"
         "../../logic/env.rkt"
         "../../ty/relate.rkt"
         "../../ty/could-match.rkt"
         "../../ty/user-ty.rkt"
         "../../ty/hook.rkt")
(provide default-rules
         )

(define-metafunction formality-decl
  ;; Given a crate item, return a tuple of:
  ;;
  ;; * The clauses that hold in all crates due to this item
  ;; * The invariants that hold in all crates due to this item
  ;; * The invariants that hold only in the crate that declared this item
  default-rules : () -> (Clauses Invariants)

  ((default-rules ())
   (((well-formed (type (user-ty i32)))
     (well-formed (type (user-ty u32)))
     (well-formed (type (user-ty ())))
     )
    ())
   )

  )