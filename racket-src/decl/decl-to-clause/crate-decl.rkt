#lang racket
(require redex/reduction-semantics
         "../grammar.rkt"
         "crate-item.rkt"
         )
(provide crate-decl-rules
         )

(define-metafunction formality-decl
  ;; Generate the complete set of rules that result from `CrateDecl`
  ;; when checking the crate `CrateId`.
  ;;
  ;; NB: This assumes that we can compile to a complete set of
  ;; clauses. This will eventually not suffice, e.g., with
  ;; auto traits. But this helper is private, so we can refactor
  ;; that later.
  crate-decl-rules : CrateDecls CrateDecl CrateId -> (Clauses Invariants)

  [(crate-decl-rules CrateDecls (crate CrateId_0 (CrateItemDecl ...)) CrateId_1)
   ((Clause ... ...) (Invariant ... ...))

   (where/error (((Clause ...) (Invariant ...)) ...)
                ((crate-item-decl-rules CrateDecls CrateId_0 CrateItemDecl) ...))
   ]
  )