#lang racket
(require redex/reduction-semantics
         "../logic/cosld-solve.rkt"
         "../ty/scheme.rkt"
         "decl-ok.rkt"
         "env.rkt"
         "grammar.rkt"
         )
(provide (all-defined-out)
         )

#;(define-metafunction formality-ty
    ;; Convenient metafunction for tests:
    ;;
    ;; Creates an environment introducing the various quantifiers etc and then the
    ;; given where-clauses (as hypotheses).
    ;;
    ;; Then proves the goal and extracts a "scheme".
    ;;
    ;; Returns the resulting scheme(s), which you can test with `test-match`.
    decl:prove-scheme-in-crate : CrateDecls CrateId ((Quantifier KindedVarIds) ...) Biformulas Goal -> Schemes

    [(decl:prove-scheme-in-crate CrateDecls CrateId ((Quantifier KindedVarIds) ...) Biformulas Goal)
     (ty:query Env ((Quantifier KindedVarIds) ...) Biformulas Goal)
     (where/error Env (env-for-crate-decls CrateDecls CrateId))
     ]

    )

(define-metafunction formality-decl
  ;; Convenient metafunction for tests:
  ;;
  ;; Creates an environment introducing the various quantifiers etc and then the
  ;; given where-clauses (as hypotheses).
  ;;
  ;; Then proves the goal and extracts a "scheme".
  ;;
  ;; Returns the resulting scheme(s), which you can test with `test-match`.
  decl:can-prove-goal : CrateDecls CrateId Goal -> boolean

  [(decl:can-prove-goal CrateDecls CrateId Goal)
   ,(judgment-holds (decl:prove-top-level-goal/cosld Env Goal _))
   (where/error Env (env-for-crate-decls CrateDecls CrateId))
   ]

  )

(define-metafunction formality-decl
  ;; Convenient metafunction for tests:
  ;;
  ;; Creates an environment introducing the various quantifiers etc and then the
  ;; given where-clauses (as hypotheses).
  ;;
  ;; Then proves the goal and extracts a "scheme".
  ;;
  ;; Returns the resulting scheme(s), which you can test with `test-match`.
  decl:is-crate-ok : CrateDecls CrateId -> boolean

  [(decl:is-crate-ok CrateDecls CrateId)
   (decl:can-prove-goal CrateDecls CrateId Goal_ok)
   (where/error [_ ... (crate CrateId CrateItemDecls) _ ...] CrateDecls)
   (where/error Goal_ok (crate-ok-goal CrateDecls (crate CrateId CrateItemDecls)))
   ]

  )

(define-extended-judgment-form formality-decl logic:prove-top-level-goal/cosld
  ;; Prove a "top-level" goal is true in the given environment
  ;; using the cosld solver. cosld is a basic [SLD] solving algorithm,
  ;; enriched to handle [FOHH] predicates as well as to
  ;; support a simple form of coinduction.
  ;;
  ;; In contrast to `logic:prove-top-level-goal/cosld`, this version
  ;; uses the `formality-decl` grammar, but is otherwise the same.
  ;;
  ;; [SLD]: https://en.wikipedia.org/wiki/SLD_resolution
  ;; [FOHH]: https://en.wikipedia.org/wiki/Harrop_formula
  #:mode (decl:prove-top-level-goal/cosld I I O)
  #:contract (decl:prove-top-level-goal/cosld Env Goal Env)

  )

(; deprecated, prefer decl:can-prove-goal
 define-syntax-rule (decl:test-can-prove env goal)
  (test-equal
   (judgment-holds (decl:prove-top-level-goal/cosld env goal _))
   #t))

(; deprecated, prefer decl:can-prove-goal
 define-syntax-rule (decl:test-cannot-prove env goal)
  (test-equal
   (judgment-holds (decl:prove-top-level-goal/cosld env goal _))
   #f))

(; deprecated, prefer decl:is-crate-ok
 define-syntax-rule (decl:test-crate-decl-ok crate-decls crate-id)
  (redex-let*
   formality-decl
   ((CrateDecls (term crate-decls))
    (CrateId (term crate-id))
    (CrateDecl (term (crate-decl-with-id CrateDecls CrateId)))
    (Env (term (env-for-crate-decls CrateDecls CrateId))))
   (decl:test-can-prove Env (crate-ok-goal CrateDecls CrateDecl))
   ))

(; deprecated, prefer decl:is-crate-ok
 define-syntax-rule (decl:test-crate-decl-not-ok crate-decls crate-id)
  (redex-let*
   formality-decl
   ((CrateDecls (term crate-decls))
    (CrateId (term crate-id))
    (CrateDecl (term (crate-decl-with-id CrateDecls CrateId)))
    (Env (term (env-for-crate-decls CrateDecls CrateId))))
   (decl:test-cannot-prove Env (crate-ok-goal CrateDecls CrateDecl))
   ))
