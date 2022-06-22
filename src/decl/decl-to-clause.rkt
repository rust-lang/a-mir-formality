#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "where-clauses.rkt"
         "../logic/env.rkt"
         "../ty/relate.rkt"
         "../ty/predicate.rkt"
         "../ty/hook.rkt"
         "decl-to-clause/crate-decl.rkt"
         "decl-to-clause/default-rules.rkt")
(provide env-for-crate-decl
         env-for-crate-decls
         env-for-decl-program
         )

(define-metafunction formality-decl
  ;; Convenience function: add the clauses/hypothesis from a single crate
  ;; into the environment.
  env-for-crate-decl : CrateDecl -> Env

  [(env-for-crate-decl CrateDecl)
   (env-for-crate-decls (CrateDecl) CrateId)
   (where/error (CrateId CrateContents) CrateDecl)
   ]
  )

(define-metafunction formality-decl
  ;; Add the clauses/hypothesis from multiple crates
  ;; into the environment, where CrateId names the current crate.
  env-for-crate-decls : CrateDecls CrateId -> Env

  [(env-for-crate-decls CrateDecls CrateId)
   (env-for-decl-program (CrateDecls CrateId))
   ]
  )

(define-metafunction formality-decl
  ;; Create an environment for the given program.
  env-for-decl-program : DeclProgram -> Env

  [(env-for-decl-program (CrateDecls CrateId))
   (env-with-hook (formality-decl-hook DeclProgram))
   (where/error DeclProgram (CrateDecls CrateId))
   ]
  )

(define-metafunction formality-decl
  ;; The "hook" for a decl program -- given a set of create-decls and a current
  ;; crate, lowers those definitions to program clauses on demand using
  ;; decl-clauses-for-predicate.
  formality-decl-hook : DeclProgram -> Hook

  [(formality-decl-hook DeclProgram)
   (Hook: ,(formality-ty-hook
            (lambda (predicate)
              (term (decl-clauses-for-predicate DeclProgram ,predicate)))
            (lambda ()
              (term (decl-invariants DeclProgram)))
            (lambda (env relation)
              (term (ty:relate-parameters ,env ,relation)))
            (lambda (goal)
              (term (ty:is-predicate? ,goal)))
            (lambda (predicate1)
              (term (ty:debone-predicate ,predicate1)))
            (lambda (goal)
              (term (ty:is-relation? ,goal)))
            (lambda (adt-id)
              (term (generics-for-adt-id DeclProgram ,adt-id)))
            (lambda (where-clause)
              (term (where-clause->goal∧clause CrateDecls ,where-clause)))
            ))

   (where/error (CrateDecls CrateId) DeclProgram)
   ]
  )

(define-metafunction formality-decl
  ;; Part of the "hook" for a formality-decl program:
  ;;
  ;; Create the clauses for solving a given predicate
  ;; (right now the predicate is not used).
  decl-clauses-for-predicate : DeclProgram Predicate -> Clauses

  [(decl-clauses-for-predicate DeclProgram Predicate)
   Clauses
   (where/error (Clauses _) (program-rules DeclProgram))]
  )

(define-metafunction formality-decl
  ;; Part of the "hook" for a formality-decl program:
  ;;
  ;; Create the invariants from a given program.
  decl-invariants : DeclProgram -> Invariants

  [(decl-invariants DeclProgram)
   Invariants
   (where/error (_ Invariants) (program-rules DeclProgram))]
  )

(define-metafunction formality-decl
  ;; Return the clauses/hypothesis from multiple crates
  ;; with CrateId as the crate being compiled.
  ;;
  ;; NB: This assumes that we can compile to a complete set of
  ;; clauses. This will eventually not suffice, e.g., with
  ;; auto traits. But this helper is private, so we can refactor
  ;; that later.
  program-rules : DeclProgram -> (Clauses Invariants)

  [(program-rules (CrateDecls CrateId))
   ((flatten (Clauses_c ... Clauses_bi))
    (flatten (Invariants_c ... Invariants_bi)))
   (where (CrateDecl ...) CrateDecls)
   (where/error (Clauses_bi Invariants_bi) (default-rules ()))
   (where/error ((Clauses_c Invariants_c) ...) ((crate-decl-rules CrateDecls CrateDecl CrateId) ...))
   ]
  )


(define-metafunction formality-decl
  ;; Part of the "hook" for a formality-decl program:
  ;;
  ;; Create the clauses for solving a given predicate
  ;; (right now the predicate is not used).
  generics-for-adt-id : DeclProgram AdtId -> Generics

  [(generics-for-adt-id (CrateDecls CrateId) AdtId)
   (((VarId (ParameterKind =)) ...) WhereClauses) ; for now we hardcode `=` (invariance) as the variance
   (where/error (AdtKind AdtId ((ParameterKind VarId) ...) WhereClauses AdtVariants) (adt-with-id CrateDecls AdtId))
   ]
  )
