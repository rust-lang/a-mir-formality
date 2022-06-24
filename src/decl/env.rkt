#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "where-clauses.rkt"
         "../logic/env.rkt"
         "../ty/relate.rkt"
         "../ty/predicate.rkt"
         "../ty/hook.rkt"
         "decl-to-clause.rkt"
         )
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
            (lambda (env predicate)
              (term (decl-clauses-for-predicate ,env DeclProgram ,predicate)))
            (lambda ()
              (term (decl-invariants DeclProgram)))
            (lambda (env relation)
              (term (ty:relate-parameters ,env ,relation)))
            (lambda (env predicate)
              (term Error)) ; no built-in predicates
            (lambda (goal)
              (term (ty:is-predicate? ,goal)))
            (lambda (predicate)
              (term #f)) ; no built-in predicates
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
  generics-for-adt-id : DeclProgram AdtId -> Generics

  [(generics-for-adt-id (CrateDecls CrateId) AdtId)
   (((VarId (ParameterKind =)) ...) WhereClauses) ; for now we hardcode `=` (invariance) as the variance
   (where/error (AdtKind AdtId ((ParameterKind VarId) ...) WhereClauses AdtVariants) (adt-with-id CrateDecls AdtId))
   ]
  )