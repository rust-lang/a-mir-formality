#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../logic/env.rkt"
         "../logic/solve-query.rkt"
         "../ty/relate.rkt"
         "../ty/predicate.rkt"
         "../ty/hook.rkt"
         "../ty/elaborate-relation.rkt"
         "decl-to-clause.rkt"
         "builtin-predicate.rkt"
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
   (where/error (crate CrateId _) CrateDecl)
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
  ;; Solve the given query in the context of the given program.
  decl:solve-query : DeclProgram QueryGoal -> Solutions

  [(decl:solve-query DeclProgram QueryGoal)
   (logic:solve-query Hook QueryGoal)
   (where/error Hook (formality-decl-hook DeclProgram))
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
              (term (ty:elaborate-relation ,env ,relation)))
            (lambda (env relation)
              (term (ty:relate-parameters ,env ,relation)))
            (lambda (env predicate)
              (term (decl:solve-builtin-predicate CrateDecls ,env ,predicate)))
            (lambda (predicate1)
              (term (ty:debone-predicate ,predicate1)))
            (lambda (goal)
              (term (decl:categorize-goal ,goal)))
            (lambda (adt-id)
              (term (generics-for-adt-id CrateDecls ,adt-id)))
            ))

   (where/error (CrateDecls CrateId) DeclProgram)
   ]
  )

(define-metafunction formality-decl
  ;; Part of the "hook" for a formality-decl program:
  ;;
  ;; Create the clauses for solving a given predicate
  ;; (right now the predicate is not used).
  generics-for-adt-id : CrateDecls AdtId -> Generics

  [(generics-for-adt-id CrateDecls AdtId)
   (((VarId (ParameterKind =)) ...) Biformulas) ; for now we hardcode `=` (invariance) as the variance
   (where/error (AdtKind AdtId ((ParameterKind VarId) ...) where Biformulas AdtVariants) (adt-with-id CrateDecls AdtId))
   ]
  )