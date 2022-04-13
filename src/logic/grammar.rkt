#lang racket
(require redex/reduction-semantics racket/set)
(provide formality-logic
         RootUniverse
         )

(define-language formality-logic
  ;; The "hook" is a bit of a hack that allows the environment
  ;; to (on demand) get access to clauses and invariants from the
  ;; program without actually knowing the full representation
  ;; of the program itself. See hook.rkt for more details.
  (Hook ::= (Hook: any))

  ;; Parameters to the logic:
  (Parameter ::= Term)     ; Value of a variable
  (ParameterKind ::= Term) ; Kinds for variables (e.g., type/lifetimes)
  (Predicate ::= Term)     ; Kinds of predicates we can prove
  (VarInequality ::= Term) ; Variable relationships inferred and stored in the environment

  ;; Env: Typing environment
  ;;
  ;; * Hook -- the "hook" that lets us get program information
  ;; * Universe -- the largest universe yet defined
  ;; * VarBinders -- maps variable names to quantifier-kinds/universes
  ;;   * When bound variables are instantiated, their names
  ;;     are added to this list.
  ;;   * When equating (existential) variables,
  ;;     we modify the universe that it is mapped to here
  ;; * Hypotheses -- facts believed to be true, introduced by
  ;;   where clauses
  (Envs ::= (Env ...))
  (Env ::= (Hook Universe VarBinders Substitution VarInequalities Hypotheses))

  ;; Maps variables to their values; those values are not core to the
  ;; logic, though.
  (Substitution ::= ((VarId Parameter) ...))
  (Substitution-or-error ::= Substitution Error)

  ;; VarBinder -- maps a `VarId` to a kind (ty/lifetime/etc), quantifier kind (forall/exists),
  ;; and universe
  (VarBinders ::= (VarBinder ...))
  (VarBinder ::= (VarId ParameterKind Quantifier Universe))

  ;; VarInequality -- for variables that don't have a known
  ;; value (which would appear in the substitution), we may
  ;; have an *inequality*. These are opaque to the logic layer,
  ;; they get manipulated by the type layer in the various
  ;; hook functions.
  (VarInequalities ::= (VarInequality ...))

  ;; KindedVarId: declares a bound parameter and
  ;; its kind (type, lifetime, etc).
  (KindedVarIds ::= (KindedVarId ...))
  (KindedVarId ::= (ParameterKind VarId))

  ;; `Parameters` -- parameters
  (Parameters ::= (Parameter ...))

  ;; `Predicate` -- the atomic items that we can prove
  (Predicates ::= (Predicate ...))

  ;; ANCHOR:GoalsAndHypotheses
  ;; `Goal` -- things we can prove. These consists of predicates
  ;; joined by various forms of logical operators that are built
  ;; into the proving system (see `cosld-solve.rkt`).
  (Goals = (Goal ...))
  (Goal ::=
        Predicate
        Relation
        BuiltinGoal)
  (BuiltinGoal ::=
               (All Goals)
               (Any Goals)
               (Implies Hypotheses Goal)
               (Quantifier KindedVarIds Goal)
               )

  ;; `Clause`, `Hypothesis` -- axioms. These are both built-in and derived from
  ;; user-defined items like `trait` and `impl`.
  (Hypotheses Clauses ::= (Clause ...))
  (Hypothesis Clause ::=
              Predicate
              (Implies Goals Predicate)
              (ForAll KindedVarIds Clause)
              )
  ;; ANCHOR_END:GoalsAndHypotheses

  ;; `Invariants` -- things which must be true or the type system has some bugs.
  ;; A rather restricted form of clause.
  (Invariants ::= (Invariant ...))
  (Invariant ::= (ForAll KindedVarIds (Implies (Predicate) Predicate)))

  ;; Different ways to relate parameters
  (Relations ::= (Relation ...))
  (Relation ::= (Parameter RelationOp Parameter))
  (RelationOp ::= == InequalityOp)
  (InequalityOp ::= <= >=)

  ;; `Quantifier` -- the two kinds of quantifiers.
  (Quantifier ::= ForAll Exists)

  ;; `Universe` -- the root universe `RootUniverse` consists of all user-defined names.
  ;; Each time we enter into a `ForAll` quantifier, we introduce a new universe
  ;; that extends the previous one to add new names that didn't exist in the old
  ;; universe (e.g., the placeholders for the universally quantified variables).
  ;; See the paper XXX
  (Universe ::= (UniverseId number))

  ;; Identifiers -- these are all equivalent, but we give them fresh names to help
  ;; clarify their purpose
  (VarIds ::= (VarId ...))
  (VarId AnyId ::= variable-not-otherwise-mentioned)

  ; Term -- preferred name to any that reads better :)
  (Terms ::= (Term ...))
  (Term ::= any)
  (TermPair ::= (Term Term))
  (TermPairs ::= (TermPair ...))

  ; Internal data structure used during cosld proving
  (Prove/Stacks ::= (Predicates Predicates))
  (Prove/Coinductive ::= + -)

  #:binding-forms
  (ForAll ((ParameterKind VarId) ...) any #:refers-to (shadow VarId ...))
  (Exists ((ParameterKind VarId) ...) any #:refers-to (shadow VarId ...))
  )

(define-term
  RootUniverse
  (UniverseId 0)
  )
