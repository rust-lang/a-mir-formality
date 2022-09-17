#lang racket
(require redex/reduction-semantics
         )
(provide formality-logic
         RootUniverse
         true-goal
         false-goal
         )

(define-language formality-logic
  ;; The "hook" is a bit of a hack that allows the environment
  ;; to (on demand) get access to clauses and invariants from the
  ;; program without actually knowing the full representation
  ;; of the program itself. See hook.rkt for more details.
  (Hook ::= (Hook: any))

  ;; Parameters to the logic:
  (Parameter ::= Term)          ; Value of a variable
  (ParameterKind ::= Term)      ; Kinds for variables (e.g., type/lifetimes)
  (Predicate ::= Term)          ; Kinds of predicates we can prove
  (Predicate/Skeleton ::= Term) ; Variant of predicates that has no parameters (see Predicate/Deboned)
  (VarInequality ::= Term)      ; Variable relationships inferred and stored in the environment
  (InequalityOp ::= Term)       ; Relations between terms beyond `==`

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

  ;; EnvOutput: When trying to find answers to a goal, we sometimes
  ;; get an `ambiguous` result. `ambiguous` means that the result could not be
  ;; definitively determined as true or false. It occurs in a variety of cases, e.g.
  ;;
  ;; * there wasn't enough type information known to resolve the goal;
  ;; * overflow (in some checkers, anyway).
  (EnvOutput ::=
             Env
             ambiguous
             )

  ;; Maps variables to their values; those values are not core to the
  ;; logic, though.
  (Substitution ::= ((VarId Parameter) ...))
  (Substitution-or-error ::= Substitution Error)

  ;; VarBinder -- maps a `VarId` to a kind (ty/lifetime/etc), quantifier kind (forall/exists),
  ;; and universe
  (VarBinders ::= (VarBinder ...))
  (VarBinder ::= (VarId ParameterKind Quantifier Universe))

  ;; ∃VarBinder -- Specialized variant of `VarBinder` that must be existential
  (∃VarBinders ::= [∃VarBinder ...])
  (∃VarBinder ::= (VarId ParameterKind ∃ Universe))

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

  ;; `Predicate/Deboned` -- A *deboned* predicate separates out the "rigid part"
  ;; (the skeleton) from input/output parameters. To determine whether two
  ;; predicates are equal, the skeletons can just be compared for
  ;; equality, but the parameters have to be equated as types.
  (Predicate/Deboned ::= (Predicate/Skeleton Parameters_input -> Parameters_output))

  ;; ANCHOR:GoalsAndHypotheses
  ;; `Goal` -- things we can prove. These consists of predicates
  ;; joined by various forms of logical operators that are built
  ;; into the proving system (see `cosld-solve.rkt`).
  ;;
  ;; `AtomicGoal` -- goals whose meaning is defined by the
  ;; upper layers and is opaque to this layer. We break them into
  ;; two categories, predicates and relations, which affects how
  ;; the upper layers convey their meaning to us:
  ;;
  ;; * For *predicates* the upper layer gives us a set of `Clause`
  ;;   instances we can use to prove them true.
  ;;
  ;; * For *relations* the upper layer directly manipulates the
  ;;   environment using a callback function and gives us a set of
  ;;   subsequent goals. This is used for things like subtyping that use a
  ;;   very custom search strategy to avoid getting "lost in the solver" and to implement
  ;;   inference.
  ;;
  ;; `BuiltinGoal` -- defines logical connectives that the solver layer
  ;; directly manages.
  ;;
  ;; The term `Goal` is commonly used
  ;; in logic programming contexts with this meaning.
  (Goals = (Goal ...))
  (Goal ::= AtomicGoal BuiltinGoal)
  (AtomicGoals ::= [AtomicGoal ...])
  (AtomicGoal ::=
              Predicate
              Relation)
  (BuiltinGoal ::=
               ambiguous
               (&& Goals)
               (|| Goals)
               (implies Hypotheses Goal)
               (Quantifier KindedVarIds Goal)
               )

  ;; A `Goal` can be classified into the following categories.
  ;; Since the logic layer doesn't know the syntax of predicates it
  ;; has to use the hook-callback to get this classification.
  (Goal/Categorization ::=
                       (user-predicate Prove/Coinductive)
                       builtin-predicate
                       builtin-relation
                       builtin-goal
                       )

  ;; `Clause`, `Hypothesis` -- axioms. These are both built-in and derived from
  ;; user-defined items like `trait` and `impl`.
  ;;
  ;; The term `Clause` (and "program clause") is commonly used
  ;; in logic programming contexts with this meaning.
  (Hypotheses Clauses ::= (Clause ...))
  (Hypothesis Clause ::=
              AtomicGoal
              (implies Goals AtomicGoal)
              (∀ KindedVarIds Clause)
              )
  ;; ANCHOR_END:GoalsAndHypotheses


  ;; A `Biformula` is a term that can be interpreted as either a goal
  ;; or a clause. As a goal, it does not take advantage of the extra
  ;; connectives that goals offer (e.g., `||` or `∃`). As a clause, implication subgoals
  ;; do not take advantage of those connectives.
  ;;
  ;; Note that -- as with goals/clauses always -- the meaning of the `&&` connective
  ;; varies depending on the role of the `Biformula`. As a goal, each of the subterms
  ;; must be proven. As a clause, any of the subterms may be assumed (as all of them have
  ;; been proven).
  ;;
  ;; Rust where-clauses can be translated into `Biformula`.
  ;;
  ;; The term `Biformula` is not in common use, we made it up.
  (Biformulas ::= (Biformula ...))
  (Biformula ::=
             AtomicGoal
             (∀ KindedVarIds Biformula)
             (implies Biformulas AtomicGoal)
             )

  ;; A `FlatHypothesis` is a flattened form of hypotheses; it is equally expressive
  ;; with the recursive structure. Useful for matching.
  (FlatHypotheses ::= (FlatHypothesis ...))
  (FlatHypothesis ::= (∀ KindedVarIds FlatImplicationHypothesis))
  (FlatImplicationHypotheses ::= (FlatImplicationHypothesis ...))
  (FlatImplicationHypothesis ::= (implies Goals AtomicGoal))

  ;; `Invariants` -- things which must be true or the type system has some bugs.
  ;; A rather restricted form of clause.
  (Invariants ::= (Invariant ...))
  (Invariant ::= (∀ KindedVarIds (implies (Predicate) AtomicGoal)))

  ;; Different ways to relate parameters
  (Relations ::= (Relation ...))
  (Relation ::= (Parameter RelationOp Parameter))
  (RelationOp ::= == InequalityOp)

  ;; `Quantifier` -- the two kinds of quantifiers.
  (Quantifier ::= ∀ ∃)

  ;; `Universe` -- the root universe `RootUniverse` consists of all user-defined names.
  ;; Each time we enter into a `∀` quantifier, we introduce a new universe
  ;; that extends the previous one to add new names that didn't exist in the old
  ;; universe (e.g., the placeholders for the universally quantified variables).
  ;; See the paper XXX
  (Universes ::= (Universe ...))
  (Universe ::= (universe number))
  (UniversePairs ::= (UniversePair ...))
  (UniversePair ::= (Universe Universe))

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
  (Prove/Progress ::= no-progress made-progress)

  ;; `QueryGoal` -- A *query term* is a goal packaged up with variables that
  ;; are referenced in the goal. Each variable carries its quantifier (∀ vs ∃)
  ;; along with a universe.
  ;;
  ;; Given a query, one can create an environment that has all the given variables
  ;; and universes in scope. This can be used to solve the canonical term and produce a result.
  ;;
  ;; The result of a query is a `Solution` (or set of `Solutions`).
  (QueryGoal ::= (?- VarBinders (implies Hypotheses Goal)))

  ;; A *solution* consists of inference variables and relations between them.
  ;; When the solver decides something is provable, it does so modulo the "solution",
  ;; which may contain various relations that were not proven, and which must be
  ;; proven at another level. Note that these are 'first-order' relations, though.
  (Solutions ::= (Solution ...))
  (Solution ::= (∃VarBinders (Substitution Relations)))

  #:binding-forms
  (∀ ((ParameterKind VarId) ...) any #:refers-to (shadow VarId ...))
  (∃ ((ParameterKind VarId) ...) any #:refers-to (shadow VarId ...))
  )

(define-term
  RootUniverse
  (universe 0)
  )

(define-term
  true-goal
  (&& ())
  )

(define-term
  false-goal
  (|| ())
  )