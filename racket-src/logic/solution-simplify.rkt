#lang racket
(require redex/reduction-semantics
         "../util.rkt"
         "env.rkt"
         "grammar.rkt"
         )
(provide simplify-solution
         )

(define-metafunction formality-logic
  ;; Applies hard-coded simplification rules repeatedly to simplify a Solution as much
  ;; as possible.
  ;;
  ;; These make solutions easier to read but may also help the compiler figure
  ;; things out sometimes.
  ;;
  ;; The rules are as follows:
  ;;
  ;; * We can remove existential variables that don't appear in any relations or in the substitution.
  ;; * We can remove any relation `Parameter_0 <Op> Parameter_1` where for `i` in {0,1}...
  ;;   * `Parameter_i` is bound as an existential in the solution.
  ;;   * `Parameter_i` only appears in this relation and nowhere else.
  simplify-solution : Solution -> Solution

  [(simplify-solution Solution)
   Solution
   (where Solution (simplify-solution-once Solution))
   ]

  [(simplify-solution Solution)
   (simplify-solution Solution_next)
   (where Solution_next (simplify-solution-once Solution))
   ]
  )

(define-metafunction formality-logic
  ;; Applies a single round of the hard-coded simplification rules.
  simplify-solution-once : Solution -> Solution

  [(simplify-solution-once Solution)
   (:- ∃VarBinders_o (Substitution_o Relations_o))

   (where/error (:- ∃VarBinders (Substitution Relations)) Solution)
   (where/error [∃VarBinder ...] ∃VarBinders)
   (where/error [(VarId _ _ _) ...] ∃VarBinders)
   (where/error [number ...] [(count-appearances VarId (Substitution Relations)) ...])
   (where/error Appearances [(VarId number) ...])

   ; remove any unused variables
   (where/error ∃VarBinders_o (flatten [(remove-unused-varbinding Appearances ∃VarBinder) ...]))

   ; remove substitution mappings if the variable is only used once
   ;
   ; FIXME-- for this to be valid, I think we must check the universe of the uniuse variable
   ; and ensure it is the same as the variable that is being mapped to it.
   #;(where/error [(VarId_from Parameter_to) ...] Substitution)
   #;(where/error Substitution_o (flatten [(remove-uniuse-substitution Appearances VarId_from Parameter_to) ...]))
   (where/error Substitution_o Substitution)

   ; remove relations if the lhs or rhs is only used once
   (where/error [Relation ...] Relations)
   (where/error Relations_o (flatten [(remove-uniuse-relation Appearances Relation) ...]))
   ]

  )

(define-metafunction formality-logic
  ;; Counts the number of times that `VarId` appears in `Term`.
  count-appearances : VarId Term -> number

  [(count-appearances VarId VarId) 1]

  [(count-appearances VarId [Term ...])
   ,(apply + (term [(count-appearances VarId Term) ...]))
   ]

  [(count-appearances VarId _) 0]

  )

(define-metafunction formality-logic
  ;; Given a variable `VarId_uni` only used once, and a mapping `VarId_from -> Parameter_to`
  ;; from the solution substitution, returns:
  ;;
  ;; * an empty substitutio `[]`, if `Parameter_to == VarId_uni`. This corresponds
  ;;   to a case where the solution had the shape `∃X. [(T X)]`, ie, `T` has to be equal to
  ;;   some unconstrained value `?X`.
  ;; * the substitution `[(VarId_from Parameter_to)]` otherwise.
  remove-unused-varbinding : Appearances ∃VarBinder -> ∃VarBinders

  ; Filter out ∃VarBinding's that have no uses
  [(remove-unused-varbinding [_ ... (VarId 0) _ ...] (VarId _ _ _))
   []]

  ; Keep the rest
  [(remove-unused-varbinding _ ∃VarBinder)
   [∃VarBinder]
   ]

  )

#;(define-metafunction formality-logic
    ;; Maps a substitution pair (VarId_from Parameter_to) to an empty list `[]` if
    ;;`Parameter_to` is an existentially bound variable `VarId` that is only used
    ;; once in the result.
    ;;
    ;; Corresponds to a solution like `(:- [X] ([T X] ...))`. i.e., saying that the goal
    ;; is true if `T == ?X` for some fresh `?X`, which adds no real information.
    ;; (This relies on the fact that we won't add a mapping like this unless X is in
    ;; a universe that can name T.)
    remove-uniuse-substitution : Appearances VarId_from Parameter_to  -> Substitution

    [(remove-uniuse-substitution [_ ... (VarId_to 1) _ ...] _ VarId_to)
     []]

    [(remove-uniuse-substitution _ VarId_from Parameter_to)
     [(VarId_from Parameter_to)]]

    )

(define-metafunction formality-logic
  ;; Maps a relation `Relation` to an empty list `[]` if the relation
  ;; has the form `VarId <op> _` or `_ <op> VarId` where `VarId`
  ;; is an existentially bound variable that is only used
  ;; once in the result.
  ;;
  ;; This corresponds to a case where the solution had the shape
  ;; `∃X. ([] (T : X))`, ie, `T` has to outlive any value `X`,
  ;; which will always be true. (This relies on the fact that
  ;; we won't add a relation like this unless X is in a universe that
  ;; can name T.)
  remove-uniuse-relation : Appearances Relation -> Relations

  [(remove-uniuse-relation [_ ... (VarId 1) _ ...] (VarId RelationOp _))
   []
   ]

  [(remove-uniuse-relation [_ ... (VarId 1) _ ...] (_ RelationOp VarId))
   []
   ]

  [(remove-uniuse-relation _ Relation)
   [Relation]
   ]

  )

(module+ test

  ; Test example like `∃X. T = X`
  ;
  ; This cannot be simplified with the current rules, though we should fix that.
  (test-alpha-equivalent
   formality-logic
   (term (simplify-solution (:- [(X type ∃ (universe 1))] ([(T X)] []))))
   (term (:- [(X type ∃ (universe 1))] ([(T X)] [])))
   )

  ; Test example like `∃X. T : X`
  (test-alpha-equivalent
   formality-logic
   (term (simplify-solution (:- [(X lifetime ∃ (universe 1))] ([] [(T -outlives- X)]))))
   (term (:- [] ([] [])))
   )

  ; Test example like `∃X. A = X ∧ T : X`
  ;
  ; This cannot be simplified with the current rules.
  (test-alpha-equivalent
   formality-logic
   (term (simplify-solution (:- [(X lifetime ∃ (universe 1))] ([(A X)] [(T -outlives- X)]))))
   (term (:- [(X lifetime ∃ (universe 1))] ([(A X)] [(T -outlives- X)])))
   )


  ; Test example like `∃X,Y. A = X ∧ T : X ∧ T : Y`
  ;
  ; The `Y` variable can be simplified away, but not `X`.
  (test-alpha-equivalent
   formality-logic
   (term (simplify-solution (:- [(X lifetime ∃ (universe 1)) (Y lifetime ∃ (universe 1))] ([(A X)] [(T -outlives- X) (T -outlives- Y)]))))
   (term (:- [(X lifetime ∃ (universe 1))] ([(A X)] [(T -outlives- X)])))
   )

  ; Test example like `∃X. T : X ∧ U : X`
  ;
  ; This cannot be simplified with the current rules, although perhaps
  ; we should extend the rules to support it!
  (test-alpha-equivalent
   formality-logic
   (term (simplify-solution (:- [(X lifetime ∃ (universe 1))] ([] [(T -outlives- X) (U -outlives- X)]))))
   (term (:- [(X lifetime ∃ (universe 1))] ([] [(T -outlives- X) (U -outlives- X)])))
   )
  )