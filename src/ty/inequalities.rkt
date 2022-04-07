#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../logic/env.rkt"
         )
(provide known-bounds
         variable-bounds
         env-with-var-related-to-parameter
         remove-var-bounds-from-env
         invert-inequality-op
         )

(define-metafunction formality-ty
  ;; Returns the bounds on `VarId_in` found in the environment, or `(() ())` if none are found.
  ;;
  ;; `VarId_in` must be a variable declared in the environment and must not be unmapped.
  invert-inequality-op : InequalityOp -> InequalityOp

  [(invert-inequality-op <=) >=]
  [(invert-inequality-op >=) <=]
  )

(define-metafunction formality-ty
  ;; Returns the bounds on `VarId_in` found in the environment, or `(() ())` if none are found.
  ;;
  ;; `VarId_in` must be a variable declared in the environment and must not be unmapped.
  variable-bounds : Env_in VarId_in -> (Parameters_lb Parameters_ub)
  #:pre (env-contains-unmapped-existential-var Env_in VarId_in)

  [(variable-bounds Env VarId)
   (Parameters_lb Parameters_ub)
   (where (_ ... (Parameters_lb <= VarId <= Parameters_ub) _ ...) (env-inequalities Env))
   ]

  [(variable-bounds Env VarId)
   (() ())
   ]
  )

(define-metafunction formality-ty
  ;; Given a variable `VarId_in` and a inequality op (e.g., `<=`, `>=`), returns the known
  ;; bounds `P` such that e.g. `P <= VarId` or `VarId <= P`.
  known-bounds : Env_in InequalityOp VarId_in -> Parameters_ub
  #:pre (env-contains-unmapped-existential-var Env_in VarId_in)

  [(known-bounds Env <= VarId)
   Parameters_lb
   (where/error (Parameters_lb Parameters_ub) (variable-bounds Env_in VarId_in))]

  [(known-bounds Env >= VarId)
   Parameters_ub
   (where/error (Parameters_lb Parameters_ub) (variable-bounds Env_in VarId_in))]

  )

(define-metafunction formality-ty
  ;; Removes `VarId_in` from the list of bounded variables in `Env_in` (assuming it is
  ;; present) and returns the resulting environment as well as whatever bounds `VarId_in`
  ;; had.
  ;;
  ;; `VarId_in` must be a variable declared in the environment and must not be unmapped.
  remove-var-bounds-from-env : Env_in VarId_in -> (Env (Parameters_lb Parameters_ub))
  #:pre (env-contains-unmapped-existential-var Env_in VarId_in)

  [(remove-var-bounds-from-env Env VarId)
   ((env-with-inequalities (VarInequality_0 ... VarInequality_1 ...)) (Parameters_lb Parameters_ub))
   (where (VarInequality_0 ... (Parameters_lb <= VarId <= Parameters_ub) VarInequality_1 ...) (env-inequalities Env))
   ]

  [(remove-var-bounds-from-env Env VarId)
   (Env (() ()))
   ]

  )

(define-metafunction formality-ty
  ;; Extends the environment with an additional bound for `VarId_in` (has no effect if this bound
  ;; is already present).
  ;;
  ;; `VarId_in` must be a variable declared in the environment and must not be unmapped.
  env-with-var-related-to-parameter : Env_in VarId_in InequalityOp Parameter -> Env
  #:pre (env-contains-unmapped-existential-var Env_in VarId_in)

  [(env-with-var-related-to-parameter Env VarId InequalityOp Parameter)
   (env-with-inequalities (VarInequality_0 ... VarInequality VarInequality_1 ...))
   (where (VarInequality_0 ... (Parameters_lb <= VarId <= Parameters_ub) VarInequality_1 ...) (env-inequalities Env))
   (where/error VarInequality (var-inequality-with-parameter (Parameters_lb <= VarId <= Parameters_ub) InequalityOp Parameter))
   ]

  [(env-with-var-related-to-parameter Env VarId InequalityOp Parameter)
   (env-with-inequalities (VarInequality (VarInequality_0 ...)))
   (where/error (VarInequality_0 ...) (env-inequalities Env))
   (where/error VarInequality (var-inequality-with-parameter (() <= VarId <= ()) InequalityOp Parameter))
   ]

  )

(define-metafunction formality-ty
  ;; Adds `Parameter` into `VarInequality` in the appropriate place based on `InequalityOp`
  ;; and returns the resulting `VarInequality`.
  var-inequality-with-parameter : VarInequality InequalityOp Parameter -> VarInequality

  [(var-inequality-with-parameter (Parameters_lb <= VarId <= (Parameter_ub ...)) <= Parameter)
   (Parameters_lb <= VarId <= (Parameter Parameter_ub ...))
   (where #f (in? Parameter (Parameter_ub ...)))
   ]

  [(var-inequality-with-parameter ((Parameter_lb ...) <= VarId <= Parameters_ub) >= Parameter)
   ((Parameter Parameter_lb ...) <= VarId <= Parameters_ub)
   (where #f (in? Parameter (Parameter_lb ...)))
   ]

  [(var-inequality-with-parameter VarInequality _ Parameter)
   VarInequality
   ]

  )
