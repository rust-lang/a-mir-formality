#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "../logic/env.rkt"
         )
(provide known-bounds
         known-relations
         env-with-var-related-to-parameters
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
  [(invert-inequality-op -outlives-) -outlived-by-]
  [(invert-inequality-op -outlived-by-) -outlives-]
  )


(define-metafunction formality-ty
  ;; Given a variable `VarId_in` and a inequality op `op` (e.g. `<=`), returns the known
  ;; bounds `P` such that `P (op) VarId` (e.g., `P <= VarId`).
  ;;
  ;; (Note that these are stored in the environment with `VarId` listed first, so we actually
  ;; look for inequalities like `VarId >= P`.)
  known-bounds : Env_in InequalityOp VarId_in -> Parameters_ub
  #:pre (env-contains-unmapped-existential-var Env_in VarId_in)

  [(known-bounds Env InequalityOp_◃ VarId)
   Parameters
   (; we store bounds like `VarId <= X`, so if we want `X <= VarId`, we have to
    ; search for `VarId >= X`.
    where/error InequalityOp_▹ (invert-inequality-op InequalityOp_◃))
   (where (_ ... (VarId InequalityOp_▹ Parameters) _ ...) (env-inequalities Env))
   ]

  [(known-bounds Env InequalityOp VarId)
   ()
   ]

  )

(define-metafunction formality-ty
  ;; Given a variable `VarId_in` returns all known relations about `VarId_in`
  ;; (e.g., `VarId_in <= Parameter` etc).
  known-relations : Env_in VarId_in -> (Relation ...)
  #:pre (env-contains-unmapped-existential-var Env_in VarId_in)

  [(known-relations Env VarId)
   (Relation ... ...)
   (where/error (VarInequality ...) (env-inequalities Env))
   (where/error ((Relation ...) ...) ((filter-for-var VarInequality VarId) ...))
   ]

  )

(define-metafunction formality-ty
  ;; If `VarInequality` is a relation of `VarId`, return empty list.
  ;; Else return `(VarInequality)`.
  filter-for-var : VarInequality VarId -> (Relation ...)

  [(filter-for-var (VarId InequalityOp (Parameter ...)) VarId) ((VarId InequalityOp Parameter) ...)]
  [(filter-for-var VarInequality VarId) ()]
  )

(define-metafunction formality-ty
  ;; Removes `VarId_in` from the list of bounded variables in `Env_in` (assuming it is
  ;; present) and returns the resulting environment as well as whatever bounds `VarId_in`
  ;; had.
  ;;
  ;; `VarId_in` must be a variable declared in the environment and must not be unmapped.
  remove-var-bounds-from-env : Env_in VarId_in -> Env
  #:pre (env-contains-unmapped-existential-var Env_in VarId_in)

  [(remove-var-bounds-from-env Env VarId)
   (env-with-inequalities Env (flatten ((filter-not-for-var VarInequality VarId) ...)))
   (where/error (VarInequality ...) (env-inequalities Env))
   ]

  )

(define-metafunction formality-ty
  ;; If `VarInequality` is a relation of `VarId`, return empty list.
  ;; Else return `(VarInequality)`.
  filter-not-for-var : VarInequality VarId -> (VarInequality ...)

  [(filter-not-for-var (VarId InequalityOp _) VarId) ()]
  [(filter-not-for-var VarInequality VarId) (VarInequality)]
  )

(define-metafunction formality-ty
  ;; Extends the environment with additional bounds for `VarId_in` (has no effect if this bound
  ;; is already present).
  ;;
  ;; `VarId_in` must be a variable declared in the environment and must not be unmapped.
  env-with-var-related-to-parameters : Env_in VarId_in InequalityOp Parameters -> Env
  #:pre (env-contains-unmapped-existential-var Env_in VarId_in)

  [(env-with-var-related-to-parameters Env VarId InequalityOp ())
   Env]

  [(env-with-var-related-to-parameters Env VarId InequalityOp (Parameter_0 Parameter_r ...))
   (env-with-var-related-to-parameters Env_0 VarId InequalityOp (Parameter_r ...))
   (where/error Env_0 (env-with-var-related-to-parameter Env VarId InequalityOp Parameter_0))]

  )

(define-metafunction formality-ty
  ;; Extends the environment with an additional bound for `VarId_in` (has no effect if this bound
  ;; is already present).
  ;;
  ;; `VarId_in` must be a variable declared in the environment and must not be unmapped.
  env-with-var-related-to-parameter : Env_in VarId_in InequalityOp Parameter -> Env
  #:pre (env-contains-unmapped-existential-var Env_in VarId_in)

  [(env-with-var-related-to-parameter Env VarId InequalityOp Parameter)
   Env
   (where (VarInequality_0 ... (VarId InequalityOp Parameters_b) VarInequality_1 ...) (env-inequalities Env))
   (where #t (in? Parameter Parameters_b))
   ]

  [(env-with-var-related-to-parameter Env VarId InequalityOp Parameter)
   (env-with-inequalities Env (VarInequality_0 ... (VarId InequalityOp (Parameter Parameter_b ...)) VarInequality_1 ...))
   (where (VarInequality_0 ... (VarId InequalityOp (Parameter_b ...)) VarInequality_1 ...) (env-inequalities Env))
   (where #f (in? Parameter (Parameter_b ...)))
   ]

  [(env-with-var-related-to-parameter Env VarId InequalityOp Parameter)
   (env-with-inequalities Env ((VarId InequalityOp (Parameter)) VarInequality_0 ...))
   (where/error (VarInequality_0 ...) (env-inequalities Env))
   ]

  )
