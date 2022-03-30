#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "env.rkt"
         )
(provide inequalities-without-vars
         )

(define-metafunction formality-logic
  ;; Filters out any inequalities related to variables in `VarIds`.
  inequalities-without-vars : VarInequalities VarIds -> VarInequalities

  [(inequalities-without-vars () VarIds)
   ()]

  [(inequalities-without-vars ((_ <= VarId <= _) VarInequality ...) VarIds)
   (inequalities-without-vars (VarInequality ...) VarIds)
   (where #t (in?/id VarId VarIds))
   ]

  [(inequalities-without-vars (VarInequality VarInequality_rest ...) VarIds)
   (VarInequality VarInequality_rest1 ...)
   (where/error (VarInequality_rest1 ...) (inequalities-without-vars (VarInequality_rest ...) VarIds))
   ]
  )

(module+ test
  (redex-let*
   formality-logic

   [
    (VarInequalities (term ((() <= X <= ()) (() <= Y <= ()))))
    ]

   (test-equal
    (term (inequalities-without-vars VarInequalities (Z)))
    (term ((() <= X <= ()) (() <= Y <= ())))
    )

   (test-equal
    (term (inequalities-without-vars VarInequalities (X Y)))
    (term ())
    )

   (test-equal
    (term (inequalities-without-vars VarInequalities (X)))
    (term ((() <= Y <= ())))
    )

   (test-equal
    (term (inequalities-without-vars VarInequalities (Y)))
    (term ((() <= X <= ())))
    )
   )
  )