#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "substitution.rkt"
         "env.rkt"
         "../util.rkt"
         )

(provide match-terms
)

(define-metafunction formality-logic
  ;; *matching terms* is a very simple minded form of unification
  ;; used in environment elaboration:
  ;;
  ;; It returns a substitution Θ that replaces any instance of `VarIds_m` in `Term_l`
  ;; with the corresponding term from `Term_r` such that `Θ Term_l == Term_r` (or Error
  ;; if no such substitution exists).
  ;;
  ;; Assumes that `VarIds_m` do not appear free in `Term_r`, only in `Term_l`.
  match-terms : VarIds_m Term_l Term_r -> Substitution-or-error_out

  #:pre (not? (any-appears-free VarIds_m Term_r))
  #:post (match-terms-postcondition-satisfied? VarIds_m Term_l Term_r Substitution-or-error_out)

  [(match-terms VarIds_m Term_l Term_r)
   (match-pairs VarIds_m () ((Term_l Term_r)))]

  )

(define-metafunction formality-logic
  match-terms-postcondition-satisfied? : VarIds_m Term_l Term_r Substitution-or-error_out -> boolean

  #:pre (not? (any-appears-free VarIds_m Term_r))

  [(match-terms-postcondition-satisfied? VarIds_m Term_l Term_r Error) #t]

  [(match-terms-postcondition-satisfied? VarIds_m Term_l Term_r Substitution)
   #t
   (; makes `Term_l` equal to `Term_r` (which also implies that `VarIds_m` do not appear in the
    ; values, at least if they are used)
    where Term_r (apply-substitution Substitution Term_l))
   (; only maps variables in `VarIds_m`
    where () (substitution-without-vars Substitution VarIds_m))
   ]

  [(match-terms-postcondition-satisfied? VarIds_m Term_l Term_r Substitution) #f]
  )

(define-metafunction formality-logic
  match-pairs : VarIds_m Substitution TermPairs -> Substitution or Error

  [; base case, all done, but we have to apply the substitution to itself
   (match-pairs VarIds_m Substitution ())
   (substitution-fix Substitution)
   ]

  [; unify the first pair ===> if that is an error, fail
   (match-pairs VarIds_m Substitution (TermPair_first TermPair_rest ...))
   Error
   (where Error (match-pair VarIds_m TermPair_first))]

  [; unify the first pair ===> if that succeeds, apply resulting substitution to the rest
   ; and recurse
   (match-pairs VarIds_m Substitution_in (TermPair_1st (Term_rest-l Term_rest-r) ...))
   (match-pairs VarIds_m Substitution_out TermPairs_new)

   (where (Substitution_from-1st (TermPair_from-1st ...)) (match-pair VarIds_m TermPair_1st))
   (where/error Substitution_out (substitution-concat-disjoint Substitution_in Substitution_from-1st))
   (where/error (Term_rest-l-subst ...) (apply-substitution Substitution_out (Term_rest-l ...)))
   (where/error TermPairs_new (TermPair_from-1st ... (Term_rest-l-subst Term_rest-r) ...))
   ]
  )

(define-metafunction formality-logic
  match-pair : VarIds_m TermPair -> (Substitution TermPairs) or Error

  [; Term = Term ===> always ok
   (match-pair _ (Term Term))
   (() ())
   ]

  [; ?X = Parameter ===> map ?X to Parameter
   (match-pair VarIds_m (VarId Parameter))
   (((VarId Parameter)) ())

   (where #t (in?/id VarId VarIds_m))
   ]

  [; (L ...) = (R ...) ===> true if Li = Ri for all i and the lengths are the same
   (match-pair VarIds_m ((Term_l ..._0) (Term_r ..._0)))
   (() ((Term_l Term_r) ...))]

  [; any other case fails to unify
   (match-pair VarIds_m (Term_1 Term_2))
   Error
   ]

  )

(module+ test

  (traced '()
          (test-equal
           (term (match-terms (X) (Implemented (TraitRef (X))) (Implemented (TraitRef (u32)))))
           (term ((X u32)))
           )
          )

  (traced '()
          (test-equal
           (term (match-terms (X) (Implemented (TraitRef (X))) (HasImpl (TraitRef (u32)))))
           (term Error)
           )
          )

  )