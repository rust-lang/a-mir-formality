#lang racket
(require redex/reduction-semantics
         "../locations.rkt"
         "../grammar.rkt"
         "statement.rkt"
         "terminator.rkt"
         )
(provide type-check-goal/BasicBlockDecl
         )

;; Check the body of a basic blofk and generate a series of goals that must be
;; proven by the type checker. Each goal is tagged with a location where it must
;; hold. These locations are needed by polonius.

(define-judgment-form
  formality-body
  #:mode (type-check-goal/BasicBlockDecl I I O)
  #:contract (type-check-goal/BasicBlockDecl Γ BasicBlockDecl GoalAtLocations)

  [(where/error (((Location_s Statement) ...) (Location_t Terminator)) (basic-block-locations BasicBlockDecl))
   (type-check-goal/Statement Γ Statement Goals_s) ...
   (type-check-goal/Terminator Γ Terminator Goals_t)
   ----------------------------------------
   (type-check-goal/BasicBlockDecl Γ
                                   BasicBlockDecl
                                   [(Location_s (all-goal Goals_s)) ... 
                                   (Location_t (all-goal Goals_t))])
   ]
  )

(define-metafunction formality-body
all-goal : Goals -> Goal

[(all-goal [Goal]) Goal]
[(all-goal Goals) (&& Goals)]
)






