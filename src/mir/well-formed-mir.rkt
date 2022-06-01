#lang racket
(require redex/reduction-semantics
         "grammar-extended.rkt"
         "type-check-goal.rkt"
         )
(provide well-formed/Γ
         )

(;; well-formed/MirBody -- basic syntactical checks and criteria
 ;;
 ;; Checks:
 ;;
 ;; * Each basic block name is distinct
 ;; * All references to basic blocks are to a valid, defined name
 ;; * Each local variable name is distinct
 ;; * All references to local variables are to a valid, defined name
 define-judgment-form
  formality-mir-extended
  #:mode (well-formed/Γ I)
  #:contract (well-formed/Γ Γ)

  [(where/error (LocalDecls (BasicBlockDecl ...)) (mir-body-of-Γ Γ))
   (; each basic block id is distinct
    where ((BasicBlockId_!_1 _) ...) (BasicBlockDecl ...))

   (; each local id is distinct
    where ((LocalId_!_1 _ _) ...) LocalDecls)

   (well-formed/BasicBlockDecl Γ BasicBlockDecl) ...
   ----------------------------------------
   (well-formed/Γ Γ)
   ]
  )

(define-judgment-form
  formality-mir-extended
  #:mode (well-formed/BasicBlockDecl I I)
  #:contract (well-formed/BasicBlockDecl Γ BasicBlockDecl)

  [(well-formed/Statement Γ Statement) ...
   (well-formed/Terminator Γ Terminator)
   ----------------------------------------
   (well-formed/BasicBlockDecl Γ (_ ((Statement ...) Terminator)))
   ]
  )

(define-judgment-form
  formality-mir-extended
  #:mode (well-formed/Statement I I)
  #:contract (well-formed/Statement Γ Statement)

  [----------------------------------------
   (well-formed/Statement Γ noop)
   ]

  )

(define-judgment-form
  formality-mir-extended
  #:mode (well-formed/LocalId I I)
  #:contract (well-formed/LocalId Γ Statement)

  [(where (_ ... (LocalId _ _) _ ...) (local-decls-of-Γ Γ))
   ----------------------------------------
   (well-formed/LocalId Γ LocalId)
   ]

  )

(define-judgment-form
  formality-mir-extended
  #:mode (well-formed/Terminator I I)
  #:contract (well-formed/Terminator Γ Terminator)

  [(well-formed/BasicBlockId Γ BasicBlockId)
   ----------------------------------------
   (well-formed/Terminator Γ (goto BasicBlockId))
   ]

  )

(define-judgment-form
  formality-mir-extended
  #:mode (well-formed/BasicBlockId I I)
  #:contract (well-formed/BasicBlockId Γ BasicBlockId)

  [(where (_ ... (BasicBlockId _) _ ...) (basic-block-decls-of-Γ Γ))
   ----------------------------------------
   (well-formed/BasicBlockId Γ BasicBlockId)
   ]

  )

