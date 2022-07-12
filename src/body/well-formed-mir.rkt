#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
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
  formality-body
  #:mode (well-formed/Γ I)
  #:contract (well-formed/Γ Γ)

  [(where/error (LocalDecls (BasicBlockDecl ...)) (locals-and-blocks-of-Γ Γ))
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
  formality-body
  #:mode (well-formed/BasicBlockDecl I I)
  #:contract (well-formed/BasicBlockDecl Γ BasicBlockDecl)

  [(well-formed/Statement Γ Statement) ...
   (well-formed/Terminator Γ Terminator)
   ----------------------------------------
   (well-formed/BasicBlockDecl Γ (_ ((Statement ...) Terminator)))
   ]
  )

(define-judgment-form
  formality-body
  #:mode (well-formed/Statement I I)
  #:contract (well-formed/Statement Γ Statement)

  [----------------------------------------
   (well-formed/Statement Γ noop)
   ]

  [(well-formed/Place Γ Place)
   (well-formed/Rvalue Γ Rvalue)
   ----------------------------------------
   (well-formed/Statement Γ (Place = Rvalue))
   ]

  )

(define-judgment-form
  formality-body
  #:mode (well-formed/Place I I)
  #:contract (well-formed/Place Γ Place)

  [(well-formed/LocalId Γ LocalId)
   ----------------------------------------
   (well-formed/Place Γ LocalId)
   ]

  )

(define-judgment-form
  formality-body
  #:mode (well-formed/Rvalue I I)
  #:contract (well-formed/Rvalue Γ Rvalue)

  [(well-formed/Operand Γ Operand)
   ----------------------------------------
   (well-formed/Rvalue Γ (use Operand))
   ]

  )

(define-judgment-form
  formality-body
  #:mode (well-formed/Operand I I)
  #:contract (well-formed/Operand Γ Operand)

  [(well-formed/Place Γ Place)
   ----------------------------------------
   (well-formed/Operand Γ (copy Place))
   ]

  [(well-formed/Place Γ Place)
   ----------------------------------------
   (well-formed/Operand Γ (move Place))
   ]

  [----------------------------------------
   (well-formed/Operand Γ (const number))
   ]

  )

(define-judgment-form
  formality-body
  #:mode (well-formed/LocalId I I)
  #:contract (well-formed/LocalId Γ LocalId)

  [(where (_ ... (LocalId _ _) _ ...) (local-decls-of-Γ Γ))
   ----------------------------------------
   (well-formed/LocalId Γ LocalId)
   ]

  )

(define-judgment-form
  formality-body
  #:mode (well-formed/Terminator I I)
  #:contract (well-formed/Terminator Γ Terminator)

  [(well-formed/BasicBlockId Γ BasicBlockId)
   ----------------------------------------
   (well-formed/Terminator Γ (goto BasicBlockId))
   ]

  [----------------------------------------
   (well-formed/Terminator Γ return)
   ]

  [----------------------------------------
   (well-formed/Terminator Γ resume)
   ]

  )

(define-judgment-form
  formality-body
  #:mode (well-formed/BasicBlockId I I)
  #:contract (well-formed/BasicBlockId Γ BasicBlockId)

  [(where (_ ... (BasicBlockId _) _ ...) (basic-block-decls-of-Γ Γ))
   ----------------------------------------
   (well-formed/BasicBlockId Γ BasicBlockId)
   ]

  )

