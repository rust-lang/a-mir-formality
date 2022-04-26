#lang racket
(require redex
         "grammar.rkt"
         "../ty/grammar.rkt")
(provide (all-defined-out))

(define-extended-language formality-mir+Γ formality-mir
  [Γ · (local-variable-id : Ty Γ)])

(define-judgment-form
  formality-mir+Γ
  #:mode (types I I O)
  #:contract (types Γ any Ty)

  ; axioms
  [-------------------------------
   (types Γ (nullary-op Ty_t) Ty_t)]

  [---------------------
   (types (local-variable-id_v : Ty_t Γ) local-variable-id_v Ty_t)]
  
  [--------------------
   (types Γ number (TyRigid i32 ()))]

  ; places
  [(types Γ local-variable-id_v Ty_t)
   --------------------
   (types Γ (place (local-variable-id_v (projections ()))) Ty_t)]

  [(types Γ (place (local-variable-id (projections (projection_ps ...)))) (TyRigid (Ref _) (_ Ty)))
   ------------------------------------------------------------------------------------------------
   (types Γ (place (local-variable-id (projections (projection-deref projection_ps ...)))) Ty)]

 ;[(types Γ (place (local-variable-id_v (projections (projection_ps ...)))) (TyRigid AdtId Parameters))
 ; ( field with id field-id of ADT with id AdtId has type ty )
 ; --------------------
 ; (types Γ (place (local-variable-id_v (projections ((projection-field field-id_f) projection_ps ...)))) Ty_t)]

  ; operands
  [(types Γ place_p Ty_t)
    --------------------
   (types Γ (operand-copy place_p) Ty_t)]

  [(types Γ place_p Ty_t)
    --------------------
   (types Γ (operand-move place_p) Ty_t)]

  [(types Γ constant Ty)
    --------------------
   (types Γ (operand-constant constant) Ty)]

  ; rvalues
  [(types Γ place Ty)
   ------------------
   (types Γ (rvalue-ref lifetime mutability place) (TyRigid (Ref mutability) (lifetime Ty)))]

  [(types Γ operand_a (TyRigid ScalarId_ty ()))
   (types Γ operand_b (TyRigid ScalarId_ty ()))
   -------------------------------
   (types Γ (rvalue-binary-op _ operand_a operand_b) (TyRigid ScalarId_ty ()))]
)

(define-judgment-form
  formality-mir+Γ
  #:mode (types-ok I I O)
  #:contract (types-ok Γ any Γ)

  ; blocks
  [(types-ok Γ_0 statements Γ_1)
   (types-ok Γ_1 terminator Γ_2)
   -------------------------------------------------------
   (types-ok Γ_0 (basic-block-data (statements terminator)) Γ_2)]

  [------------------------------
   (types-ok Γ (statements ()) Γ)]

  [(types-ok Γ_0 statement_hd Γ_1)
   (types-ok Γ_1 (statements (statement_tl ...)) Γ_2)
   -------------------------------------------------------------
   (types-ok Γ_0 (statements (statement_hd statement_tl ...)) Γ_2)]

  ; statements
  [------------------------------
   (types-ok Γ (statement-nop) Γ)]

  [(types Γ rvalue Ty)
   (types Γ place Ty)
   ----------------------------------------------
   (types-ok Γ (statement-assign place rvalue) Γ)]

  ;[(types Γ rvalue Ty)
  ;-------------------------------------------------------------------------------------------------
  ; (types-ok Γ (statement-assign (place (local-variable-id ())) rvalue) (local-variable-id : Ty Γ))]

   ; terminators
  [(types Γ operand_fn (TyRigid (Fn _ _) (Ty_arg ..._n Ty_ret)))
   (types Γ operand_arg Ty_arg) ...
   (types Γ place Ty_ret)
    ----------------------------------------------------------------
   (types-ok Γ (terminator-call operand_fn (operand_arg ..._n) place _) Γ)]
)
