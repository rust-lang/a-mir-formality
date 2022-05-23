#lang racket
(require redex
         "grammar.rkt"
         "../decl/grammar.rkt")
(provide (all-defined-out))

(define-extended-language formality-mir+Γ formality-mir
  ;; Extension of `Ty` to optionally store a `VariantId`.
  [PlaceTy ::= (place-ty Ty MaybeMut) (place-ty-variant Ty MaybeMut VariantId)]
  ;; Typing context storing bindings from locals to types and `CrateDecls`.
  [Γ ::= (LocalDecls CrateDecls)]
  )

(define-metafunction formality-mir+Γ
  ;; Returns the type of the local with the given `LocalId`.
  type-of-local : Γ LocalId -> PlaceTy

  [(type-of-local Γ LocalId)
   (place-ty Ty MaybeMut)
   (where/error (LocalDecls _) Γ)
   (where (_ ... (LocalId Ty MaybeMut) _ ...) LocalDecls)]
  )

(define-metafunction formality-mir+Γ
  ;; Returns the `AdtContents` of the ADT with the given `AdtId`.
  decl-of-adt : Γ AdtId -> AdtContents

  [(decl-of-adt Γ AdtId)
   AdtContents
   (where/error (_ CrateDecls) Γ)
   (where AdtContents (item-with-id CrateDecls AdtId))]
  )

(define-judgment-form
  formality-mir+Γ
  #:mode (types/place I I O)
  #:contract (types/place Γ Place PlaceTy)

  [(where PlaceTy (type-of-local Γ LocalId))
   ----------------------------------------- "local"
   (types/place Γ LocalId PlaceTy)]

  [(types/place Γ Place (place-ty (rigid-ty (ref _) (_ Ty)) MaybeMut))
   ------------------------------------------------------------------- "deref"
   (types/place Γ (* Place) (place-ty Ty MaybeMut))]

  [(types/place Γ Place (place-ty (rigid-ty AdtId _) MaybeMut))
   (where (struct _ _ ((_ FieldDecls))) (decl-of-adt Γ AdtId))
   (where (_ ... (FieldId Ty_field) _ ...) FieldDecls)
   ------------------------------------------------------------------- "field-struct"
   (types/place Γ (field  Place FieldId) (place-ty Ty_field MaybeMut))]

  [(types/place Γ Place (place-ty Ty_adt MaybeMut))
   (where (rigid-ty AdtId _) Ty_adt)
   (where (enum _ _ AdtVariants) (decl-of-adt Γ AdtId))
   (where (_ ... (VariantId _) _ ...) AdtVariants)
   --------------------------------------------------------------------------------------- "downcast"
   (types/place Γ (downcast Place VariantId) (place-ty-variant Ty_adt MaybeMut VariantId))]

  [(types/place Γ Place (place-ty-variant (rigid-ty AdtId _) MaybeMut VariantId))
   (where (enum _ _ AdtVariants) (decl-of-adt Γ AdtId))
   (where (_ ... (VariantId FieldDecls) _ ...) AdtVariants)
   (where (_ ... (FieldId Ty_field) _ ...) FieldDecls)
   ------------------------------------------------------------------------------ "field-enum"
   (types/place Γ (field  Place FieldId) (place-ty Ty_field MaybeMut))]
  )

(define-judgment-form
  formality-mir+Γ
  #:mode (types/operand I I O)
  #:contract (types/operand Γ Operand Ty)

  [(types/place Γ Place (place-ty Ty _))
   ------------------------------------- "copy"
   (types/operand Γ (copy Place) Ty)]

  [(types/place Γ Place (place-ty Ty _))
   ------------------------------------- "move"
   (types/operand Γ (move Place) Ty)]

  [------------------------------------------- "const"
   (types/operand Γ (const _) (scalar-ty i32))]
  )

(define-judgment-form
  formality-mir+Γ
  #:mode (types/rvalue I I O)
  #:contract (types/rvalue Γ Rvalue Ty)

  [(types/operand Γ Operand Ty)
   --------------------------------- "use"
   (types/rvalue Γ (use Operand) Ty)]

  [(types/place Γ Place (place-ty Ty _))
   -------------------------------------------------------------- "ref"
   (types/rvalue Γ (ref Lt () Place) (rigid-ty (ref ()) (Lt Ty)))]

  [(types/place Γ Place (place-ty Ty (mut)))
   -------------------------------------------------------------------- "ref-mut"
   (types/rvalue Γ (ref Lt (mut) Place) (rigid-ty (ref (mut)) (Lt Ty)))]

  [(types/operand Γ Operand_a (rigid-ty ScalarId_ty ()))
   (types/operand Γ Operand_b (rigid-ty ScalarId_ty ()))
   ------------------------------------------------------------------------- "binop"
   (types/rvalue Γ (BinaryOp Operand_a Operand_b) (rigid-ty ScalarId_ty ()))]
  )

(define-judgment-form
  formality-mir+Γ
  #:mode (types/stmt I I O)
  #:contract (types/stmt Γ Statement Γ)

  [--------------------- "noop"
   (types/stmt Γ noop Γ)]

  [----------------------------- "storage-live"
   (types/stmt Γ storage-live Γ)]

  [----------------------------- "storage-dead"
   (types/stmt Γ storage-dead Γ)]

  [(types/rvalue Γ Rvalue Ty)
   (types/place Γ Place (place-ty Ty (mut)))
   ----------------------------------------- "assign"
   (types/stmt Γ (assign Place Rvalue) Γ)]

  [(types/place Γ Place (place-ty (rigid-ty AdtId _) (mut)))
   (where (enum _ _ AdtVariants) (decl-of-adt Γ AdtId))
   (where (_ ... (VariantId _) _ ...) AdtVariants)
   --------------------------------------------------------- "set-discr"
   (types/stmt Γ (set-discriminant Place VariantId) Γ)]
  )

(define-judgment-form
  formality-mir+Γ
  #:mode (types/term I I O)
  #:contract (types/term Γ Terminator Γ)

  [------------------------- "goto"
   (types/term Γ (goto _) Γ)]

  [----------------------- "resume"
   (types/term Γ resume Γ)]

  [---------------------- "abort"
   (types/term Γ abort Γ)]

  [----------------------- "return"
   (types/term Γ return Γ)]

  [---------------------------- "unreachable"
   (types/term Γ unreachable Γ)]

  [(types/place Γ Place _)
   ------------------------------- "drop"
   (types/term Γ (drop Place _) Γ)]

  [(types/place Γ Place (place-ty Ty _))
   (types/operand Γ Operand Ty)
   --------------------------------------------------- "drop-replace"
   (types/term Γ (drop-and-replace Place Operand _) Γ)]

  [(types/operand Γ Operand_fn (rigid-ty (fn-ptr _ _) (Ty_arg ..._n Ty_ret)))
   (types/operand Γ Operand_arg Ty_arg) ...
   (types/place Γ Place (place-ty Ty_ret (mut)))
   -------------------------------------------------------------- "call"
   (types/term Γ (call Operand_fn (Operand_arg ..._n) Place _) Γ)]
  )

(define-judgment-form
  formality-mir+Γ
  #:mode (types/block I I O)
  #:contract (types/block Γ BasicBlockData Γ)

  [(types/term Γ_0 Terminator Γ_1)
   ------------------------------------- "block-base"
   (types/block Γ_0 (() Terminator) Γ_1)]

  [(types/stmt Γ_0 Statement_hd Γ_1)
   (types/block Γ_1 ((Statement_tl ...) Terminator) Γ_2)
   ------------------------------------------------------------------ "block-step"
   (types/block Γ_0 ((Statement_hd Statement_tl ...) Terminator) Γ_2)]
  )

(module+ test
  (redex-let*
   formality-mir+Γ

   ((; struct Foo { counter: i32 }
     AdtDecl_Foo (term (Foo (struct () () ((struct-variant ((counter (scalar-ty i32)))))))))
    (Ty_Foo (term (rigid-ty Foo ())))

    (; enum Bar { Baz { counter: i32 } }
     AdtDecl_Bar (term (Bar (enum () () ((Baz ((counter (scalar-ty i32)))))))))
    (Ty_Bar (term (rigid-ty Bar ())))

    (; crate TheCrate { ... }
     CrateDecl (term (TheCrate (crate (AdtDecl_Foo AdtDecl_Bar)))))
    
    ; let foo: Foo; let bar: Bar;
    (Γ (term (((foo Ty_Foo ()) (bar Ty_Bar ())) (CrateDecl))))
    )

    (test-equal
      (judgment-holds
        (types/operand Γ
                       (const 42)
                       Ty)
        Ty)
      (list (term (scalar-ty i32))))

    (test-equal
      (judgment-holds
        (types/place Γ
                     foo
                     (place-ty Ty ()))
        Ty)
      (list (term (rigid-ty Foo ()))))

    (test-equal
      (judgment-holds
        (types/place Γ
                    (field foo counter)
                    (place-ty Ty ()))
        Ty)
      (list (term (scalar-ty i32))))

    (test-equal
      (judgment-holds
        (types/place Γ
                    (field (downcast bar Baz) counter)
                    (place-ty Ty ()))
        Ty)
      (list (term (scalar-ty i32))))

    (test-equal
      (judgment-holds
        (types/rvalue Γ
                      (+ (const 1) (const 2))
                      Ty)
        Ty)
      (list (term (scalar-ty i32))))
  )
)
