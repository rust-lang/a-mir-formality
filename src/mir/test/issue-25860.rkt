#lang racket
(require redex/reduction-semantics
         "../all-check.rkt"
         "../grammar.rkt"
         "../../ty/grammar.rkt"
         "../../ty/user-ty.rkt"
         "../../util.rkt"
         )

(module+ test
  ;; Program:
  ;;
  ;; ```
  ;; static UNIT: &'static &'static () = &&();
  ;; fn foo<'a, 'b, T>(_: &'a &'b (), v: &'b T) -> &'a T {
  ;;     v
  ;; }
  ;; fn bad<'a, T>(x: &'a T) -> &'static T {
  ;;     let f: fn(_, &'a T) -> &'static T = foo;
  ;;     f(UNIT, x)
  ;; }
  ;; fn main() {
  ;;     let value = {
  ;;         let data = 22;
  ;;         bad(&data)
  ;;     };
  ;;     let _ = *value;
  ;; }
  ;; ```

  (redex-let*
   formality-mir

   [
    ;; ```
    ;; static UNIT: &'static &'static () = &&();
    ;; { // MIR (actually, the real MIR is "promoted", but this is close enough for our purposes)
    ;;     let mut _0: &&();                    // return place in scope 0 at src/main.rs:1:37: 1:41
    ;;     let mut _1: &();                     // in scope 0 at src/main.rs:1:38: 1:41
    ;;     let mut _2: ();                      // in scope 0 at src/main.rs:1:39: 1:41
    ;;
    ;;     bb0: {
    ;;         nop;                             // scope 0 at src/main.rs:1:39: 1:41
    ;;         _1 = &_2;                        // scope 0 at src/main.rs:1:38: 1:41
    ;;         _0 = &_1;                        // scope 0 at src/main.rs:1:37: 1:41
    ;;         return;                          // scope 0 at src/main.rs:1:37: 1:41
    ;;     }
    ;; }
    ;; ```
    (StaticDecl_UNIT (term (static UNIT
                                   () ; generics
                                   () ; where-clauses
                                   (user-ty (& static (& static ()))) ; type
                                   (∃
                                    ((lifetime L0)
                                     (lifetime L1)
                                     (lifetime L2)
                                     (lifetime L3)
                                     (lifetime L4)
                                     )
                                    ([(_0 (user-ty (& L0 (& L1 ()))) mut)
                                      (_1 (user-ty (& L2 ())) mut)
                                      (_2 (user-ty ()) mut)
                                      ]
                                     [(bb0 {(noop
                                             (_1 = (ref L3 () _2))
                                             (_0 = (ref L4 () _1)))
                                            return
                                            })]
                                     )
                                    )
                                   )))
    ]

   (traced '()
           (test-equal "FIXME" "FIXME"))
   )
  )

;; ```
;; fn foo<'a, 'b, T>(_: &'a &'b (), v: &'b T) -> &'a T {
;;     v
;; }
;; MIR {
;;     debug v => _2;                       // in scope 0 at src/main.rs:3:34: 3:35
;;     let mut _0: &T;                      // return place in scope 0 at src/main.rs:3:47: 3:52
;;
;;     bb0: {
;;         _0 = _2;                         // scope 0 at src/main.rs:4:5: 4:6
;;         return;                          // scope 0 at src/main.rs:5:2: 5:2
;;     }
;; }
;;
;; fn bad<'a, T>(x: &'a T) -> &'static T {
;;     let f: fn(_, &'a T) -> &'static T = foo;
;;     f(UNIT, x)
;; }
;; MIR {
;;     debug x => _1;                       // in scope 0 at src/main.rs:7:15: 7:16
;;     let mut _0: &T;                      // return place in scope 0 at src/main.rs:7:28: 7:38
;;     let _2: fn(&&(), &T) -> &T;          // in scope 0 at src/main.rs:8:9: 8:10
;;     let mut _3: fn(&&(), &T) -> &T;      // in scope 0 at src/main.rs:9:5: 9:6
;;     let mut _4: &&();                    // in scope 0 at src/main.rs:9:7: 9:11
;;     let _5: &&&();                       // in scope 0 at src/main.rs:9:7: 9:11
;;     let mut _6: &T;                      // in scope 0 at src/main.rs:9:13: 9:14
;;     scope 1 {
;;         debug f => _2;                   // in scope 1 at src/main.rs:8:9: 8:10
;;     }
;;
;;     bb0: {
;;         _2 = foo::<T> as for<'a, 'b> fn(&'a &'b (), &'b T) -> &'a T (Pointer(ReifyFnPointer)); // scope 0 at src/main.rs:8:41: 8:44
;;                                          // mir::Constant
;;                                          // + span: src/main.rs:8:41: 8:44
;;                                          // + literal: Const { ty: for<'a, 'b> fn(&'a &'b (), &'b T) -> &'a T {foo::<T>}, val: Value(Scalar(<ZST>)) }
;;         _3 = _2;                         // scope 1 at src/main.rs:9:5: 9:6
;;         _5 = const {alloc1: &&&()};      // scope 1 at src/main.rs:9:7: 9:11
;;                                          // mir::Constant
;;                                          // + span: src/main.rs:9:7: 9:11
;;                                          // + literal: Const { ty: &&&(), val: Value(Scalar(alloc1)) }
;;         _4 = (*_5);                      // scope 1 at src/main.rs:9:7: 9:11
;;         _6 = _1;                         // scope 1 at src/main.rs:9:13: 9:14
;;         _0 = move _3(move _4, move _6) -> bb1; // scope 1 at src/main.rs:9:5: 9:15
;;     }
;;
;;     bb1: {
;;         return;                          // scope 0 at src/main.rs:10:2: 10:2
;;     }
;; }
;;
;; fn main() {
;;     let value = {
;;         let data = 22;
;;         bad(&data)
;;     };
;;     let _ = *value;
;; }
;; MIR {
;;     let mut _0: ();                      // return place in scope 0 at src/main.rs:12:11: 12:11
;;     let _1: &i32;                        // in scope 0 at src/main.rs:13:9: 13:14
;;     let _2: i32;                         // in scope 0 at src/main.rs:14:13: 14:17
;;     let mut _3: &i32;                    // in scope 0 at src/main.rs:15:13: 15:18
;;     let _4: &i32;                        // in scope 0 at src/main.rs:15:13: 15:18
;;     scope 1 {
;;         debug value => _1;               // in scope 1 at src/main.rs:13:9: 13:14
;;         scope 3 {
;;         }
;;     }
;;     scope 2 {
;;         debug data => _2;                // in scope 2 at src/main.rs:14:13: 14:17
;;     }
;;
;;     bb0: {
;;         _2 = const 22_i32;               // scope 0 at src/main.rs:14:20: 14:22
;;         _4 = &_2;                        // scope 2 at src/main.rs:15:13: 15:18
;;         _3 = _4;                         // scope 2 at src/main.rs:15:13: 15:18
;;         _1 = bad::<i32>(move _3) -> bb1; // scope 2 at src/main.rs:15:9: 15:19
;;                                          // mir::Constant
;;                                          // + span: src/main.rs:15:9: 15:12
;;                                          // + literal: Const { ty: for<'a> fn(&'a i32) -> &'static i32 {bad::<i32>}, val: Value(Scalar(<ZST>)) }
;;     }
;;
;;     bb1: {
;;         return;                          // scope 0 at src/main.rs:18:2: 18:2
;;     }
;; }
;; ```
