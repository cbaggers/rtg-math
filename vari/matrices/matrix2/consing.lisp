(in-package :rtg-math.matrix2)

;; All matrices are stored in column-major format, but when you
;; write them (like in m!) then you write them in row-major format.

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 0! () "mat2(0.0)" () :mat2 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 make (a b c d)
 "mat2(~a,~a,~a,~a)"
 (:float :float :float :float)
 :mat2 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 copy-mat2 (v) "mat4(~a)" (:mat2) :mat2)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 identity () "mat2(1.0)" () :mat2 :pure t)

;;----------------------------------------------------------------

(varjo:v-defun from-rows ((row-1 :vec2) (row-2 :vec2))
  (make (x row-1) (y row-1)
        (x row-2) (y row-2)))

;;----------------------------------------------------------------

(varjo:v-defun get-rows ((mat-a :mat2))
  (values (v2:make (melm mat-a 0 0)
                   (melm mat-a 0 1))
          (v2:make (melm mat-a 1 0)
                   (melm mat-a 1 1))))

;;----------------------------------------------------------------

(varjo:v-defun get-row ((mat-a :mat2) (row-num :int))
  (v2:make (melm mat-a row-num 0)
           (melm mat-a row-num 1)))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 from-columns (a b) "mat2(~a,~a)" (:vec2 :vec2) :mat2 :pure t)

;;----------------------------------------------------------------

(varjo:v-defun get-columns ((mat-a :mat2))
  (values (get-column mat-a 0)
          (get-column mat-a 1)))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 get-column (a b) "~a[~a]" (:mat2 :int) :vec2 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 0p (a) "(~a==0)" (:mat2) :bool :pure t)

;;----------------------------------------------------------------

(varjo:v-defun identityp ((mat-a :mat2))
  (and (cl:= mat-a (identity))))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 = (a b) "(~a==~a)" (:mat2 :mat2) :bool :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 transpose (a) "transpose(~a)" (:mat2) :mat2 :pure t)

;;----------------------------------------------------------------

(varjo:v-defun adjoint ((mat-a :mat2))
  (make
   (melm mat-to-mutate 1 1) (cl:- (melm mat-to-mutate 1 0))
   (cl:- (melm mat-to-mutate 0 1)) (melm mat-to-mutate 0 0)))

;;----------------------------------------------------------------

(varjo:v-defun trace ((mat-a :mat3))
  (cl:+ (melm mat-a 0 0) (melm mat-a 1 1) (melm mat-a 2 2)))

;;----------------------------------------------------------------

(varjo:v-defun rotation-from-euler ((angle :float))
  (let ((sa (sin angle))
        (ca (cos angle)))
    (make ca sa
          (cl:- sa) ca)))

;;----------------------------------------------------------------

(varjo:v-defun scale ((scale-vec2 :vec2))
  (make (x scale-vec2)  0.0
        0.0             (y scale-vec2)))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun + (a b c &rest c) "(~a £+£ ~a £+£ ~a ~{ £+£ ~a~})"
                               (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro + ((a t) (b t) (c t) &rest (d t))
  `(+ ,a (+ ,b (+ ,c ,@d))))

(varjo:v-def-glsl-template-fun + (a) "~a" (:mat2) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:mat2 :mat2) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun - (a b c &rest c) "(~a £-£ ~a £-£ ~a ~{ £-£ ~a~})"
                               (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro - ((a t) (b t) (c t) &rest (d t))
  `(- ,a (- ,b (- ,c ,@d))))

(varjo:v-def-glsl-template-fun - (a) "(-~a)" (:mat2) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:mat2 :mat2) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 negate (a) "(-~a)" (:mat2) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 *v (a b) "(~a * ~a)" (:mat2 :vec2) :vec2 :pure t)

;;----------------------------------------------------------------

(varjo:v-defun mrow*vec2 ((vec :vec2) (mat-a :mat2))
  (make (cl:+ (cl:* (x vec2-to-mutate) (melm mat-a 0 0))
              (cl:* (y vec2-to-mutate) (melm mat-a 1 0)))
        (cl:+ (cl:* (x vec2-to-mutate) (melm mat-a 0 1))
              (cl:* (y vec2-to-mutate) (melm mat-a 1 1)))))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun * (a b c &rest c) "(~a £*£ ~a £*£ ~a ~{ £*£ ~a~})"
                               (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro * ((a t) (b t) (c t) &rest (d t))
  `(* ,a (* ,b (* ,c ,@d))))

(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:mat3 :mat3) :mat3 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 *s (v s) "(~a * ~a)" (:mat3 :float) :mat3 :pure t)

;;----------------------------------------------------------------
