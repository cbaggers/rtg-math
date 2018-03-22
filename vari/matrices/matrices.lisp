(in-package :rtg-math.matrices)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 0p (a) "(~a==0)" (vari:v-matrix) :bool :pure t)

;;----------------------------------------------------------------


;; (defun unitp (matrix)
;;   (let ((len (cl:length matrix)))
;;     (cond
;;       ((cl:= len 9)
;;        (null (mismatch matrix (m3:identity))))
;;       ((cl:= len 16)
;;        (null (mismatch matrix (m4:identity)))))))


;;----------------------------------------------------------------


(varjo:v-def-glsl-template-fun
 = (a b) "(~a==~a)" (:mat2 :mat2) :bool :pure t)
(varjo:v-def-glsl-template-fun
 = (a b) "(~a==~a)" (:mat3 :mat3) :bool :pure t)
(varjo:v-def-glsl-template-fun
 = (a b) "(~a==~a)" (:mat4 :mat4) :bool :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 /= (a b) "(~a!=~a)" (:mat2 :mat2) :bool :pure t)
(varjo:v-def-glsl-template-fun
 /= (a b) "(~a!=~a)" (:mat3 :mat3) :bool :pure t)
(varjo:v-def-glsl-template-fun
 /= (a b) "(~a!=~a)" (:mat4 :mat4) :bool :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun + (a b c &rest c) "(~a £+£ ~a £+£ ~a ~{ £+£ ~a~})"
                               (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro + ((a t) (b t) (c t) &rest (d t))
  `(+ ,a (+ ,b (+ ,c ,@d))))

(varjo:v-def-glsl-template-fun + (a) "~a" (:mat2) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:mat2 :mat2) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a) "~a" (:mat3) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:mat3 :mat3) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a) "~a" (:mat4) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:mat4 :mat4) 0 :pure t)

;;----------------------------------------------------------------


(varjo:v-def-glsl-template-fun - (a b c &rest c) "(~a £-£ ~a £-£ ~a ~{ £-£ ~a~})"
                               (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro - ((a t) (b t) (c t) &rest (d t))
  `(- ,a (- ,b (- ,c ,@d))))

(varjo:v-def-glsl-template-fun - (a) "(-~a)" (:mat2) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:mat2 :mat2) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a) "(-~a)" (:mat3) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:mat3 :mat3) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a) "(-~a)" (:mat4) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:mat4 :mat4) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 elt (m r c) "~a[~a, ~a]" (vari:v-matrix :int :int)
 :float :v-place-index 0 :pure t)

(varjo:v-def-glsl-template-fun
 elm (m r c) "~a[~a, ~a]" (vari:v-matrix :int :int)
 :float :v-place-index 0 :pure t)

;;----------------------------------------------------------------


(vari:v-defun get-rows ((m :mat2))
  (m2:get-rows m))

(vari:v-defun get-rows ((m :mat3))
  (m3:get-rows m))

(vari:v-defun get-rows ((m :mat4))
  (m4:get-rows m))


;;----------------------------------------------------------------

(vari:v-defun get-columns ((m :mat2))
  (m2:get-columns m))

(vari:v-defun get-columns ((m :mat3))
  (m3:get-columns m))

(vari:v-defun get-columns ((m :mat4))
  (m4:get-columns m))

;;----------------------------------------------------------------


(vari:v-defun get-row ((m :mat2))
  (m2:get-row m))

(vari:v-defun get-row ((m :mat3))
  (m3:get-row m))

(vari:v-defun get-row ((m :mat4))
  (m4:get-row m))


;;----------------------------------------------------------------


(vari:v-defun get-column ((m :mat2))
  (m2:get-column m))

(vari:v-defun get-column ((m :mat3))
  (m3:get-column m))

(vari:v-defun get-column ((m :mat4))
  (m4:get-column m))


;;----------------------------------------------------------------

(vari:v-defun determinant ((m :mat3))
  (m3:determinant m))

(vari:v-defun determinant ((m :mat4))
  (m4:determinant m))

;;----------------------------------------------------------------

(vari:v-defun affine-inverse ((m :mat3))
  (m3:affine-inverse m))

(vari:v-defun affine-inverse ((m :mat4))
  (m4:affine-inverse m))

;;----------------------------------------------------------------

(vari:v-defun transpose ((m :mat2))
  (m2:transpose m))

(vari:v-defun transpose ((m :mat3))
  (m3:transpose m))

(vari:v-defun transpose ((m :mat4))
  (m4:transpose m))

;;----------------------------------------------------------------


(vari:v-defun trace ((m :mat2))
  (m2:trace m))

(vari:v-defun trace ((m :mat3))
  (m3:trace m))

(vari:v-defun trace ((m :mat4))
  (m4:trace m))


;;----------------------------------------------------------------


(varjo:v-def-glsl-template-fun
 negate (a) "(-~a)" (:mat2) 0 :pure t)

(varjo:v-def-glsl-template-fun
 negate (a) "(-~a)" (:mat3) 0 :pure t)

(varjo:v-def-glsl-template-fun
 negate (a) "(-~a)" (:mat4) 0 :pure t)


;;----------------------------------------------------------------


(varjo:v-def-glsl-template-fun * (a b c &rest c) "(~a £*£ ~a £*£ ~a ~{ £*£ ~a~})"
                               (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro * ((a t) (b t) (c t) &rest (d t))
  `(* ,a (* ,b (* ,c ,@d))))

(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:mat2 :mat2) :mat2 :pure t)
(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:mat3 :mat3) :mat3 :pure t)
(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:mat4 :mat4) :mat4 :pure t)


;;----------------------------------------------------------------
