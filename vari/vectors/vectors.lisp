(in-package :rtg-math.vectors)

;;----------------------------------------------------------------

(varjo:add-equivalent-name 'vari.cl:swizzle 'swizzle)
(varjo:add-equivalent-name 'vari.cl:swizzle 's~)

;;----------------------------------------------------------------

;; (defmacro dvec (var-list expression &body body)
;;   (when (or (not (listp var-list))
;;             (not (every #'symbolp var-list))
;;             (< (cl:length var-list) 1)
;;             (> (cl:length var-list) 4))
;;     (error "dvec: invalid vector destructuring pattern ~s"
;;            var-list))
;;   (let ((e (gensym "expression")))
;;     `(let ((,e ,expression))
;;        (let ,(loop :for v :in var-list :for i :from 0 :collect
;;                 `(,v (aref ,e ,i)))
;;          ,@body))))

;; (defmacro dvec* (var-list-expression-pairs &body body)
;;   (labels ((group (source n)
;;              "This takes a  flat list and emit a list of lists, each n long
;;               containing the elements of the original list"
;;              (if (= 0 n) (error "zero length"))
;;              (labels ((rec (source acc)
;;                         (let ((rest (nthcdr n source)))
;;                           (if (consp rest)
;;                               (rec rest (cons (subseq source 0 n)
;;                                               acc))
;;                               (nreverse (cons source acc))))))
;;                (if source
;;                    (rec source nil)
;;                    nil))))
;;     (let ((pairs (append body
;;                          (group var-list-expression-pairs 2))))
;;       (labels ((m (x y) `(dvec ,@y ,x)))
;;         (reduce #'m pairs)))))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun 0p (v) "(~a==0)" (vari:v-vector) :bool)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 unitp (v) "(length(~a)==1.0f)" (vari:v-vector) :bool)

;;----------------------------------------------------------------

(varjo:add-equivalent-name 'cl:= '=)

;;----------------------------------------------------------------

(varjo:add-equivalent-name 'cl:/= '/=)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun +s (v s) "(~a + ~a)" (:vec2 :float) :vec2 :pure t)
(varjo:v-def-glsl-template-fun -s (v s) "(~a - ~a)" (:vec2 :float) :vec2 :pure t)
(varjo:v-def-glsl-template-fun +s (v s) "(~a + ~a)" (:vec3 :float) :vec3 :pure t)
(varjo:v-def-glsl-template-fun -s (v s) "(~a - ~a)" (:vec3 :float) :vec3 :pure t)
(varjo:v-def-glsl-template-fun +s (v s) "(~a + ~a)" (:vec4 :float) :vec4 :pure t)
(varjo:v-def-glsl-template-fun -s (v s) "(~a - ~a)" (:vec4 :float) :vec4 :pure t)

(varjo:v-def-glsl-template-fun +s (v s) "(~a + ~a)" (:dvec2 :double) :dvec2 :pure t)
(varjo:v-def-glsl-template-fun -s (v s) "(~a - ~a)" (:dvec2 :double) :dvec2 :pure t)
(varjo:v-def-glsl-template-fun +s (v s) "(~a + ~a)" (:dvec3 :double) :dvec3 :pure t)
(varjo:v-def-glsl-template-fun -s (v s) "(~a - ~a)" (:dvec3 :double) :dvec3 :pure t)
(varjo:v-def-glsl-template-fun +s (v s) "(~a + ~a)" (:dvec4 :double) :dvec4 :pure t)
(varjo:v-def-glsl-template-fun -s (v s) "(~a - ~a)" (:dvec4 :double) :dvec4 :pure t)

(varjo:v-def-glsl-template-fun +s (v s) "(~a + ~a)" (:ivec2 :int) :ivec2 :pure t)
(varjo:v-def-glsl-template-fun -s (v s) "(~a - ~a)" (:ivec2 :int) :ivec2 :pure t)
(varjo:v-def-glsl-template-fun +s (v s) "(~a + ~a)" (:ivec3 :int) :ivec3 :pure t)
(varjo:v-def-glsl-template-fun -s (v s) "(~a - ~a)" (:ivec3 :int) :ivec3 :pure t)
(varjo:v-def-glsl-template-fun +s (v s) "(~a + ~a)" (:ivec4 :int) :ivec4 :pure t)
(varjo:v-def-glsl-template-fun -s (v s) "(~a - ~a)" (:ivec4 :int) :ivec4 :pure t)

(varjo:v-def-glsl-template-fun +s (v s) "(~a + ~a)" (:uvec2 :uint) :uvec2 :pure t)
(varjo:v-def-glsl-template-fun -s (v s) "(~a - ~a)" (:uvec2 :uint) :uvec2 :pure t)
(varjo:v-def-glsl-template-fun +s (v s) "(~a + ~a)" (:uvec3 :uint) :uvec3 :pure t)
(varjo:v-def-glsl-template-fun -s (v s) "(~a - ~a)" (:uvec3 :uint) :uvec3 :pure t)
(varjo:v-def-glsl-template-fun +s (v s) "(~a + ~a)" (:uvec4 :uint) :uvec4 :pure t)
(varjo:v-def-glsl-template-fun -s (v s) "(~a - ~a)" (:uvec4 :uint) :uvec4 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun + (a b c &rest c) "(~a £+£ ~a £+£ ~a ~{ £+£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro + ((a t) (b t) (c t) &rest (d t))
  `(+ ,a (+ ,b (+ ,c ,@d))))

(varjo:v-def-glsl-template-fun + () "0" () :int :pure t)
(varjo:v-def-glsl-template-fun + (a) "~a" (vari:v-vector) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:vec2 :vec2) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:vec3 :vec3) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:vec4 :vec4) 0 :pure t)

(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:ivec2 :ivec2) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:ivec3 :ivec3) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:ivec4 :ivec4) 0 :pure t)

(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:uvec2 :uvec2) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:uvec3 :uvec3) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:uvec4 :uvec4) 0 :pure t)

(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:dvec2 :dvec2) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:dvec3 :dvec3) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:dvec4 :dvec4) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun - (a b c &rest c) "(~a £-£ ~a £-£ ~a ~{ £-£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro - ((a t) (b t) (c t) &rest (d t))
  `(- ,a (- ,b (- ,c ,@d))))

(varjo:v-def-glsl-template-fun - (a) "(-~a)" (:vec2) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a) "(-~a)" (:vec3) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a) "(-~a)" (:vec4) 0 :pure t)

(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:vec2 :vec2) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:vec3 :vec3) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:vec4 :vec4) 0 :pure t)

(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:ivec2 :ivec2) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:ivec3 :ivec3) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:ivec4 :ivec4) 0 :pure t)

(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:uvec2 :uvec2) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:uvec3 :uvec3) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:uvec4 :uvec4) 0 :pure t)

(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:dvec2 :dvec2) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:dvec3 :dvec3) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:dvec4 :dvec4) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun * (a b c &rest c) "(~a £*£ ~a £*£ ~a ~{ £*£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro * ((a t) (b t) (c t) &rest (d t))
  `(* ,a (* ,b (* ,c ,@d))))

(varjo:v-def-glsl-template-fun * () "1" () :int :pure t)
(varjo:v-def-glsl-template-fun * (a) "~a" (vari:v-vector) 0 :pure t)

(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:vec2 :vec2) 0 :pure t)
(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:vec3 :vec3) 0 :pure t)
(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:vec4 :vec4) 0 :pure t)

(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:ivec2 :ivec2) 0 :pure t)
(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:ivec3 :ivec3) 0 :pure t)
(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:ivec4 :ivec4) 0 :pure t)

(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:uvec2 :uvec2) 0 :pure t)
(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:uvec3 :uvec3) 0 :pure t)
(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:uvec4 :uvec4) 0 :pure t)

(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:dvec2 :dvec2) 0 :pure t)
(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:dvec3 :dvec3) 0 :pure t)
(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:dvec4 :dvec4) 0 :pure t)


(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:float :vec2) 1 :pure t)
(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:float :vec3) 1 :pure t)
(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:float :vec4) 1 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun / (a b c &rest c) "(~a £/£ ~a £/£ ~a ~{ £/£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro / ((a t) (b t) (c t) &rest (d t))
  `(/ ,a (/ ,b (/ ,c ,@d))))

(varjo:v-def-glsl-template-fun / (a) "(1 / ~a)" (vari:v-vector) 0 :pure t)

(varjo:v-def-glsl-template-fun / (a b) "(~a / ~a)" (vari:v-vector vari:v-real) 0 :pure t)
(varjo:v-def-glsl-template-fun / (a b) "(~a / ~a)" (:vec2 :vec2) 0 :pure t)
(varjo:v-def-glsl-template-fun / (a b) "(~a / ~a)" (:vec3 :vec3) 0 :pure t)
(varjo:v-def-glsl-template-fun / (a b) "(~a / ~a)" (:vec4 :vec4) 0 :pure t)

(varjo:v-def-glsl-template-fun / (a b) "(~a / ~a)" (:ivec2 :ivec2) 0 :pure t)
(varjo:v-def-glsl-template-fun / (a b) "(~a / ~a)" (:ivec3 :ivec3) 0 :pure t)
(varjo:v-def-glsl-template-fun / (a b) "(~a / ~a)" (:ivec4 :ivec4) 0 :pure t)

(varjo:v-def-glsl-template-fun / (a b) "(~a / ~a)" (:uvec2 :uvec2) 0 :pure t)
(varjo:v-def-glsl-template-fun / (a b) "(~a / ~a)" (:uvec3 :uvec3) 0 :pure t)
(varjo:v-def-glsl-template-fun / (a b) "(~a / ~a)" (:uvec4 :uvec4) 0 :pure t)

(varjo:v-def-glsl-template-fun / (a b) "(~a / ~a)" (:dvec2 :dvec2) 0 :pure t)
(varjo:v-def-glsl-template-fun / (a b) "(~a / ~a)" (:dvec3 :dvec3) 0 :pure t)
(varjo:v-def-glsl-template-fun / (a b) "(~a / ~a)" (:dvec4 :dvec4) 0 :pure t)

;;----------------------------------------------------------------

(varjo:add-equivalent-name 'cl:length 'rtg-length)

;;----------------------------------------------------------------

(varjo:v-defun length-squared ((vector-a :vec2))
  (cl:+ (cl:expt x 2) (cl:expt y 2)))
(varjo:v-defun length-squared ((vector-a :vec3))
  (cl:+ (cl:expt x 2) (cl:expt y 2) (cl:expt z 2)))
(varjo:v-defun length-squared ((vector-a :vec4))
  (cl:+ (cl:expt x 2) (cl:expt y 2) (cl:expt z 2) (cl:expt w 2)))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 distance (a b) "distance(~a, ~a)" (:vec2 :vec2) :float :pure t)
(varjo:v-def-glsl-template-fun
 distance (a b) "distance(~a, ~a)" (:vec3 :vec3) :float :pure t)
(varjo:v-def-glsl-template-fun
 distance (a b) "distance(~a, ~a)" (:vec4 :vec4) :float :pure t)
(varjo:v-def-glsl-template-fun
 distance (a b) "distance(~a, ~a)" (:dvec2 :dvec2) :double :pure t)
(varjo:v-def-glsl-template-fun
 distance (a b) "distance(~a, ~a)" (:dvec3 :dvec3) :double :pure t)
(varjo:v-def-glsl-template-fun
 distance (a b) "distance(~a, ~a)" (:dvec4 :dvec4) :double :pure t)

;;----------------------------------------------------------------

(varjo:v-defun distance-squared ((vec-a :vec2) (vec-b :vec2))
  (length-squared (- vector-b vector-a)))
(varjo:v-defun distance-squared ((vec-a :vec3) (vec-b :vec3))
  (length-squared (- vector-b vector-a)))
(varjo:v-defun distance-squared ((vec-a :vec4) (vec-b :vec4))
  (length-squared (- vector-b vector-a)))

(varjo:v-defun distance-squared ((vec-a :dvec2) (vec-b :dvec2))
  (length-squared (- vector-b vector-a)))
(varjo:v-defun distance-squared ((vec-a :dvec3) (vec-b :dvec3))
  (length-squared (- vector-b vector-a)))
(varjo:v-defun distance-squared ((vec-a :dvec4) (vec-b :dvec4))
  (length-squared (- vector-b vector-a)))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 dot (a b) "dot(~a, ~a)" (:vec2 :vec2) :float :pure t)
(varjo:v-def-glsl-template-fun
 dot (a b) "dot(~a, ~a)" (:vec3 :vec3) :float :pure t)
(varjo:v-def-glsl-template-fun
 dot (a b) "dot(~a, ~a)" (:vec4 :vec4) :float :pure t)
(varjo:v-def-glsl-template-fun
 dot (a b) "dot(~a, ~a)" (:dvec2 :dvec2) :double :pure t)
(varjo:v-def-glsl-template-fun
 dot (a b) "dot(~a, ~a)" (:dvec3 :dvec3) :double :pure t)
(varjo:v-def-glsl-template-fun
 dot (a b) "dot(~a, ~a)" (:dvec4 :dvec4) :double :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 negate (a) "(-~a)" (vari:v-vector) 0 :pure t)

(vari:v-defun face-foreward ((vec-a :vec2) (vec-b :vec2))
  (if (> (dot vector-a vector-b) 0)
      vector-a
      (negate vector-a)))

(vari:v-defun face-foreward ((vec-a :vec3) (vec-b :vec3))
  (if (> (dot vector-a vector-b) 0)
      vector-a
      (negate vector-a)))

(vari:v-defun face-foreward ((vec-a :vec4) (vec-b :vec4))
  (if (> (dot vector-a vector-b) 0)
      vector-a
      (negate vector-a)))

(vari:v-defun face-foreward ((vec-a :dvec2) (vec-b :dvec2))
  (if (> (dot vector-a vector-b) 0)
      vector-a
      (negate vector-a)))

(vari:v-defun face-foreward ((vec-a :dvec3) (vec-b :dvec3))
  (if (> (dot vector-a vector-b) 0)
      vector-a
      (negate vector-a)))

(vari:v-defun face-foreward ((vec-a :dvec4) (vec-b :dvec4))
  (if (> (dot vector-a vector-b) 0)
      vector-a
      (negate vector-a)))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 absolute-dot (a b) "abs(dot(~a, ~a))" (:vec2 :vec2) :float :pure t)
(varjo:v-def-glsl-template-fun
 absolute-dot (a b) "abs(dot(~a, ~a))" (:vec3 :vec3) :float :pure t)
(varjo:v-def-glsl-template-fun
 absolute-dot (a b) "abs(dot(~a, ~a))" (:vec4 :vec4) :float :pure t)
(varjo:v-def-glsl-template-fun
 absolute-dot (a b) "abs(dot(~a, ~a))" (:dvec2 :dvec2) :double :pure t)
(varjo:v-def-glsl-template-fun
 absolute-dot (a b) "abs(dot(~a, ~a))" (:dvec3 :dvec3) :double :pure t)
(varjo:v-def-glsl-template-fun
 absolute-dot (a b) "abs(dot(~a, ~a))" (:dvec4 :dvec4) :double :pure t)

;;----------------------------------------------------------------

(varjo:v-defun perp-dot ((vec-a :vec2) (vec-b :vec2))
  (cl:- (cl:* (x vec-a) (y vec-b))
        (cl:* (y vec-a) (x vec-b))))

;;----------------------------------------------------------------

(varjo:add-equivalent-name 'vari.glsl:normalize 'normalize)

;;----------------------------------------------------------------

(varjo:add-equivalent-name 'vari.glsl:cross 'cross)

;;----------------------------------------------------------------

;; (declaim (inline lerp)
;;          (ftype (function ((or vec2 vec3 vec4)
;;                            (or vec2 vec3 vec4)
;;                            (or (integer) single-float))
;;                           (or vec2 vec3 vec4))
;;                 lerp))
;; (defun lerp (vector-a vector-b ammount)
;;   (declare (type (or vec2 vec3 vec4) vector-a vector-b))
;;   (case= (cl:length vector-a)
;;     (2 (v2:lerp vector-a vector-b (float ammount)))
;;     (3 (v3:lerp vector-a vector-b (float ammount)))
;;     (4 (v4:lerp vector-a vector-b (float ammount)))
;;     (otherwise (error "only vectors of size 2-4 are valid"))))

;; (declaim (inline mix)
;;          (ftype (function ((or vec2
;;                                vec3
;;                                vec4)
;;                            (or vec2
;;                                vec3
;;                                vec4)
;;                            (or (integer) single-float))
;;                           (or vec2
;;                               vec3
;;                               vec4))
;;                 mix))
;; (defun mix (vector-a vector-b ammount)
;;   (declare (type (or vec2 vec3 vec4) vector-a vector-b))
;;   (lerp vector-a vector-b ammount))

;; (defun bezier (a1 a2 b1 b2 ammount)
;;   (lerp (lerp a1 a2 ammount)
;;         (lerp b1 b2 ammount)
;;         ammount))

;;---------------------------------------------------------------

(varjo:add-equivalent-name 'cl:incf 'incf)
(varjo:add-equivalent-name 'cl:decf 'decf)

;;---------------------------------------------------------------
