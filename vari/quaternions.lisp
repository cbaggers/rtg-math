(in-package :rtg-math.vari)

(varjo:v-deftype quaternion () :vec4)

(v-def-glsl-template-fun q:w (a) "~a.x" (quaternion) :float
                         :v-place-index 0 :pure t)
(v-def-glsl-template-fun q:x (a) "~a.y" (quaternion) :float
                         :v-place-index 0 :pure t)
(v-def-glsl-template-fun q:y (a) "~a.z" (quaternion) :float
                         :v-place-index 0 :pure t)
(v-def-glsl-template-fun q:z (a) "~a.w" (quaternion) :float
                         :v-place-index 0 :pure t)

(v-defun q:dot ((quat-a quaternion) (quat-b quaternion))
  (+ (* (q:w quat-a) (q:w quat-b))
     (* (q:x quat-a) (q:x quat-b))
     (* (q:y quat-a) (q:y quat-b))
     (* (q:z quat-a) (q:z quat-b))))

(v-def-glsl-template-fun q:= (a b) "(~a == ~a)" (quaternion quaternion)
                         v-bool :pure t)

(v-def-glsl-template-fun q:/= (a b) "(~a != ~a)" (quaternion quaternion)
                         v-bool :pure t)

(v-def-glsl-template-fun q:+ (a b) "(~a + ~a)" (quaternion quaternion)
                         quaternion :pure t)

(v-def-glsl-template-fun q:- (a b) "(~a - ~a)" (quaternion quaternion)
                         quaternion :pure t)

(v-def-glsl-template-fun q:copy (a) "~a.xyzw" (quaternion)
                         quaternion :pure t)

(v-def-glsl-template-fun q:q! (w x y z) "vec4(~a, ~a, ~a, ~a)"
                         (:float :float :float :float) quaternion
                         :pure t)

(v-def-glsl-template-fun q:0! () "vec4(0,0,0,0)"
                         () quaternion
                         :pure t)

(v-def-glsl-template-fun q:identity () "vec4(1,0,0,0)"
                         () quaternion
                         :pure t)

(v-def-glsl-template-fun q:*s (a b) "(~a * ~a)" (quaternion :float)
                         quaternion :pure t)

(v-defun q:0p ((q quaternion))
  (= 0.0 (q:w q) (q:x q) (q:y q) (q:z q)))

(v-defun q:unitp ((q quaternion))
  (= 1f0 (+ (* (q:w q) (q:w q))
            (* (q:x q) (q:x q))
            (* (q:y q) (q:y q))
            (* (q:z q) (q:z q)))))

(v-defun q:identity-p ((q quaternion))
  (and (= (q:w q) 1.0)
       (= 0.0 (q:x q) (q:y q) (q:z q))))

(v-defun q:magnitude ((q quaternion))
  (let ((w (q:w q)) (x (q:x q)) (y (q:y q)) (z (q:z q)))
    (sqrt (+ (* w w) (* x x) (* y y) (* z z)))))

(v-defun q:normalize ((q quaternion))
  (let ((length-squared (q:dot q q)))
    (let ((factor (/ 1.0 (sqrt length-squared))))
      (q! (* (q:w q) factor)
          (* (q:x q) factor)
          (* (q:y q) factor)
          (* (q:z q) factor)))))

(v-defun q:conjugate ((q quaternion))
  (q! (q:w q) (- (q:x q)) (- (q:y q)) (- (q:z q))))

(v-defun inverse ((quat quaternion))
  (let* ((w (q:w quat))
         (x (q:x quat))
         (y (q:y quat))
         (z (q:z quat))
         (norm (+ (* w w) (* x x) (* y y) (* z z))))
    (if (= norm 0f0)
        (q:identity)
        (let ((norm-recip (/ 1.0 norm)))
          (q:q! (* norm-recip (q:w quat))
                (- (* norm-recip (q:x quat)))
                (- (* norm-recip (q:y quat)))
                (- (* norm-recip (q:z quat))))))))

(v-defun q:* ((quat-a quaternion) (quat-b quaternion))
  (q! (- (* (q:w quat-a) (q:w quat-b))
         (* (q:x quat-a) (q:x quat-b))
         (* (q:y quat-a) (q:y quat-b))
         (* (q:z quat-a) (q:z quat-b)))
      (- (+ (* (q:w quat-a) (q:x quat-b))
            (* (q:x quat-a) (q:w quat-b))
            (* (q:y quat-a) (q:z quat-b)))
         (* (q:z quat-a) (q:y quat-b)))
      (- (+ (* (q:w quat-a) (q:y quat-b))
            (* (q:y quat-a) (q:w quat-b))
            (* (q:z quat-a) (q:x quat-b)))
         (* (q:x quat-a) (q:z quat-b)))
      (- (+ (* (q:w quat-a) (q:z quat-b))
            (* (q:z quat-a) (q:w quat-b))
            (* (q:x quat-a) (q:y quat-b)))
         (* (q:y quat-a) (q:x quat-b)))))

(v-defun q:to-mat3 ((quat quaternion))
  (let ((w (q:w quat)) (x (q:x quat)) (y (q:y quat)) (z (q:z quat)))
    (let ((x2 (+ x x)) (y2 (+ y y)) (z2 (+ z z)))
      (let ((wx (* w x2))  (wy (* w y2))  (wz (* w z2))
            (xx (* x x2))  (xy (* x y2))  (xz (* x z2))
            (yy (* y y2))  (yz (* y z2))
            (zz (* z z2)))
        (mat3
         (- 1.0 (+ yy zz)) (- xy wz)          (+ xz wy)
         (+ xy wz)         (- 1.0 (+ xx zz))  (- yz wx)
         (- xz wy)         (+ yz wx)          (- 1.0 (+ xx yy)))))))

(v-defun q:to-mat4 ((quat quaternion))
  (let ((w (q:w quat)) (x (q:x quat)) (y (q:y quat)) (z (q:z quat)))
    (let ((x2 (+ x x)) (y2 (+ y y)) (z2 (+ z z)))
      (let ((wx (* w x2))  (wy (* w y2))  (wz (* w z2))
            (xx (* x x2))  (xy (* x y2))  (xz (* x z2))
            (yy (* y y2))  (yz (* y z2))
            (zz (* z z2)))
        (mat4
         (- 1.0 (+ yy zz)) (- xy wz)          (+ xz wy)         0.0
         (+ xy wz)         (- 1.0 (+ xx zz))  (- yz wx)         0.0
         (- xz wy)         (+ yz wx)          (- 1.0 (+ xx yy)) 0.0
         0.0               0.0                0.0               1.0)))))

(v-defun q:rotate ((vec3 :vec3) (quat quaternion))
  (let* ((v-mult (* 2.0 (+ (* (q:x quat) (aref vec3 0))
                           (* (q:y quat) (aref vec3 1))
                           (* (q:z quat) (aref vec3 2)))))
         (cross-mult (* 2.0 (q:w quat)))
         (p-mult (- (* cross-mult (q:w quat)) 1.0)))
    (vec3 (+ (* p-mult (aref vec3 0))
             (* v-mult (q:x quat))
             (* cross-mult
                (- (* (q:y quat) (aref vec3 2))
                   (* (q:z quat) (aref vec3 1)))))
          (+ (* p-mult (aref vec3 1))
             (* v-mult (q:y quat))
             (* cross-mult
                (- (* (q:z quat) (aref vec3 0))
                   (* (q:x quat) (aref vec3 2)))))
          (+ (* p-mult (aref vec3 2))
             (* v-mult (q:z quat))
             (* cross-mult
                (- (* (q:x quat) (aref vec3 1))
                   (* (q:y quat) (aref vec3 0))))))))

;; {TODO}
;; q:from-mat3
;; q:from-axis-angle
;; q:from-axies
;; q:look-at
;; q:point-at
;; q:from-direction
;; q:to-direction
;; q:to-direction-vec4
;; q:from-fixed-angles
;; q:from-fixed-angles-v3
;; q:get-axis-angle
;; q:lerp
;; q:slerp
;; q:approx-slerp
