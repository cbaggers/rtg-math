(in-package :rtg-math.matrix4.non-consing)

;;----------------------------------------------------------------

(defn-inline set-components
    ((c00 single-float) (c01 single-float) (c02 single-float) (c03 single-float)
     (c10 single-float) (c11 single-float) (c12 single-float) (c13 single-float)
     (c20 single-float) (c21 single-float) (c22 single-float) (c23 single-float)
     (c30 single-float) (c31 single-float) (c32 single-float) (c33 single-float)
     (mat4-to-mutate mat4)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  ;; as you can see it is stored in column major order
  (setf (melm mat4-to-mutate 0 0) c00)
  (setf (melm mat4-to-mutate 0 1) c01)
  (setf (melm mat4-to-mutate 0 2) c02)
  (setf (melm mat4-to-mutate 0 3) c03)
  (setf (melm mat4-to-mutate 1 0) c10)
  (setf (melm mat4-to-mutate 1 1) c11)
  (setf (melm mat4-to-mutate 1 2) c12)
  (setf (melm mat4-to-mutate 1 3) c13)
  (setf (melm mat4-to-mutate 2 0) c20)
  (setf (melm mat4-to-mutate 2 1) c21)
  (setf (melm mat4-to-mutate 2 2) c22)
  (setf (melm mat4-to-mutate 2 3) c23)
  (setf (melm mat4-to-mutate 3 0) c30)
  (setf (melm mat4-to-mutate 3 1) c31)
  (setf (melm mat4-to-mutate 3 2) c32)
  (setf (melm mat4-to-mutate 3 3) c33)
  mat4-to-mutate)

;;----------------------------------------------------------------

(defn-inline copy-components ((mat mat4) (copy-from mat4)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (melm mat 0 0) (melm copy-from 0 0))
  (setf (melm mat 0 1) (melm copy-from 0 1))
  (setf (melm mat 0 2) (melm copy-from 0 2))
  (setf (melm mat 0 3) (melm copy-from 0 3))
  (setf (melm mat 1 0) (melm copy-from 1 0))
  (setf (melm mat 1 1) (melm copy-from 1 1))
  (setf (melm mat 1 2) (melm copy-from 1 2))
  (setf (melm mat 1 3) (melm copy-from 1 3))
  (setf (melm mat 2 0) (melm copy-from 2 0))
  (setf (melm mat 2 1) (melm copy-from 2 1))
  (setf (melm mat 2 2) (melm copy-from 2 2))
  (setf (melm mat 2 3) (melm copy-from 2 3))
  (setf (melm mat 3 0) (melm copy-from 3 0))
  (setf (melm mat 3 1) (melm copy-from 3 1))
  (setf (melm mat 3 2) (melm copy-from 3 2))
  (setf (melm mat 3 3) (melm copy-from 3 3))
  mat)

;;----------------------------------------------------------------

(defn %* ((mat-accum mat4) (to-multiply-mat mat4)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((a (cl:+ (cl:* (melm mat-accum 0 0) (melm to-multiply-mat 0 0))
                 (cl:* (melm mat-accum 0 1) (melm to-multiply-mat 1 0))
                 (cl:* (melm mat-accum 0 2) (melm to-multiply-mat 2 0))
                 (cl:* (melm mat-accum 0 3) (melm to-multiply-mat 3 0))))
        (b (cl:+ (cl:* (melm mat-accum 0 0) (melm to-multiply-mat 0 1))
                 (cl:* (melm mat-accum 0 1) (melm to-multiply-mat 1 1))
                 (cl:* (melm mat-accum 0 2) (melm to-multiply-mat 2 1))
                 (cl:* (melm mat-accum 0 3) (melm to-multiply-mat 3 1))))
        (c (cl:+ (cl:* (melm mat-accum 0 0) (melm to-multiply-mat 0 2))
                 (cl:* (melm mat-accum 0 1) (melm to-multiply-mat 1 2))
                 (cl:* (melm mat-accum 0 2) (melm to-multiply-mat 2 2))
                 (cl:* (melm mat-accum 0 3) (melm to-multiply-mat 3 2))))
        (d (cl:+ (cl:* (melm mat-accum 0 0) (melm to-multiply-mat 0 3))
                 (cl:* (melm mat-accum 0 1) (melm to-multiply-mat 1 3))
                 (cl:* (melm mat-accum 0 2) (melm to-multiply-mat 2 3))
                 (cl:* (melm mat-accum 0 3) (melm to-multiply-mat 3 3))))
        (e (cl:+ (cl:* (melm mat-accum 1 0) (melm to-multiply-mat 0 0))
                 (cl:* (melm mat-accum 1 1) (melm to-multiply-mat 1 0))
                 (cl:* (melm mat-accum 1 2) (melm to-multiply-mat 2 0))
                 (cl:* (melm mat-accum 1 3) (melm to-multiply-mat 3 0))))
        (f (cl:+ (cl:* (melm mat-accum 1 0) (melm to-multiply-mat 0 1))
                 (cl:* (melm mat-accum 1 1) (melm to-multiply-mat 1 1))
                 (cl:* (melm mat-accum 1 2) (melm to-multiply-mat 2 1))
                 (cl:* (melm mat-accum 1 3) (melm to-multiply-mat 3 1))))
        (g (cl:+ (cl:* (melm mat-accum 1 0) (melm to-multiply-mat 0 2))
                 (cl:* (melm mat-accum 1 1) (melm to-multiply-mat 1 2))
                 (cl:* (melm mat-accum 1 2) (melm to-multiply-mat 2 2))
                 (cl:* (melm mat-accum 1 3) (melm to-multiply-mat 3 2))))
        (h (cl:+ (cl:* (melm mat-accum 1 0) (melm to-multiply-mat 0 3))
                 (cl:* (melm mat-accum 1 1) (melm to-multiply-mat 1 3))
                 (cl:* (melm mat-accum 1 2) (melm to-multiply-mat 2 3))
                 (cl:* (melm mat-accum 1 3) (melm to-multiply-mat 3 3))))
        (i (cl:+ (cl:* (melm mat-accum 2 0) (melm to-multiply-mat 0 0))
                 (cl:* (melm mat-accum 2 1) (melm to-multiply-mat 1 0))
                 (cl:* (melm mat-accum 2 2) (melm to-multiply-mat 2 0))
                 (cl:* (melm mat-accum 2 3) (melm to-multiply-mat 3 0))))
        (j (cl:+ (cl:* (melm mat-accum 2 0) (melm to-multiply-mat 0 1))
                 (cl:* (melm mat-accum 2 1) (melm to-multiply-mat 1 1))
                 (cl:* (melm mat-accum 2 2) (melm to-multiply-mat 2 1))
                 (cl:* (melm mat-accum 2 3) (melm to-multiply-mat 3 1))))
        (k (cl:+ (cl:* (melm mat-accum 2 0) (melm to-multiply-mat 0 2))
                 (cl:* (melm mat-accum 2 1) (melm to-multiply-mat 1 2))
                 (cl:* (melm mat-accum 2 2) (melm to-multiply-mat 2 2))
                 (cl:* (melm mat-accum 2 3) (melm to-multiply-mat 3 2))))
        (l (cl:+ (cl:* (melm mat-accum 2 0) (melm to-multiply-mat 0 3))
                 (cl:* (melm mat-accum 2 1) (melm to-multiply-mat 1 3))
                 (cl:* (melm mat-accum 2 2) (melm to-multiply-mat 2 3))
                 (cl:* (melm mat-accum 2 3) (melm to-multiply-mat 3 3))))
        (m (cl:+ (cl:* (melm mat-accum 3 0) (melm to-multiply-mat 0 0))
                 (cl:* (melm mat-accum 3 1) (melm to-multiply-mat 1 0))
                 (cl:* (melm mat-accum 3 2) (melm to-multiply-mat 2 0))
                 (cl:* (melm mat-accum 3 3) (melm to-multiply-mat 3 0))))
        (n (cl:+ (cl:* (melm mat-accum 3 0) (melm to-multiply-mat 0 1))
                 (cl:* (melm mat-accum 3 1) (melm to-multiply-mat 1 1))
                 (cl:* (melm mat-accum 3 2) (melm to-multiply-mat 2 1))
                 (cl:* (melm mat-accum 3 3) (melm to-multiply-mat 3 1))))
        (o (cl:+ (cl:* (melm mat-accum 3 0) (melm to-multiply-mat 0 2))
                 (cl:* (melm mat-accum 3 1) (melm to-multiply-mat 1 2))
                 (cl:* (melm mat-accum 3 2) (melm to-multiply-mat 2 2))
                 (cl:* (melm mat-accum 3 3) (melm to-multiply-mat 3 2))))
        (p (cl:+ (cl:* (melm mat-accum 3 0) (melm to-multiply-mat 0 3))
                 (cl:* (melm mat-accum 3 1) (melm to-multiply-mat 1 3))
                 (cl:* (melm mat-accum 3 2) (melm to-multiply-mat 2 3))
                 (cl:* (melm mat-accum 3 3) (melm to-multiply-mat 3 3)))))
    (setf (melm mat-accum 0 0) a)
    (setf (melm mat-accum 0 1) b)
    (setf (melm mat-accum 0 2) c)
    (setf (melm mat-accum 0 3) d)
    (setf (melm mat-accum 1 0) e)
    (setf (melm mat-accum 1 1) f)
    (setf (melm mat-accum 1 2) g)
    (setf (melm mat-accum 1 3) h)
    (setf (melm mat-accum 2 0) i)
    (setf (melm mat-accum 2 1) j)
    (setf (melm mat-accum 2 2) k)
    (setf (melm mat-accum 2 3) l)
    (setf (melm mat-accum 3 0) m)
    (setf (melm mat-accum 3 1) n)
    (setf (melm mat-accum 3 2) o)
    (setf (melm mat-accum 3 3) p)
    mat-accum))

(defn * ((accum-mat mat4) &rest (mat4s mat4)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (reduce #'%* mat4s :initial-value accum-mat))

(define-compiler-macro * (&whole whole accum-mat &rest mat4s)
  (assert accum-mat)
  (case= (cl:length mat4s)
    (0 accum-mat)
    (1 `(%* ,accum-mat ,(first mat4s)))
    (otherwise whole)))

;;----------------------------------------------------------------

(defn set-from-mat3 ((mat-to-mutate mat4) (m-a mat3)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components
   (m3:melm m-a 0 0)  (m3:melm m-a 0 1)  (m3:melm m-a 0 2)  0f0
   (m3:melm m-a 1 0)  (m3:melm m-a 1 1)  (m3:melm m-a 1 2)  0f0
   (m3:melm m-a 2 0)  (m3:melm m-a 2 1)  (m3:melm m-a 2 2)  0f0
   0f0                0f0                0f0                1f0
   mat-to-mutate))

;;----------------------------------------------------------------

(defn set-from-rows ((mat-to-mutate mat4)
                     (row-1 vec4) (row-2 vec4) (row-3 vec4) (row-4 vec4)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (x row-1) (y row-1) (z row-1) (w row-1)
                  (x row-2) (y row-2) (z row-2) (w row-2)
                  (x row-3) (y row-3) (z row-3) (w row-3)
                  (x row-4) (y row-4) (z row-4) (w row-4)
                  mat-to-mutate))

(defn set-from-rows-v3 ((mat-to-mutate mat4)
                        (row-1 vec3) (row-2 vec3) (row-3 vec3)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (x row-1) (y row-1) (z row-1) 0f0
                  (x row-2) (y row-2) (z row-2) 0f0
                  (x row-3) (y row-3) (z row-3) 0f0
                  0f0       0f0       0f0       1f0
                  mat-to-mutate))

;;----------------------------------------------------------------

(defn set-from-columns ((mat-to-mutate mat4)
                        (col-1 vec4) (col-2 vec4) (col-3 vec4) (col-4 vec4)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (x col-1)
                  (x col-2)
                  (x col-3)
                  (x col-4)
                  (y col-1)
                  (y col-2)
                  (y col-3)
                  (y col-4)
                  (z col-1)
                  (z col-2)
                  (z col-3)
                  (z col-4)
                  (w col-1)
                  (w col-2)
                  (w col-3)
                  (w col-4)
                  mat-to-mutate))

(defn set-from-columns-v3 ((mat-to-mutate mat4)
                           (col-1 vec3) (col-2 vec3) (col-3 vec3)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (x col-1)
                  (x col-2)
                  (x col-3)
                  0f0
                  (y col-1)
                  (y col-2)
                  (y col-3)
                  0f0
                  (z col-1)
                  (z col-2)
                  (z col-3)
                  0f0
                  0f0
                  0f0
                  0f0
                  1f0
                  mat-to-mutate))

;;----------------------------------------------------------------

(defn adjoint ((mat-to-mutate mat4)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (minor mat-to-mutate 1 2 3 1 2 3)
                  (cl:- (minor mat-to-mutate 0 2 3 1 2 3))
                  (minor mat-to-mutate 0 1 3 1 2 3)
                  (cl:- (minor mat-to-mutate 0 1 2 1 2 3))

                  (cl:- (minor mat-to-mutate 1 2 3 0 2 3))
                  (minor mat-to-mutate 0 2 3 0 2 3)
                  (cl:- (minor mat-to-mutate 0 1 3 0 2 3))
                  (minor mat-to-mutate 0 1 2 0 2 3)

                  (minor mat-to-mutate 1 2 3 0 1 3)
                  (cl:- (minor mat-to-mutate 0 2 3 0 1 3))
                  (minor mat-to-mutate 0 1 3 0 1 3)
                  (cl:- (minor mat-to-mutate 0 1 2 0 1 3))

                  (cl:- (minor mat-to-mutate 1 2 3 0 1 2))
                  (minor mat-to-mutate 0 2 3 0 1 2)
                  (cl:- (minor mat-to-mutate 0 1 3 0 1 2))
                  (minor mat-to-mutate 0 1 2 0 1 2)
                  mat-to-mutate))

;;----------------------------------------------------------------

;;this one is from 'Essential Maths'
(defn affine-inverse ((mat-to-invert mat4)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  ;;calculate upper left 3x3 matrix determinant
  (let* ((cofac-0 (cl:- (cl:* (melm mat-to-invert 1 1) (melm mat-to-invert 2 2))
                        (cl:* (melm mat-to-invert 2 1) (melm mat-to-invert 1 2))))

         (cofac-4 (cl:- (cl:* (melm mat-to-invert 2 0) (melm mat-to-invert 1 2))
                        (cl:* (melm mat-to-invert 1 0) (melm mat-to-invert 2 2))))

         (cofac-8 (cl:- (cl:* (melm mat-to-invert 1 0) (melm mat-to-invert 2 1))
                        (cl:* (melm mat-to-invert 2 0) (melm mat-to-invert 1 1))))
         (det (cl:+ (cl:* (melm mat-to-invert 0 0) cofac-0)
                    (cl:* (melm mat-to-invert 0 1) cofac-4)
                    (cl:* (melm mat-to-invert 0 2) cofac-8))))
    (if
     (cl:= 0f0 det)
     (error "Matrix4 Inverse: Singular Matrix")
     (let*
         ((inv-det (cl:/ 1f0 det))
          (r00 (cl:* inv-det cofac-0))
          (r10 (cl:* inv-det cofac-4))
          (r20 (cl:* inv-det cofac-8))
          (r01 (cl:* inv-det (cl:- (cl:* (melm mat-to-invert 2 1) (melm mat-to-invert 0 2))
                                   (cl:* (melm mat-to-invert 0 1) (melm mat-to-invert 2 2)))))
          (r11 (cl:* inv-det (cl:- (cl:* (melm mat-to-invert 0 0) (melm mat-to-invert 2 2))
                                   (cl:* (melm mat-to-invert 2 0) (melm mat-to-invert 0 2)))))
          (r21 (cl:* inv-det (cl:- (cl:* (melm mat-to-invert 2 0) (melm mat-to-invert 0 1))
                                   (cl:* (melm mat-to-invert 0 0) (melm mat-to-invert 2 1)))))
          (r02 (cl:* inv-det (cl:- (cl:* (melm mat-to-invert 0 1) (melm mat-to-invert 1 2))
                                   (cl:* (melm mat-to-invert 1 1) (melm mat-to-invert 0 2)))))
          (r12 (cl:* inv-det (cl:- (cl:* (melm mat-to-invert 1 0) (melm mat-to-invert 0 2))
                                   (cl:* (melm mat-to-invert 0 0) (melm mat-to-invert 1 2)))))
          (r22 (cl:* inv-det (cl:- (cl:* (melm mat-to-invert 0 0) (melm mat-to-invert 1 1))
                                   (cl:* (melm mat-to-invert 1 0) (melm mat-to-invert 0 1))))))
       (set-components
        r00 r01 r02
        (cl:- 0f0
              (cl:* (melm mat-to-invert 0 0) (melm mat-to-invert 0 3))
              (cl:* (melm mat-to-invert 0 1) (melm mat-to-invert 1 3))
              (cl:* (melm mat-to-invert 0 2) (melm mat-to-invert 2 3)))
        r10 r11 r12
        (cl:- 0f0
              (cl:* (melm mat-to-invert 1 0) (melm mat-to-invert 0 3))
              (cl:* (melm mat-to-invert 1 1) (melm mat-to-invert 1 3))
              (cl:* (melm mat-to-invert 1 2) (melm mat-to-invert 2 3)))
        r20 r21 r22
        (cl:- 0f0
              (cl:* (melm mat-to-invert 2 0) (melm mat-to-invert 0 3))
              (cl:* (melm mat-to-invert 2 1) (melm mat-to-invert 1 3))
              (cl:* (melm mat-to-invert 2 2) (melm mat-to-invert 2 3)))
        0f0 0f0 0f0 1f0
        mat-to-invert)))))

;;----------------------------------------------------------------
;; {TODO} could just feed straight from array into make

(defn transpose ((mat-to-transpose mat4)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components
   (melm mat-to-transpose 0 0) (melm mat-to-transpose 1 0) (melm mat-to-transpose 2 0) (melm mat-to-transpose 3 0)
   (melm mat-to-transpose 0 1) (melm mat-to-transpose 1 1) (melm mat-to-transpose 2 1) (melm mat-to-transpose 3 1)
   (melm mat-to-transpose 0 2) (melm mat-to-transpose 1 2) (melm mat-to-transpose 2 2) (melm mat-to-transpose 3 2)
   (melm mat-to-transpose 0 3) (melm mat-to-transpose 1 3) (melm mat-to-transpose 2 3) (melm mat-to-transpose 3 3)
   mat-to-transpose))

;;----------------------------------------------------------------

(defn set-from-translation ((mat-to-mutate mat4)
                            (vec-a (simple-array single-float))) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components
   1f0  0f0  0f0  (x vec-a)
   0f0  1f0  0f0  (y vec-a)
   0f0  0f0  1f0  (z vec-a)
   0f0  0f0  0f0  1f0
   mat-to-mutate))

;;----------------------------------------------------------------

(defn set-rotation-from-euler ((mat-to-mutate mat4) (vec3-a vec3)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((x (x vec3-a))
        (y (y vec3-a))
        (z (z vec3-a)))
    (let ((sx (sin x)) (cx (cos x))
          (sy (sin y)) (cy (cos y))
          (sz (sin z)) (cz (cos z)))
      (set-components
       (cl:* cy cz)
       (cl:- (cl:* cy sz))
       sy
       0f0

       (cl:+ (cl:* sx sy cz) (cl:* cx sz))
       (cl:- (cl:* cx cz) (cl:* sx sy sz))
       (cl:- (cl:* sx cy))
       0f0

       (cl:- (cl:* sx sz) (cl:* cx sy cz))
       (cl:+ (cl:* cx sy sz) (cl:* sx cz))
       (cl:* cx cy)
       0f0

       0f0 0f0 0f0 1f0
       mat-to-mutate))))

;;----------------------------------------------------------------

(defn set-from-scale ((mat-to-mutate mat4) (scale-vec3 vec3)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components
   (x scale-vec3)    0f0               0f0               0f0
   0f0               (y scale-vec3)    0f0               0f0
   0f0               0f0               (z scale-vec3)    0f0
   0f0               0f0               0f0               1f0
   mat-to-mutate))

;;----------------------------------------------------------------

(defn set-from-rotation-x ((mat-to-mutate mat4) (angle single-float)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (set-components 1f0  0f0  0f0        0f0
                    0f0  c-a  (cl:- s-a) 0f0
                    0f0  s-a  c-a        0f0
                    0f0  0f0  0f0        1f0
                    mat-to-mutate)))

;;----------------------------------------------------------------

(defn set-from-rotation-y ((mat-to-mutate mat4) (angle single-float)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (set-components c-a         0f0  s-a  0f0
                    0f0         1f0  0f0  0f0
                    (cl:- s-a)  0f0  c-a  0f0
                    0f0         0f0  0f0  1f0
                    mat-to-mutate)))

;;----------------------------------------------------------------

(defn set-from-rotation-z ((mat-to-mutate mat4) (angle single-float)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (set-components c-a  (cl:- s-a)  0f0  0f0
                    s-a  c-a         0f0  0f0
                    0f0  0f0         1f0  0f0
                    0f0  0f0         0f0  1f0
                    mat-to-mutate)))

;;----------------------------------------------------------------

(defn set-rotation-from-axis-angle ((mat-to-mutate mat4)
                                    (axis3 vec3) (angle single-float)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((c (cos angle))
         (s (sin angle))
         (g (cl:- 1f0 c)))
    (let* ((naxis3 (v3:normalize axis3))
           (x (x naxis3))
           (y (y naxis3))
           (z (z naxis3))
           (gxx (cl:* g x x))
           (gxy (cl:* g x y))
           (gxz (cl:* g x z))
           (gyy (cl:* g y y))
           (gyz (cl:* g y z))
           (gzz (cl:* g z z)))
      (set-components
       (cl:+ gxx c)           (cl:- gxy (cl:* s z))  (cl:+ gxz (cl:* s y))  0f0
       (cl:+ gxy (cl:* s z))  (cl:+ gyy c)           (cl:- gyz (cl:* s x))  0f0
       (cl:- gxz (cl:* s y))  (cl:+ gyz (cl:* s x))  (cl:+ gzz c)           0f0
       0f0                    0f0                    0f0                    1f0
       mat-to-mutate))))

;;----------------------------------------------------------------

(defn + ((mat-accum mat4) (mat-b mat4)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for i :below 16 :do
     (cl:incf (aref mat-accum i) (aref mat-b i)))
  mat-accum)

;;----------------------------------------------------------------

(defn - ((mat-accum mat4) (mat-b mat4)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for i :below 16 :do
     (cl:decf (aref mat-accum i) (aref mat-b i)))
  mat-accum)

;;----------------------------------------------------------------

(defn negate ((mat-to-negate mat4)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for i :below 16 :do
     (setf (aref mat-to-negate i) (cl:- (aref mat-to-negate i))))
  mat-to-negate)

;;----------------------------------------------------------------

(defn *s ((mat-to-mutate mat4) (scalar single-float)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for i :below 16 :do
     (setf (aref mat-to-mutate i) (cl:* scalar (aref mat-to-mutate i))))
  mat-to-mutate)

;;----------------------------------------------------------------

(defn *v ((mat-a mat4) (vec4-to-mutate vec4)) vec4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (v4-n:set-components
   (cl:+ (cl:* (x vec4-to-mutate) (melm mat-a 0 0))
         (cl:* (y vec4-to-mutate) (melm mat-a 0 1))
         (cl:* (z vec4-to-mutate) (melm mat-a 0 2))
         (cl:* (w vec4-to-mutate) (melm mat-a 0 3)))
   (cl:+ (cl:* (x vec4-to-mutate) (melm mat-a 1 0))
         (cl:* (y vec4-to-mutate) (melm mat-a 1 1))
         (cl:* (z vec4-to-mutate) (melm mat-a 1 2))
         (cl:* (w vec4-to-mutate) (melm mat-a 1 3)))
   (cl:+ (cl:* (x vec4-to-mutate) (melm mat-a 2 0))
         (cl:* (y vec4-to-mutate) (melm mat-a 2 1))
         (cl:* (z vec4-to-mutate) (melm mat-a 2 2))
         (cl:* (w vec4-to-mutate) (melm mat-a 2 3)))
   (cl:+ (cl:* (x vec4-to-mutate) (melm mat-a 3 0))
         (cl:* (y vec4-to-mutate) (melm mat-a 3 1))
         (cl:* (z vec4-to-mutate) (melm mat-a 3 2))
         (cl:* (w vec4-to-mutate) (melm mat-a 3 3)))
   vec4-to-mutate))

(defn *v3 ((mat-a mat4) (vec3-to-mutate vec3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (v3-n:set-components
   (cl:+ (cl:* (melm mat-a 0 0) (x vec3-to-mutate))
         (cl:* (melm mat-a 0 1) (y vec3-to-mutate))
         (cl:* (melm mat-a 0 2) (z vec3-to-mutate))
         (melm mat-a 0 3))
   (cl:+ (cl:* (melm mat-a 1 0) (x vec3-to-mutate))
         (cl:* (melm mat-a 1 1) (y vec3-to-mutate))
         (cl:* (melm mat-a 1 2) (z vec3-to-mutate))
         (melm mat-a 1 3))
   (cl:+ (cl:* (melm mat-a 2 0) (x vec3-to-mutate))
         (cl:* (melm mat-a 2 1) (y vec3-to-mutate))
         (cl:* (melm mat-a 2 2) (z vec3-to-mutate))
         (melm mat-a 2 3))
   vec3-to-mutate))

;;----------------------------------------------------------------

(defn mrow*vec4 ((vec4-to-mutate vec4) (mat-a mat4)) vec4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (v4-n:set-components
   (cl:+ (cl:* (x vec4-to-mutate) (melm mat-a 0 0))
         (cl:* (y vec4-to-mutate) (melm mat-a 1 0))
         (cl:* (z vec4-to-mutate) (melm mat-a 2 0))
         (cl:* (w vec4-to-mutate) (melm mat-a 3 0)))

   (cl:+ (cl:* (x vec4-to-mutate) (melm mat-a 0 1))
         (cl:* (y vec4-to-mutate) (melm mat-a 1 1))
         (cl:* (z vec4-to-mutate) (melm mat-a 2 1))
         (cl:* (w vec4-to-mutate) (melm mat-a 3 1)))

   (cl:+ (cl:* (x vec4-to-mutate) (melm mat-a 0 2))
         (cl:* (y vec4-to-mutate) (melm mat-a 1 2))
         (cl:* (z vec4-to-mutate) (melm mat-a 2 2))
         (cl:* (w vec4-to-mutate) (melm mat-a 3 2)))

   (cl:+ (cl:* (x vec4-to-mutate) (melm mat-a 0 3))
         (cl:* (y vec4-to-mutate) (melm mat-a 1 3))
         (cl:* (z vec4-to-mutate) (melm mat-a 2 3))
         (cl:* (w vec4-to-mutate) (melm mat-a 3 3)))
   vec4-to-mutate))
