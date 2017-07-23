(in-package :rtg-math.matrix2.non-consing)

;;----------------------------------------------------------------

(defn-inline set-components ((a single-float) (b single-float) (c single-float)
                             (d single-float) (mat2-to-mutate mat2))
    mat2
  "Make a 2x2 matrix. Data must be provided in row major order"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (melm mat2-to-mutate 0 0) a)
  (setf (melm mat2-to-mutate 0 1) b)
  (setf (melm mat2-to-mutate 1 0) c)
  (setf (melm mat2-to-mutate 1 1) d)
  mat2-to-mutate)

;;----------------------------------------------------------------

(defn %* ((mat-accum mat2) (to-multiply-mat mat2)) mat2
  "Multiplies 2 matrices and returns the result as a new
   matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((a (cl:+ (cl:* (melm mat-accum 0 0) (melm to-multiply-mat 0 0))
                 (cl:* (melm mat-accum 0 1) (melm to-multiply-mat 1 0))))
        (b (cl:+ (cl:* (melm mat-accum 1 0) (melm to-multiply-mat 0 0))
                 (cl:* (melm mat-accum 1 1) (melm to-multiply-mat 1 0))))
        (c (cl:+ (cl:* (melm mat-accum 0 0) (melm to-multiply-mat 0 1))
                 (cl:* (melm mat-accum 0 1) (melm to-multiply-mat 1 1))))
        (d (cl:+ (cl:* (melm mat-accum 1 0) (melm to-multiply-mat 0 1))
                 (cl:* (melm mat-accum 1 1) (melm to-multiply-mat 1 1)))))
    (setf (melm mat-accum 0 0) a)
    (setf (melm mat-accum 0 1) b)
    (setf (melm mat-accum 1 0) c)
    (setf (melm mat-accum 1 1) d)
    mat-accum))

(defn * ((accum-mat mat2) &rest (mat2s mat2)) mat2
  (reduce #'%* mat2s :initial-value accum-mat))

(define-compiler-macro * (&whole whole accum-mat &rest mat2s)
  (assert accum-mat)
  (case= (cl:length mat2s)
    (0 accum-mat)
    (1 `(%* ,accum-mat ,(first mat2s)))
    (otherwise whole)))

;;----------------------------------------------------------------

(defn set-from-rows ((mat-to-mutate mat2) (row-1 vec2) (row-2 vec2)) mat2
  "Make a 3x3 matrix using the data in the 3 vector3s provided
   to populate the rows"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (x row-1) (y row-1)
                  (x row-2) (y row-2)
                  mat-to-mutate))

;;----------------------------------------------------------------

(defn set-from-columns ((mat-to-mutate mat2) (col-1 vec3) (col-2 vec3)) mat2
  "Make a 3x3 matrix using the data in the 3 vector3s provided
   to populate the columns"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (x col-1) (x col-2)
                  (y col-1) (y col-2)
                  mat-to-mutate))

;;----------------------------------------------------------------

;; (defn affine-inverse ((mat-to-invert mat2)) mat2
;;   "returns the inverse of the matrix"
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (let* ((cofactor-0 (cl:- (cl:* (melm mat-to-invert 1 1) (melm mat-to-invert 2 2))
;;                            (cl:* (melm mat-to-invert 2 1) (melm mat-to-invert 1 2))))

;;          (cofactor-3 (cl:- (cl:* (melm mat-to-invert 2 0) (melm mat-to-invert 1 2))
;;                            (cl:* (melm mat-to-invert 1 0) (melm mat-to-invert 2 2))))

;;          (cofactor-6 (cl:- (cl:* (melm mat-to-invert 1 0) (melm mat-to-invert 2 1))
;;                            (cl:* (melm mat-to-invert 2 0) (melm mat-to-invert 1 1))))
;;          (det (cl:+ (cl:* (melm mat-to-invert 0 0) cofactor-0)
;;                     (cl:* (melm mat-to-invert 0 1) cofactor-3)
;;                     (cl:* (melm mat-to-invert 0 2) cofactor-6))))
;;     (if (cl:= 0f0 det)
;;         (error "Matrix4 Inverse: Singular Matrix")
;;         (let*
;;             ((inv-det (cl:/ 1.0 det))
;;              (r00 (cl:* inv-det cofactor-0))
;;              (r10 (cl:* inv-det cofactor-3))
;;              (r20 (cl:* inv-det cofactor-6))
;;              (r01 (cl:* inv-det (cl:- (cl:* (melm mat-to-invert 2 1) (melm mat-to-invert 0 2))
;;                                       (cl:* (melm mat-to-invert 0 1) (melm mat-to-invert 2 2)))))
;;              (r11 (cl:* inv-det (cl:- (cl:* (melm mat-to-invert 0 0) (melm mat-to-invert 2 2))
;;                                       (cl:* (melm mat-to-invert 2 0) (melm mat-to-invert 0 2)))))
;;              (r21 (cl:* inv-det (cl:- (cl:* (melm mat-to-invert 2 0) (melm mat-to-invert 0 1))
;;                                       (cl:* (melm mat-to-invert 0 0) (melm mat-to-invert 2 1)))))
;;              (r02 (cl:* inv-det (cl:- (cl:* (melm mat-to-invert 0 1) (melm mat-to-invert 1 2))
;;                                       (cl:* (melm mat-to-invert 1 1) (melm mat-to-invert 0 2)))))
;;              (r12 (cl:* inv-det (cl:- (cl:* (melm mat-to-invert 1 0) (melm mat-to-invert 0 2))
;;                                       (cl:* (melm mat-to-invert 0 0) (melm mat-to-invert 1 2)))))
;;              (r22 (cl:* inv-det (cl:- (cl:* (melm mat-to-invert 0 0) (melm mat-to-invert 1 1))
;;                                       (cl:* (melm mat-to-invert 1 0) (melm mat-to-invert 0 1))))))
;;           (set-components r00 r01 r02
;;                           r10 r11 r12
;;                           r20 r21 r22
;;                           mat-to-invert)))))

;;----------------------------------------------------------------

(defn transpose ((mat-to-transpose mat2)) mat2
  "Returns the transpose of the provided matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components
   (melm mat-to-transpose 0 0) (melm mat-to-transpose 1 0)
   (melm mat-to-transpose 0 1) (melm mat-to-transpose 1 1)
   mat-to-transpose))

;;----------------------------------------------------------------

;; (defn adjoint ((mat-to-mutate mat2)) mat2
;;   "Returns the adjoint of the matrix"
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (set-components
;;    (cl:- (cl:* (melm mat-to-mutate 1 1) (melm mat-to-mutate 2 2))
;;          (cl:* (melm mat-to-mutate 1 2) (melm mat-to-mutate 2 1)))
;;    (cl:- (cl:* (melm mat-to-mutate 0 2) (melm mat-to-mutate 2 1))
;;          (cl:* (melm mat-to-mutate 0 1) (melm mat-to-mutate 2 2)))
;;    (cl:- (cl:* (melm mat-to-mutate 0 1) (melm mat-to-mutate 1 2))
;;          (cl:* (melm mat-to-mutate 0 2) (melm mat-to-mutate 1 1)))
;;    (cl:- (cl:* (melm mat-to-mutate 1 2) (melm mat-to-mutate 2 0))
;;          (cl:* (melm mat-to-mutate 1 0) (melm mat-to-mutate 2 2)))
;;    (cl:- (cl:* (melm mat-to-mutate 0 0) (melm mat-to-mutate 2 2))
;;          (cl:* (melm mat-to-mutate 0 2) (melm mat-to-mutate 2 0)))
;;    (cl:- (cl:* (melm mat-to-mutate 0 2) (melm mat-to-mutate 1 0))
;;          (cl:* (melm mat-to-mutate 0 0) (melm mat-to-mutate 1 2)))
;;    (cl:- (cl:* (melm mat-to-mutate 1 0) (melm mat-to-mutate 2 1))
;;          (cl:* (melm mat-to-mutate 1 1) (melm mat-to-mutate 2 0)))
;;    (cl:- (cl:* (melm mat-to-mutate 0 1) (melm mat-to-mutate 2 0))
;;          (cl:* (melm mat-to-mutate 0 0) (melm mat-to-mutate 2 1)))
;;    (cl:- (cl:* (melm mat-to-mutate 0 0) (melm mat-to-mutate 1 1))
;;          (cl:* (melm mat-to-mutate 0 1) (melm mat-to-mutate 1 0)))
;;    mat-to-mutate))

;;----------------------------------------------------------------

;; (defn set-rotation-from-euler ((mat-to-mutate mat2) (vec3-a vec3)) mat2
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (let ((x (x vec3-a)) (y (y vec3-a)) (z (z vec3-a)))
;;     (let ((sx (sin x)) (cx (cos x))
;;           (sy (sin y)) (cy (cos y))
;;           (sz (sin z)) (cz (cos z)))
;;       (set-components (cl:* cy cz)
;;                       (cl:- (cl:* cy sz))
;;                       sy

;;                       (cl:+ (cl:* sx sy cz) (cl:* cx sz))
;;                       (cl:- (cl:* cx cz) (cl:* sx sy sz))
;;                       (cl:- (cl:* sx cy))

;;                       (cl:- (cl:* sx sz) (cl:* cx sy cz))
;;                       (cl:+ (cl:* cx sy sz) (cl:* sx cz))
;;                       (cl:* cx cy)
;;                       mat-to-mutate))))

;;----------------------------------------------------------------

(defn set-from-scale ((mat-to-mutate mat2) (scale-vec2 vec2)) mat2
  "Returns a matrix which will scale by the amounts specified"
  (declare (optimize (speed 2) (safety 1) (debug 1)))
  (set-components (x scale-vec2)  0.0
                  0.0             (y scale-vec2)
                  mat-to-mutate))

;;----------------------------------------------------------------

;; (defn set-from-rotation-x ((mat-to-mutate mat2) (angle single-float)) mat2
;;   "Returns a matrix which would rotate a point around the x axis
;;    by the specified amount"
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (let ((s-a (sin angle))
;;         (c-a (cos angle)))
;;     (set-components 1.0  0.0  0.0
;;                     0.0  c-a  (cl:- s-a)
;;                     0.0  s-a  c-a
;;                     mat-to-mutate)))

;;----------------------------------------------------------------

;; (defn set-from-rotation-y ((mat-to-mutate mat2) (angle single-float)) mat2
;;   "Returns a matrix which would rotate a point around the y axis
;;    by the specified amount"
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (let ((s-a (sin angle))
;;         (c-a (cos angle)))
;;     (set-components c-a         0.0    s-a
;;                     0.0         1.0    0.0
;;                     (cl:- s-a)  0.0    c-a
;;                     mat-to-mutate)))

;;----------------------------------------------------------------

;; (defn set-from-rotation-z ((mat-to-mutate mat2) (angle single-float)) mat2
;;   "Returns a matrix which would rotate a point around the z axis
;;    by the specified amount"
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (let ((s-a (sin angle))
;;         (c-a (cos angle)))
;;     (set-components c-a  (cl:- s-a)  0.0
;;                     s-a  c-a         0.0
;;                     0.0  0.0         1.0
;;                     mat-to-mutate)))

;;----------------------------------------------------------------

;; (defn set-rotation-from-axis-angle
;;     ((mat-to-mutate mat2) (axis3 vec3) (angle single-float)) mat2
;;   "Returns a matrix which will rotate a point about the axis
;;    specified by the angle provided"
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (cond ((v3:= axis3 (v! 1f0 0f0 0f0))
;;          (set-from-rotation-x mat-to-mutate angle))
;;         ((v3:= axis3 (v! 0f0 1f0 0f0))
;;          (set-from-rotation-y mat-to-mutate angle))
;;         ((v3:= axis3 (v! 0f0 0f0 1f0))
;;          (set-from-rotation-z mat-to-mutate angle))
;;         (t
;;          (let ((c (cos angle))
;;                (s (sin angle))
;;                (g (cl:- 1f0 (cos angle))))
;;            (let* ((x (x axis3))
;;                   (y (y axis3))
;;                   (z (z axis3))
;;                   (gxx (cl:* g x x)) (gxy (cl:* g x y)) (gxz (cl:* g x z))
;;                   (gyy (cl:* g y y)) (gyz (cl:* g y z)) (gzz (cl:* g z z)))
;;              (set-components
;;               (cl:+ gxx c)        (cl:- gxy (cl:* s z))  (cl:+ gxz (cl:* s y))
;;               (cl:+ gxy (cl:* s z))  (cl:+ gyy c)        (cl:- gyz (cl:* s x))
;;               (cl:- gxz (cl:* s y))  (cl:+ gyz (cl:* s x))  (cl:+ gzz c)
;;               mat-to-mutate))))))

;;----------------------------------------------------------------

(defn %+ ((accum-vec mat2) (to-add-vec mat2)) mat2
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:incf (aref accum-vec 0) (aref to-add-vec 0))
  (cl:incf (aref accum-vec 1) (aref to-add-vec 1))
  (cl:incf (aref accum-vec 2) (aref to-add-vec 2))
  (cl:incf (aref accum-vec 3) (aref to-add-vec 3))
  accum-vec)

(defn + ((accum-vec mat2) &rest (mat2s mat2)) mat2
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for vec :in mat2s :do (%+ accum-vec vec))
  accum-vec)

(define-compiler-macro + (&whole whole accum-vec &rest mat2s)
  (assert accum-vec)
  (case= (cl:length mat2s)
    (0 accum-vec)
    (1 `(%+ ,accum-vec ,(first mat2s)))
    (otherwise whole)))

;;----------------------------------------------------------------

(defn %- ((accum-vec mat2) (to-add-vec mat2)) mat2
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:decf (aref accum-vec 0) (aref to-add-vec 0))
  (cl:decf (aref accum-vec 1) (aref to-add-vec 1))
  (cl:decf (aref accum-vec 2) (aref to-add-vec 2))
  (cl:decf (aref accum-vec 3) (aref to-add-vec 3))
  accum-vec)

(defn - ((accum-vec mat2) &rest (mat2s mat2)) mat2
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for vec :in mat2s :do (%- accum-vec vec))
  accum-vec)

(define-compiler-macro - (&whole whole accum-vec &rest mat2s)
  (assert accum-vec)
  (case= (cl:length mat2s)
    (0 accum-vec)
    (1 `(%- ,accum-vec ,(first mat2s)))
    (otherwise whole)))

;;----------------------------------------------------------------

(defn negate ((vector-a mat2)) mat2
  "Return a vector that is the negative of the vector passed in"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (cl:- (x vector-a))
                  (cl:- (y vector-a))
                  (cl:- (z vector-a))
                  (cl:- (w vector-a))
                  vector-a))

;;----------------------------------------------------------------

;; (defn *v ((mat-a mat2) (vec2-to-mutate vec2)) vec2
;;   "Multiplies the vector2 by the matrix and returning the mutated vector2"
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (v2-n:set-components (cl:+ (cl:* (x vec2-to-mutate) (melm mat-a 0 0))
;;                              (cl:* (y vec2-to-mutate) (melm mat-a 0 1)))
;;                        (cl:+ (cl:* (x vec2-to-mutate) (melm mat-a 1 0))
;;                              (cl:* (y vec2-to-mutate) (melm mat-a 1 1)))
;;                        vec2-to-mutate))

;;----------------------------------------------------------------

;; (defn mrow*vec3 ((vec3-to-mutate vec3) (mat-a mat2)) vec3
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (v3-n:set-components (cl:+ (cl:* (x vec3-to-mutate) (melm mat-a 0 0))
;;                              (cl:* (y vec3-to-mutate) (melm mat-a 1 0))
;;                              (cl:* (z vec3-to-mutate) (melm mat-a 2 0)))
;;                        (cl:+ (cl:* (x vec3-to-mutate) (melm mat-a 0 1))
;;                              (cl:* (y vec3-to-mutate) (melm mat-a 1 1))
;;                              (cl:* (z vec3-to-mutate) (melm mat-a 2 1)))
;;                        (cl:+ (cl:* (x vec3-to-mutate) (melm mat-a 0 2))
;;                              (cl:* (y vec3-to-mutate) (melm mat-a 1 2))
;;                              (cl:* (z vec3-to-mutate) (melm mat-a 2 2)))
;;                        vec3-to-mutate))

;;----------------------------------------------------------------

(defn *s ((mat2 mat2) (a single-float)) mat2
  "Multiply vector by scalar"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (x mat2) (cl:* (x mat2) a))
  (setf (y mat2) (cl:* (y mat2) a))
  (setf (z mat2) (cl:* (z mat2) a))
  (setf (w mat2) (cl:* (w mat2) a))
  mat2)
