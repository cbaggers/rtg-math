(in-package :rtg-math.matrix3.non-consing)

;;----------------------------------------------------------------

(defn-inline set-components ((a single-float) (b single-float) (c single-float)
                             (d single-float) (e single-float) (f single-float)
                             (g single-float) (h single-float) (i single-float)
                             (mat3-to-mutate mat3)) mat3
  "Make a 3x3 matrix. Data must be provided in row major order"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (melm mat3-to-mutate 0 0) a)
  (setf (melm mat3-to-mutate 0 1) b)
  (setf (melm mat3-to-mutate 0 2) c)
  (setf (melm mat3-to-mutate 1 0) d)
  (setf (melm mat3-to-mutate 1 1) e)
  (setf (melm mat3-to-mutate 1 2) f)
  (setf (melm mat3-to-mutate 2 0) g)
  (setf (melm mat3-to-mutate 2 1) h)
  (setf (melm mat3-to-mutate 2 2) i)
  mat3-to-mutate)

;;----------------------------------------------------------------

(defn %* ((mat-accum mat3) (to-multiply-mat mat3)) mat3
  "Multiplies 2 matrices and returns the result as a new
   matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((a (cl:+ (cl:* (melm mat-accum 0 0) (melm to-multiply-mat 0 0))
                 (cl:* (melm mat-accum 0 1) (melm to-multiply-mat 1 0))
                 (cl:* (melm mat-accum 0 2) (melm to-multiply-mat 2 0))))
        (b (cl:+ (cl:* (melm mat-accum 1 0) (melm to-multiply-mat 0 0))
                 (cl:* (melm mat-accum 1 1) (melm to-multiply-mat 1 0))
                 (cl:* (melm mat-accum 1 2) (melm to-multiply-mat 2 0))))
        (c (cl:+ (cl:* (melm mat-accum 2 0) (melm to-multiply-mat 0 0))
                 (cl:* (melm mat-accum 2 1) (melm to-multiply-mat 1 0))
                 (cl:* (melm mat-accum 2 2) (melm to-multiply-mat 2 0))))
        (d (cl:+ (cl:* (melm mat-accum 0 0) (melm to-multiply-mat 0 1))
                 (cl:* (melm mat-accum 0 1) (melm to-multiply-mat 1 1))
                 (cl:* (melm mat-accum 0 2) (melm to-multiply-mat 2 1))))
        (e (cl:+ (cl:* (melm mat-accum 1 0) (melm to-multiply-mat 0 1))
                 (cl:* (melm mat-accum 1 1) (melm to-multiply-mat 1 1))
                 (cl:* (melm mat-accum 1 2) (melm to-multiply-mat 2 1))))
        (f (cl:+ (cl:* (melm mat-accum 2 0) (melm to-multiply-mat 0 1))
                 (cl:* (melm mat-accum 2 1) (melm to-multiply-mat 1 1))
                 (cl:* (melm mat-accum 2 2) (melm to-multiply-mat 2 1))))
        (g (cl:+ (cl:* (melm mat-accum 0 0) (melm to-multiply-mat 0 2))
                 (cl:* (melm mat-accum 0 1) (melm to-multiply-mat 1 2))
                 (cl:* (melm mat-accum 0 2) (melm to-multiply-mat 2 2))))
        (h (cl:+ (cl:* (melm mat-accum 0 1) (melm to-multiply-mat 0 2))
                 (cl:* (melm mat-accum 1 1) (melm to-multiply-mat 1 2))
                 (cl:* (melm mat-accum 1 2) (melm to-multiply-mat 2 2))))
        (i (cl:+ (cl:* (melm mat-accum 2 0) (melm to-multiply-mat 0 2))
                 (cl:* (melm mat-accum 2 1) (melm to-multiply-mat 1 2))
                 (cl:* (melm mat-accum 2 2) (melm to-multiply-mat 2 2)))))
    (setf (melm mat-accum 0 0) a)
    (setf (melm mat-accum 0 1) b)
    (setf (melm mat-accum 0 2) c)
    (setf (melm mat-accum 1 0) d)
    (setf (melm mat-accum 1 1) e)
    (setf (melm mat-accum 1 2) f)
    (setf (melm mat-accum 2 0) g)
    (setf (melm mat-accum 2 1) h)
    (setf (melm mat-accum 2 2) i)
    mat-accum))

(defn * ((accum-mat mat3) &rest (mat3s mat3)) mat3
  (reduce #'%* mat3s :initial-value accum-mat))

(define-compiler-macro * (&whole whole accum-mat &rest mat4s)
  (assert accum-mat)
  (case= (cl:length mat4s)
    (0 accum-mat)
    (1 `(%* ,accum-mat ,(first mat4s)))
    (otherwise whole)))

;;----------------------------------------------------------------

(defn set-from-rows ((mat-to-mutate mat3)
                     (row-1 vec3) (row-2 vec3) (row-3 vec3)) mat3
  "Make a 3x3 matrix using the data in the 3 vector3s provided
   to populate the rows"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (x row-1) (y row-1) (z row-1)
                  (x row-2) (y row-2) (z row-2)
                  (x row-3) (y row-3) (z row-3)
                  mat-to-mutate))

;;----------------------------------------------------------------

(defn set-from-columns ((mat-to-mutate mat3)
                        (col-1 vec3) (col-2 vec3) (col-3 vec3)) mat3
  "Make a 3x3 matrix using the data in the 3 vector3s provided
   to populate the columns"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (x col-1) (x col-2) (x col-3)
                  (y col-1) (y col-2) (y col-3)
                  (z col-1) (z col-2) (z col-3)
                  mat-to-mutate))

;;----------------------------------------------------------------

(defn affine-inverse ((mat-to-invert mat3)) mat3
  "returns the inverse of the matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((cofactor-0 (cl:- (cl:* (melm mat-to-invert 1 1) (melm mat-to-invert 2 2))
                           (cl:* (melm mat-to-invert 2 1) (melm mat-to-invert 1 2))))

         (cofactor-3 (cl:- (cl:* (melm mat-to-invert 2 0) (melm mat-to-invert 1 2))
                           (cl:* (melm mat-to-invert 1 0) (melm mat-to-invert 2 2))))

         (cofactor-6 (cl:- (cl:* (melm mat-to-invert 1 0) (melm mat-to-invert 2 1))
                           (cl:* (melm mat-to-invert 2 0) (melm mat-to-invert 1 1))))
         (det (cl:+ (cl:* (melm mat-to-invert 0 0) cofactor-0)
                    (cl:* (melm mat-to-invert 0 1) cofactor-3)
                    (cl:* (melm mat-to-invert 0 2) cofactor-6))))
    (if (cl:= 0f0 det)
        (error "Matrix4 Inverse: Singular Matrix")
        (let*
            ((inv-det (cl:/ 1.0 det))
             (r00 (cl:* inv-det cofactor-0))
             (r10 (cl:* inv-det cofactor-3))
             (r20 (cl:* inv-det cofactor-6))
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
          (set-components r00 r01 r02
                          r10 r11 r12
                          r20 r21 r22
                          mat-to-invert)))))

;;----------------------------------------------------------------

(defn transpose ((mat-to-transpose mat3)) mat3
  "Returns the transpose of the provided matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components
   (melm mat-to-transpose 0 0) (melm mat-to-transpose 1 0) (melm mat-to-transpose 2 0)
   (melm mat-to-transpose 0 1) (melm mat-to-transpose 1 1) (melm mat-to-transpose 2 1)
   (melm mat-to-transpose 0 2) (melm mat-to-transpose 1 2) (melm mat-to-transpose 2 2)
   mat-to-transpose))

;;----------------------------------------------------------------

(defn adjoint ((mat-to-mutate mat3)) mat3
  "Returns the adjoint of the matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components
   (cl:- (cl:* (melm mat-to-mutate 1 1) (melm mat-to-mutate 2 2))
         (cl:* (melm mat-to-mutate 1 2) (melm mat-to-mutate 2 1)))
   (cl:- (cl:* (melm mat-to-mutate 0 2) (melm mat-to-mutate 2 1))
         (cl:* (melm mat-to-mutate 0 1) (melm mat-to-mutate 2 2)))
   (cl:- (cl:* (melm mat-to-mutate 0 1) (melm mat-to-mutate 1 2))
         (cl:* (melm mat-to-mutate 0 2) (melm mat-to-mutate 1 1)))
   (cl:- (cl:* (melm mat-to-mutate 1 2) (melm mat-to-mutate 2 0))
         (cl:* (melm mat-to-mutate 1 0) (melm mat-to-mutate 2 2)))
   (cl:- (cl:* (melm mat-to-mutate 0 0) (melm mat-to-mutate 2 2))
         (cl:* (melm mat-to-mutate 0 2) (melm mat-to-mutate 2 0)))
   (cl:- (cl:* (melm mat-to-mutate 0 2) (melm mat-to-mutate 1 0))
         (cl:* (melm mat-to-mutate 0 0) (melm mat-to-mutate 1 2)))
   (cl:- (cl:* (melm mat-to-mutate 1 0) (melm mat-to-mutate 2 1))
         (cl:* (melm mat-to-mutate 1 1) (melm mat-to-mutate 2 0)))
   (cl:- (cl:* (melm mat-to-mutate 0 1) (melm mat-to-mutate 2 0))
         (cl:* (melm mat-to-mutate 0 0) (melm mat-to-mutate 2 1)))
   (cl:- (cl:* (melm mat-to-mutate 0 0) (melm mat-to-mutate 1 1))
         (cl:* (melm mat-to-mutate 0 1) (melm mat-to-mutate 1 0)))
   mat-to-mutate))

;;----------------------------------------------------------------

(defn set-rotation-from-euler ((mat-to-mutate mat3) (vec3-a vec3)) mat3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((x (x vec3-a)) (y (y vec3-a)) (z (z vec3-a)))
    (let ((sx (sin x)) (cx (cos x))
          (sy (sin y)) (cy (cos y))
          (sz (sin z)) (cz (cos z)))
      (set-components (cl:* cy cz)
                      (cl:- (cl:* cy sz))
                      sy

                      (cl:+ (cl:* sx sy cz) (cl:* cx sz))
                      (cl:- (cl:* cx cz) (cl:* sx sy sz))
                      (cl:- (cl:* sx cy))

                      (cl:- (cl:* sx sz) (cl:* cx sy cz))
                      (cl:+ (cl:* cx sy sz) (cl:* sx cz))
                      (cl:* cx cy)
                      mat-to-mutate))))

;;----------------------------------------------------------------

(defn set-from-scale ((mat-to-mutate mat3) (scale-vec3 vec3)) mat3
  "Returns a matrix which will scale by the amounts specified"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (x scale-vec3)  0.0               0.0
                  0.0               (y scale-vec3)  0.0
                  0.0               0.0               (z scale-vec3)
                  mat-to-mutate))

;;----------------------------------------------------------------

(defn set-from-rotation-x ((mat-to-mutate mat3) (angle single-float)) mat3
  "Returns a matrix which would rotate a point around the x axis
   by the specified amount"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (set-components 1.0  0.0  0.0
                    0.0  c-a  (cl:- s-a)
                    0.0  s-a  c-a
                    mat-to-mutate)))

;;----------------------------------------------------------------

(defn set-from-rotation-y ((mat-to-mutate mat3) (angle single-float)) mat3
  "Returns a matrix which would rotate a point around the y axis
   by the specified amount"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (set-components c-a         0.0    s-a
                    0.0         1.0    0.0
                    (cl:- s-a)  0.0    c-a
                    mat-to-mutate)))

;;----------------------------------------------------------------

(defn set-from-rotation-z ((mat-to-mutate mat3) (angle single-float)) mat3
  "Returns a matrix which would rotate a point around the z axis
   by the specified amount"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (set-components c-a  (cl:- s-a)  0.0
                    s-a  c-a         0.0
                    0.0  0.0         1.0
                    mat-to-mutate)))

;;----------------------------------------------------------------

(defn set-rotation-from-axis-angle
    ((mat-to-mutate mat3) (axis3 vec3) (angle single-float)) mat3
  "Returns a matrix which will rotate a point about the axis
   specified by the angle provided"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cond ((v3:= axis3 (v! 1f0 0f0 0f0))
         (set-from-rotation-x mat-to-mutate angle))
        ((v3:= axis3 (v! 0f0 1f0 0f0))
         (set-from-rotation-y mat-to-mutate angle))
        ((v3:= axis3 (v! 0f0 0f0 1f0))
         (set-from-rotation-z mat-to-mutate angle))
        (t
         (let ((c (cos angle))
               (s (sin angle))
               (g (cl:- 1f0 (cos angle))))
           (let* ((x (x axis3))
                  (y (y axis3))
                  (z (z axis3))
                  (gxx (cl:* g x x)) (gxy (cl:* g x y)) (gxz (cl:* g x z))
                  (gyy (cl:* g y y)) (gyz (cl:* g y z)) (gzz (cl:* g z z)))
             (set-components
              (cl:+ gxx c)        (cl:- gxy (cl:* s z))  (cl:+ gxz (cl:* s y))
              (cl:+ gxy (cl:* s z))  (cl:+ gyy c)        (cl:- gyz (cl:* s x))
              (cl:- gxz (cl:* s y))  (cl:+ gyz (cl:* s x))  (cl:+ gzz c)
              mat-to-mutate))))))

;;----------------------------------------------------------------

(defn + ((mat-accum mat3) (mat-b mat3)) mat3
  "Add the second matrix component wise to the first and return
   the first"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for i :below 9 :do
     (cl:incf (aref mat-accum i) (aref mat-b i)))
  mat-accum)

;;----------------------------------------------------------------

(defn - ((mat-accum mat3) (mat-b mat3)) mat3
  "Subtracts the second matrix component wise from the first and return
   the first"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for i :below 9 :do
     (cl:decf (aref mat-accum i) (aref mat-b i)))
  mat-accum)

;;----------------------------------------------------------------

(defn negate ((mat-to-negate mat3)) mat3
  "Negates the components of the matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for i :below 9 :do
     (setf (aref mat-to-negate i) (cl:- (aref mat-to-negate i))))
  mat-to-negate)

;;----------------------------------------------------------------

(defn *v ((mat-a mat3) (vec3-to-mutate vec3)) vec3
  "Multiplies the vector3 by the matrix and returning the mutated vector3"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (v3-n:set-components (cl:+ (cl:* (x vec3-to-mutate) (melm mat-a 0 0))
                             (cl:* (y vec3-to-mutate) (melm mat-a 0 1))
                             (cl:* (z vec3-to-mutate) (melm mat-a 0 2)))
                       (cl:+ (cl:* (x vec3-to-mutate) (melm mat-a 1 0))
                             (cl:* (y vec3-to-mutate) (melm mat-a 1 1))
                             (cl:* (z vec3-to-mutate) (melm mat-a 1 2)))
                       (cl:+ (cl:* (x vec3-to-mutate) (melm mat-a 2 0))
                             (cl:* (y vec3-to-mutate) (melm mat-a 2 1))
                             (cl:* (z vec3-to-mutate) (melm mat-a 2 2)))
                       vec3-to-mutate))

;;----------------------------------------------------------------

(defn mrow*vec3 ((vec3-to-mutate vec3) (mat-a mat3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (v3-n:set-components (cl:+ (cl:* (x vec3-to-mutate) (melm mat-a 0 0))
                             (cl:* (y vec3-to-mutate) (melm mat-a 1 0))
                             (cl:* (z vec3-to-mutate) (melm mat-a 2 0)))

                       (cl:+ (cl:* (x vec3-to-mutate) (melm mat-a 0 1))
                             (cl:* (y vec3-to-mutate) (melm mat-a 1 1))
                             (cl:* (z vec3-to-mutate) (melm mat-a 2 1)))

                       (cl:+ (cl:* (x vec3-to-mutate) (melm mat-a 0 2))
                             (cl:* (y vec3-to-mutate) (melm mat-a 1 2))
                             (cl:* (z vec3-to-mutate) (melm mat-a 2 2)))
                       vec3-to-mutate))

;;----------------------------------------------------------------

(defn *s ((mat-to-mutate mat3) (scalar single-float)) mat3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  "Multiplies the components of the matrix by the scalar
   provided"
  (loop :for i :below 9 :do
     (setf (aref mat-to-mutate i) (cl:* scalar (aref mat-to-mutate i))))
  mat-to-mutate)
