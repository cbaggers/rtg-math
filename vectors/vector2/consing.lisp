(in-package :rtg-math.vector2)

;;----------------------------------------------------------------

(defn make ((x single-float) (y single-float)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (v2-n:set-components x y (make-array 2 :element-type `single-float)))

;;---------------------------------------------------------------

(defn-inline copy-vec2 ((vec2 vec2)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((vec (make-array 2 :element-type `single-float)))
    (setf (aref vec 0) (aref vec2 0)
          (aref vec 1) (aref vec2 1))
    vec))

;;----------------------------------------------------------------

(defn 0p ((vector-a vec2)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:= 0f0 (cl:+ (expt (aref vector-a 0) 2)
                  (expt (aref vector-a 1) 2))))

;;----------------------------------------------------------------

(defn unitp ((vector-a vec2)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:= 0f0 (cl:- 1.0 (cl:+ (expt (aref vector-a 0) 2)
                            (expt (aref vector-a 1) 2)))))
;;----------------------------------------------------------------

(defn = ((vector-a vec2) (vector-b vec2)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (and (cl:= (aref vector-a 0) (aref vector-b 0))
       (cl:= (aref vector-a 1) (aref vector-b 1))))

;;----------------------------------------------------------------

(defn +s ((vec2 vec2) (scalar single-float)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:+ (x vec2) scalar)
        (cl:+ (y vec2) scalar)))

;;----------------------------------------------------------------

(defn -s ((vec2 vec2) (scalar single-float)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:- (x vec2) scalar)
        (cl:- (y vec2) scalar)))

;;----------------------------------------------------------------

(defn %+ ((vector-a vec2) (vector-b vec2)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:+ (aref vector-a 0) (aref vector-b 0))
        (cl:+ (aref vector-a 1) (aref vector-b 1))))

(defn + (&rest (vec2s vec2)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (if vec2s
      (let ((x 0f0)
            (y 0f0))
        (declare (single-float x y))
        (loop :for vec :in vec2s :do
           (let ((vec vec))
             (declare (vec2 vec))
             (cl:incf x (x vec))
             (cl:incf y (y vec))))
        (make x y))
      (make 0f0 0f0)))

(define-compiler-macro + (&whole whole &rest vec2s)
  (case= (cl:length vec2s)
    (0 (make 0f0 0f0))
    (1 (first vec2s))
    (2 `(%+ ,@vec2s))
    (otherwise whole)))

;;----------------------------------------------------------------

(defn %- ((vector-a vec2) (vector-b vec2)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:- (aref vector-a 0) (aref vector-b 0))
        (cl:- (aref vector-a 1) (aref vector-b 1))))

(defn - ((vec2 vec2) &rest (vec2s vec2)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (assert vec2)
  (if vec2s
      (let ((x (x vec2))
            (y (y vec2)))
        (declare (single-float x y))
        (loop :for vec :in vec2s :do
           (let ((vec vec))
             (declare (vec2 vec))
             (cl:decf x (x vec))
             (cl:decf y (y vec))))
        (make x y))
      vec2))

(define-compiler-macro - (&whole whole &rest vec2s)
  (case= (cl:length vec2s)
    (2 `(%- ,@vec2s))
    (otherwise whole)))

;;----------------------------------------------------------------

(defn * (&rest (vec2s vec2)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (if vec2s
      (destructuring-bind (vec2 . vec2s) vec2s
        (declare (vec2 vec2))
        (let ((x (x vec2))
              (y (y vec2)))
          (declare (single-float x y))
          (loop :for vec :in vec2s :do
             (let ((vec vec))
               (declare (vec2 vec))
               (setf x (cl:* x (x vec)))
               (setf y (cl:* y (y vec)))))
          (make x y)))
      (make 1f0 1f0)))

(define-compiler-macro * (&whole whole &rest vec2s)
  (case= (cl:length vec2s)
    (0 `(make 1f0 1f0))
    (1 (first vec2s))
    (2 `(*v ,@vec2s))
    (otherwise whole)))

(defn *v ((vector-a vec2) (vector-b vec2)) vec2
  (declare (vec2 vector-a vector-b))
  (make (cl:* (aref vector-a 0) (aref vector-b 0))
        (cl:* (aref vector-a 1) (aref vector-b 1))))

;;----------------------------------------------------------------

(defn *s ((vector-a vec2) (a single-float)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:* (aref vector-a 0) a)
        (cl:* (aref vector-a 1) a)))

;;----------------------------------------------------------------

(defn /s ((vector-a vec2) (a single-float)) vec2
  (declare (vec2 vector-a)
           (single-float a))
  (let ((b (cl:/ 1 a)))
    (make (cl:* (aref vector-a 0) b) (cl:* (aref vector-a 1) b))))

;;----------------------------------------------------------------

(defn / ((vector-a vec2) (vector-b vec2)) vec2
  (declare (vec2 vector-a vector-b))
  (make (cl:/ (aref vector-a 0)
              (aref vector-b 0))
        (cl:/ (aref vector-a 1)
              (aref vector-b 1))))

;;----------------------------------------------------------------

(defn negate ((vector-a vec2)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:- (aref vector-a 0)) (cl:- (aref vector-a 1))))

;;----------------------------------------------------------------

(defn dot ((vector-a vec2) (vector-b vec2)) single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:+ (cl:* (x vector-a) (x vector-b))
        (cl:* (y vector-a) (y vector-b))))

;;----------------------------------------------------------------

(defn face-foreward ((vector-a vec2) (vector-b vec2)) vec2
  (if (> (dot vector-a vector-b) 0)
      vector-a
      (negate vector-a)))

;;----------------------------------------------------------------

(defn length-squared ((vector-a vec2))
    (single-float 0f0 #.most-positive-single-float)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((x (x vector-a))
        (y (y vector-a)))
    (cl:+ (cl:* x x) (cl:* y y))))

;;----------------------------------------------------------------

(defn length ((vector-a vec2))
    (single-float 0f0 #.most-positive-single-float)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (sqrt (length-squared vector-a)))

;;----------------------------------------------------------------

(defn distance-squared ((vector-a vec2) (vector-b vec2))
    (single-float 0f0 #.most-positive-single-float)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (length-squared (- vector-b vector-a)))

;;----------------------------------------------------------------

(defn distance ((vector-a vec2) (vector-b vec2))
    (single-float 0f0 #.most-positive-single-float)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (sqrt (distance-squared vector-a vector-b)))

;;----------------------------------------------------------------

(defn abs ((vector-a vec2)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:abs (x vector-a)) (cl:abs (y vector-a))))

;;----------------------------------------------------------------

(defn absolute-dot ((vector-a vec2) (vector-b vec2)) single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:+ (cl:abs (cl:* (aref vector-a 0) (aref vector-b 0)))
        (cl:abs (cl:* (aref vector-a 1) (aref vector-b 1)))))

;;----------------------------------------------------------------


(defn normalize ((vector-a vec2)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((len (length-squared vector-a)))
    (if (cl:= 0f0 len)
        vector-a
        (*s vector-a (inv-sqrt len)))))

;;----------------------------------------------------------------

(defn perp-dot ((vec-a vec2) (vec-b vec2)) single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:- (cl:* (x vec-a) (y vec-b)) (cl:* (y vec-a) (x vec-b))))

;;----------------------------------------------------------------

(defn cross ((vec-a vec2) (vec-b vec2)) single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:- (cl:* (x vec-a) (y vec-b))
        (cl:* (y vec-a) (x vec-b))))

;;----------------------------------------------------------------

(defn lerp ((vector-a vec2) (vector-b vec2) (ammount single-float)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%+ (*s vector-a (cl:- 1f0 ammount))
      (*s vector-b ammount)))


(defn stable-lerp ((vector-a vec2) (vector-b vec2) (ammount single-float)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (lerp vector-a vector-b ammount))

;;----------------------------------------------------------------

(defn bezier ((a1 vec2) (a2 vec2)
              (b1 vec2) (b2 vec2) (ammount single-float)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (lerp (lerp a1 a2 ammount)
        (lerp b1 b2 ammount)
        ammount))

;;----------------------------------------------------------------

(defun spline (x knots)
  (make (rtg-math.base-maths:spline x (mapcar #'x knots))
        (rtg-math.base-maths:spline x (mapcar #'y knots))))

;;----------------------------------------------------------------

(defn from-complex ((c complex)) vec2
  (make (coerce (realpart c) 'single-float)
        (coerce (imagpart c) 'single-float)))

;;---------------------------------------------------------------

(defn from-angle ((angle single-float)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((a (cl:- angle)))
    (make (sin a)
          (cos a))))

;;---------------------------------------------------------------

(defn angle-from ((vec-from vec2)
                  (vec-to vec2))
    single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (atan (cross vec-from vec-to)
        (dot vec-from vec-to)))

(defn angle-between ((vec-a vec2)
                     (vec-b vec2))
    single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:abs (angle-from vec-a vec-b)))

;;---------------------------------------------------------------

(defn rotate ((vec vec2) (angle single-float)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (v2-n:rotate (copy-vec2 vec) angle))

;;---------------------------------------------------------------

(defmacro incf (place &optional delta)
  (let ((vec (gensym "PLACE"))
        (val (or delta (make-array 2 :element-type 'single-float
                                   :initial-contents '(1f0 1f0)))))
    `(let ((,vec ,place))
       (v2-n:+ ,vec ,val))))

(defmacro decf (place &optional delta)
  (let ((vec (gensym "PLACE"))
        (val (or delta (make-array 2 :element-type 'single-float
                                   :initial-contents '(1f0 1f0)))))
    `(let ((,vec ,place))
       (v2-n:- ,vec ,val))))

;;---------------------------------------------------------------
