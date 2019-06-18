(in-package :rtg-math.matrix2.non-consing)

;;----------------------------------------------------------------

(defn-inline set-components ((c00 single-float) (c01 single-float)
                             (c10 single-float) (c11 single-float)
                             (mat2-to-mutate mat2)) mat2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (melm mat2-to-mutate 0 0) c00)
  (setf (melm mat2-to-mutate 0 1) c01)
  (setf (melm mat2-to-mutate 1 0) c10)
  (setf (melm mat2-to-mutate 1 1) c11)
  mat2-to-mutate)

;;----------------------------------------------------------------

(defn-inline copy-components ((mat mat2) (copy-from mat2)) mat2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (melm mat 0 0) (melm copy-from 0 0))
  (setf (melm mat 0 1) (melm copy-from 0 1))
  (setf (melm mat 1 0) (melm copy-from 1 0))
  (setf (melm mat 1 1) (melm copy-from 1 1))
  mat)

;;----------------------------------------------------------------

(defn %* ((mat-accum mat2) (to-multiply-mat mat2)) mat2
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

(define-compiler-macro * (&whole whole accum-mat &rest mat4s)
  (assert accum-mat)
  (case= (cl:length mat4s)
    (0 accum-mat)
    (1 `(%* ,accum-mat ,(first mat4s)))
    (otherwise whole)))

;;----------------------------------------------------------------

(defn set-from-rows ((mat-to-mutate mat2)
                     (row-1 vec2)
                     (row-2 vec2)) mat2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (x row-1) (y row-1)
                  (x row-2) (y row-2)
                  mat-to-mutate))

;;----------------------------------------------------------------

(defn set-from-columns ((mat-to-mutate mat2)
                        (col-1 vec2)
                        (col-2 vec2)) mat2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (x col-1) (x col-2)
                  (y col-1) (y col-2)
                  mat-to-mutate))

;;----------------------------------------------------------------

(defn transpose ((mat-to-transpose mat2)) mat2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components
   (melm mat-to-transpose 0 0) (melm mat-to-transpose 1 0)
   (melm mat-to-transpose 0 1) (melm mat-to-transpose 1 1)
   mat-to-transpose))

;;----------------------------------------------------------------

;; adj
;; [ d −c
;;  −b  a]
(defn adjoint ((mat-to-mutate mat2)) mat2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components
   (melm mat-to-mutate 1 1)
   (cl:- (melm mat-to-mutate 1 0))
   (cl:- (melm mat-to-mutate 0 1))
   (melm mat-to-mutate 0 0)
   mat-to-mutate))

;;----------------------------------------------------------------

;; [ cosθ −sinθ
;;   sinθ cosθ ]
(defn set-rotation-from-euler ((mat-to-mutate mat2)
                               (angle single-float)) mat2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((sa (sin angle))
        (ca (cos angle)))
    (set-components ca (cl:- sa)
                    sa ca
                    mat-to-mutate)))

;;----------------------------------------------------------------

(defn set-from-scale ((mat-to-mutate mat2) (scale-vec2 vec2)) mat2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (x scale-vec2)  0.0
                  0.0             (y scale-vec2)
                  mat-to-mutate))

;;----------------------------------------------------------------

(defn + ((mat-accum mat2) (mat-b mat2)) mat2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:incf (aref mat-accum 0) (aref mat-b 0))
  (cl:incf (aref mat-accum 1) (aref mat-b 1))
  (cl:incf (aref mat-accum 2) (aref mat-b 2))
  (cl:incf (aref mat-accum 3) (aref mat-b 3))
  mat-accum)

;;----------------------------------------------------------------

(defn - ((mat-accum mat2) (mat-b mat2)) mat2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:decf (aref mat-accum 0) (aref mat-b 0))
  (cl:decf (aref mat-accum 1) (aref mat-b 1))
  (cl:decf (aref mat-accum 2) (aref mat-b 2))
  (cl:decf (aref mat-accum 3) (aref mat-b 3))
  mat-accum)

;;----------------------------------------------------------------

(defn negate ((mat-to-negate mat2)) mat2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (cl:- (x mat-to-negate))
                  (cl:- (y mat-to-negate))
                  (cl:- (z mat-to-negate))
                  (cl:- (w mat-to-negate))
                  mat-to-negate))

;;----------------------------------------------------------------

(defn *v ((mat-a mat2) (vec2-to-mutate vec2)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (v2-n:set-components (cl:+ (cl:* (x vec2-to-mutate) (melm mat-a 0 0))
                             (cl:* (y vec2-to-mutate) (melm mat-a 0 1)))
                       (cl:+ (cl:* (x vec2-to-mutate) (melm mat-a 1 0))
                             (cl:* (y vec2-to-mutate) (melm mat-a 1 1)))
                       vec2-to-mutate))

;;----------------------------------------------------------------

(defn mrow*vec2 ((vec2-to-mutate vec2) (mat-a mat2)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (v2-n:set-components (cl:+ (cl:* (x vec2-to-mutate) (melm mat-a 0 0))
                             (cl:* (y vec2-to-mutate) (melm mat-a 1 0)))
                       (cl:+ (cl:* (x vec2-to-mutate) (melm mat-a 0 1))
                             (cl:* (y vec2-to-mutate) (melm mat-a 1 1)))
                       vec2-to-mutate))

;;----------------------------------------------------------------

(defn *s ((mat-to-mutate mat2) (scalar single-float)) mat2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components
   (cl:* (melm mat-to-mutate 0 0) scalar)
   (cl:* (melm mat-to-mutate 0 1) scalar)
   (cl:* (melm mat-to-mutate 1 0) scalar)
   (cl:* (melm mat-to-mutate 1 1) scalar)
   mat-to-mutate))
