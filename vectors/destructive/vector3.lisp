(in-package #:rtg-math.vector3.destructive)

;;----------------------------------------------------------------

(defun +s (vec3 scalar)
  (cl:incf (x vec3) scalar)
  (cl:incf (y vec3) scalar)
  (cl:incf (z vec3) scalar)
  vec3)

;;----------------------------------------------------------------

(defun -s (vec3 scalar)
  (cl:decf (x vec3) scalar)
  (cl:decf (y vec3) scalar)
  (cl:decf (z vec3) scalar)
  vec3)

;;---------------------------------------------------------------

(declaim (inline %+)
         (ftype (function (vec3 vec3) vec3)
                %+))
(defun %+ (accum-vec to-add-vec)
  "Add two vectors and return a new vector containing the result"
  (declare (vec3 accum-vec to-add-vec))
  (cl:incf (aref accum-vec 0) (aref to-add-vec 0))
  (cl:incf (aref accum-vec 1) (aref to-add-vec 1))
  (cl:incf (aref accum-vec 2) (aref to-add-vec 2))
  accum-vec)

(defun + (accum-vec &rest vec3s)
  "Add two vectors and return a new vector containing the result"
  (declare (inline %+))
  (loop :for vec :in vec3s :do (%+ accum-vec vec))
  accum-vec)

(define-compiler-macro + (&whole whole accum-vec &rest vec3s)
  (assert accum-vec)
  (case= (cl:length vec3s)
    (0 accum-vec)
    (1 `(%+ ,accum-vec (first vec3s)))
    (otherwise whole)))

;;---------------------------------------------------------------

(declaim (inline %-)
         (ftype (function (vec3 vec3) vec3)
                %-))
(defun %- (accum-vec to-add-vec)
  "Add two vectors and return a new vector containing the result"
  (declare (vec3 accum-vec to-add-vec))
  (cl:decf (aref accum-vec 0) (aref to-add-vec 0))
  (cl:decf (aref accum-vec 1) (aref to-add-vec 1))
  (cl:decf (aref accum-vec 2) (aref to-add-vec 2))
  accum-vec)

(defun - (accum-vec &rest vec3s)
  "Add two vectors and return a new vector containing the result"
  (declare (inline %-))
  (loop :for vec :in vec3s :do (%- accum-vec vec))
  accum-vec)

(define-compiler-macro - (&whole whole accum-vec &rest vec3s)
  (assert accum-vec)
  (case= (cl:length vec3s)
    (0 accum-vec)
    (1 `(%- ,accum-vec (first vec3s)))
    (otherwise whole)))

;;---------------------------------------------------------------

(declaim (inline %*)
         (ftype (function (vec3 vec3) vec3)
                %*))
(defun %* (accum-vec to-add-vec)
  "Add two vectors and return a new vector containing the result"
  (declare (vec3 accum-vec to-add-vec))
  (cl:setf (aref accum-vec 0) (cl:* (aref accum-vec 0) (aref to-add-vec 0)))
  (cl:setf (aref accum-vec 1) (cl:* (aref accum-vec 1) (aref to-add-vec 1)))
  (cl:setf (aref accum-vec 2) (cl:* (aref accum-vec 2) (aref to-add-vec 2)))
  accum-vec)

(defun * (accum-vec &rest vec3s)
  "Add two vectors and return a new vector containing the result"
  (declare (inline %*))
  (loop :for vec :in vec3s :do (%* accum-vec vec))
  accum-vec)

(define-compiler-macro * (&whole whole accum-vec &rest vec3s)
  (assert accum-vec)
  (case= (cl:length vec3s)
    (0 accum-vec)
    (1 `(%* ,accum-vec (first vec3s)))
    (otherwise whole)))

;;---------------------------------------------------------------

(declaim (inline *s)
         (ftype (function (vec3 single-float) vec3)
                *s))
(defun *s (vec3 a)
  "Multiply vector by scalar"
  (declare (vec3 vec3) (single-float a))
  (setf (x vec3) (cl:* (x vec3) a))
  (setf (y vec3) (cl:* (y vec3) a))
  (setf (z vec3) (cl:* (z vec3) a))
  vec3)

;;---------------------------------------------------------------
