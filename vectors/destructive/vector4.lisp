(in-package #:rtg-math.vector4.destructive)

;;----------------------------------------------------------------

(defun +s (vec4 scalar)
  (cl:incf (x vec4) scalar)
  (cl:incf (y vec4) scalar)
  (cl:incf (z vec4) scalar)
  (cl:incf (w vec4) scalar)
  vec4)

;;----------------------------------------------------------------

(defun -s (vec4 scalar)
  (cl:decf (x vec4) scalar)
  (cl:decf (y vec4) scalar)
  (cl:decf (z vec4) scalar)
  (cl:decf (w vec4) scalar))

;;---------------------------------------------------------------

(declaim (inline %+)
         (ftype (function (vec4 vec4) vec4)
                %+))
(defun %+ (accum-vec to-add-vec)
  "Add two vectors and return a new vector containing the result"
  (declare (vec4 accum-vec to-add-vec))
  (cl:incf (aref accum-vec 0) (aref to-add-vec 0))
  (cl:incf (aref accum-vec 1) (aref to-add-vec 1))
  (cl:incf (aref accum-vec 2) (aref to-add-vec 2))
  (cl:incf (aref accum-vec 3) (aref to-add-vec 3))
  accum-vec)

(defun + (accum-vec &rest vec4s)
  "Add two vectors and return a new vector containing the result"
  (declare (inline %+))
  (loop :for vec :in vec4s :do (%+ accum-vec vec))
  accum-vec)

(define-compiler-macro + (&whole whole accum-vec &rest vec4s)
  (assert accum-vec)
  (case= (cl:length vec4s)
    (0 accum-vec)
    (1 `(%+ ,accum-vec (first vec4s)))
    (otherwise whole)))

;;---------------------------------------------------------------

(declaim (inline %-)
         (ftype (function (vec4 vec4) vec4)
                %-))
(defun %- (accum-vec to-add-vec)
  "Add two vectors and return a new vector containing the result"
  (declare (vec4 accum-vec to-add-vec))
  (cl:decf (aref accum-vec 0) (aref to-add-vec 0))
  (cl:decf (aref accum-vec 1) (aref to-add-vec 1))
  (cl:decf (aref accum-vec 2) (aref to-add-vec 2))
  (cl:decf (aref accum-vec 2) (aref to-add-vec 3))
  accum-vec)

(defun - (accum-vec &rest vec4s)
  "Add two vectors and return a new vector containing the result"
  (declare (inline %-))
  (loop :for vec :in vec4s :do (%- accum-vec vec))
  accum-vec)

(define-compiler-macro - (&whole whole accum-vec &rest vec4s)
  (assert accum-vec)
  (case= (cl:length vec4s)
    (0 accum-vec)
    (1 `(%- ,accum-vec (first vec4s)))
    (otherwise whole)))

;;---------------------------------------------------------------

(declaim (inline %*)
         (ftype (function (vec4 vec4) vec4)
                %*))
(defun %* (accum-vec to-add-vec)
  "Add two vectors and return a new vector containing the result"
  (declare (vec4 accum-vec to-add-vec))
  (cl:setf (aref accum-vec 0) (cl:* (aref accum-vec 0) (aref to-add-vec 0)))
  (cl:setf (aref accum-vec 1) (cl:* (aref accum-vec 1) (aref to-add-vec 1)))
  (cl:setf (aref accum-vec 2) (cl:* (aref accum-vec 2) (aref to-add-vec 2)))
  (cl:setf (aref accum-vec 3) (cl:* (aref accum-vec 2) (aref to-add-vec 3)))
  accum-vec)

(defun * (accum-vec &rest vec4s)
  "Add two vectors and return a new vector containing the result"
  (declare (inline %*))
  (loop :for vec :in vec4s :do (%* accum-vec vec))
  accum-vec)

(define-compiler-macro * (&whole whole accum-vec &rest vec4s)
  (assert accum-vec)
  (case= (cl:length vec4s)
    (0 accum-vec)
    (1 `(%* ,accum-vec (first vec4s)))
    (otherwise whole)))

;;---------------------------------------------------------------

(declaim (inline *s)
         (ftype (function (vec4 single-float) vec4)
                *s))
(defun *s (vec4 a)
  "Multiply vector by scalar"
  (declare (vec4 vec4) (single-float a))
  (setf (x vec4) (cl:* (x vec4) a))
  (setf (y vec4) (cl:* (y vec4) a))
  (setf (z vec4) (cl:* (z vec4) a))
  (setf (w vec4) (cl:* (w vec4) a))
  vec4)

;;---------------------------------------------------------------
