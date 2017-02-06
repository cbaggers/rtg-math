(in-package #:rtg-math.vector2.destructive)

;;----------------------------------------------------------------

(defun +s (vec2 scalar)
  (cl:incf (x vec2) scalar)
  (cl:incf (y vec2) scalar)
  vec2)

;;----------------------------------------------------------------

(defun -s (vec2 scalar)
  (cl:decf (x vec2) scalar)
  (cl:decf (y vec2) scalar)
  vec2)

;;---------------------------------------------------------------

(declaim (inline %+)
         (ftype (function (vec2 vec2) vec2)
                %+))
(defun %+ (accum-vec to-add-vec)
  "Add two vectors and return a new vector containing the result"
  (declare (vec2 accum-vec to-add-vec))
  (cl:incf (aref accum-vec 0) (aref to-add-vec 0))
  (cl:incf (aref accum-vec 1) (aref to-add-vec 1))
  accum-vec)

(defun + (accum-vec &rest vec2s)
  "Add two vectors and return a new vector containing the result"
  (declare (inline %+))
  (loop :for vec :in vec2s :do (%+ accum-vec vec))
  accum-vec)

(define-compiler-macro + (&whole whole accum-vec &rest vec2s)
  (assert accum-vec)
  (case= (cl:length vec2s)
    (0 accum-vec)
    (1 `(%+ ,accum-vec (first vec2s)))
    (otherwise whole)))

;;---------------------------------------------------------------

(declaim (inline %-)
         (ftype (function (vec2 vec2) vec2)
                %-))
(defun %- (accum-vec to-add-vec)
  "Add two vectors and return a new vector containing the result"
  (declare (vec2 accum-vec to-add-vec))
  (cl:decf (aref accum-vec 0) (aref to-add-vec 0))
  (cl:decf (aref accum-vec 1) (aref to-add-vec 1))
  accum-vec)

(defun - (accum-vec &rest vec2s)
  "Add two vectors and return a new vector containing the result"
  (declare (inline %-))
  (loop :for vec :in vec2s :do (%- accum-vec vec))
  accum-vec)

(define-compiler-macro - (&whole whole accum-vec &rest vec2s)
  (assert accum-vec)
  (case= (cl:length vec2s)
    (0 accum-vec)
    (1 `(%- ,accum-vec (first vec2s)))
    (otherwise whole)))

;;---------------------------------------------------------------

(declaim (inline %*)
         (ftype (function (vec2 vec2) vec2)
                %*))
(defun %* (accum-vec to-add-vec)
  "Add two vectors and return a new vector containing the result"
  (declare (vec2 accum-vec to-add-vec))
  (cl:setf (aref accum-vec 0) (cl:* (aref accum-vec 0) (aref to-add-vec 0)))
  (cl:setf (aref accum-vec 1) (cl:* (aref accum-vec 1) (aref to-add-vec 1)))
  accum-vec)

(defun * (accum-vec &rest vec2s)
  "Add two vectors and return a new vector containing the result"
  (declare (inline %*))
  (loop :for vec :in vec2s :do (%* accum-vec vec))
  accum-vec)

(define-compiler-macro * (&whole whole accum-vec &rest vec2s)
  (assert accum-vec)
  (case= (cl:length vec2s)
    (0 accum-vec)
    (1 `(%* ,accum-vec (first vec2s)))
    (otherwise whole)))

;;---------------------------------------------------------------

(declaim (inline *s)
         (ftype (function (vec2 single-float) vec2)
                *s))
(defun *s (vec2 a)
  "Multiply vector by scalar"
  (declare (vec2 vec2) (single-float a))
  (setf (x vec2) (cl:* (x vec2) a))
  (setf (y vec2) (cl:* (y vec2) a))
  vec2)

;;---------------------------------------------------------------
