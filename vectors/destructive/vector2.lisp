(in-package #:rtg-math.vector2.destructive)

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
