(in-package :rtg-math.matrixn)

(defun convolve (array)
  (let ((data (loop for y across array append
                   (loop for x across array collect
                        (* x y)))))
    (make-array (length data) :initial-contents data)))

(defun copy-array (a)
  (make-array (length a) :initial-contents (loop :for i :across a :collect i)))

(defun normalize-array (array &optional destructive)
  (let ((array (if destructive array (copy-array array))))
    (let ((sum (loop :for i :below (length array) :sum i)))
      (loop :for i :below (length array) :do
         (setf (svref array i) (/ (float (svref array i)) sum))))
    array))


(defun pascal (n)
  (let ((line (make-array (1+ n) :initial-element 1)))
    (loop :for k :below n :for i :from 1 :do
       (setf (svref line i) (* (svref line k) (/ (- n k) (1+ k)))))
    line))
