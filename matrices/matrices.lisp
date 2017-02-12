(in-package :rtg-math.matrices)

;;----------------------------------------------------------------

(defun 0p (matrix)
  (loop :for e :across matrix :always (cl:= 0f0 e)))

;;----------------------------------------------------------------


(defun unitp (matrix)
  (let ((len (cl:length matrix)))
    (cond
      ((cl:= len 9)
       (null (mismatch matrix (m3:identity))))
      ((cl:= len 16)
       (null (mismatch matrix (m4:identity)))))))


;;----------------------------------------------------------------


(defun = (&rest matrices)
  "Returns either t if the matrices are equal.
   Otherwise it returns nil."
  (let* ((matrix-a (first matrices))
         (len-a (cl:length matrix-a)))
    (and (every (lambda (x) (cl:= (cl:length x) len-a)) (rest matrices))
         (loop :for i :below len-a :always
            (loop :for j :in (rest matrices)
               :for v cl:= (aref matrix-a i)
               :always (cl:= (aref j i) v))))))

;;----------------------------------------------------------------

(defun /= (&rest matrices)
  "Returns either t if the matrices are equal.
   Otherwise it returns nil."
  (let ((matrix-a (first matrices)))
    (loop :for matrix :in (cdr matrices) :always (cl:/= matrix-a matrix))))

;;----------------------------------------------------------------


(defun 1+ (matrix-a matrix-b)
  (let ((len (cl:length matrix-a)))
    (assert (cl:= len (cl:length matrix-b)))
    (cond
      ((cl:= len 9)
       (m3:+ matrix-a matrix-b))
      ((cl:= len 16)
       (m4:+ matrix-a matrix-b)))))


;;----------------------------------------------------------------

(defun + (&rest matrices)
  (let* ((len (cl:length (first matrices)))
         (matrix-a (make-array len :element-type 'single-float
                               :initial-element 0.0)))
    (loop for matrix in matrices
       do (loop for i below len
             do (cl:setf (cl:aref matrix-a i)
                         (cl:+ (cl:aref matrix-a i)
                               (cl:aref matrix i))))
       finally (return matrix-a))))

;;----------------------------------------------------------------


(defun 1- (matrix-a matrix-b)
  (let ((len (cl:length matrix-a)))
    (assert (cl:= len (cl:length matrix-b)))
    (cond
      ((cl:= len 9)
       (m3:- matrix-a matrix-b ))
      ((cl:= len 16)
       (m4:- matrix-a matrix-b)))))


;;----------------------------------------------------------------

(defun - (&rest matrices)
  (let* ((len (cl:length (first matrices)))
         (matrix-a (make-array len :element-type 'single-float
                               :initial-element 0.0)))
    (loop for matrix in matrices
       do (loop for i below len
             do (cl:setf (cl:aref matrix-a i)
                         (cl:- (cl:aref matrix-a i)
                               (cl:aref matrix i)))))))

;;----------------------------------------------------------------

(defun elt (matrix row col)
  (declare (simple-array matrix))
  (let ((len (if (cl:= (cl:length matrix) 16) 4 3)))
    (aref matrix (cl:+ row (cl:* col len)))))

(defun elm (matrix row col)
  (let ((len (if (cl:= (cl:length matrix) 16) 4 3)))
    (aref matrix (cl:+ row (cl:* col len)))))

;;----------------------------------------------------------------


(defun get-rows (matrix-a)
  (let ((len (cl:length matrix-a)))
    (cond
      ((cl:= len 9)
       (m3:get-rows matrix-a))
      ((cl:= len 16)
       (m4:get-rows matrix-a)))))


;;----------------------------------------------------------------


(defun get-columns (matrix-a)
  (let ((len (cl:length matrix-a)))
    (cond
      ((cl:= len 9)
       (m3:get-columns matrix-a))
      ((cl:= len 16)
       (m4:get-columns matrix-a)))))


;;----------------------------------------------------------------


(defun get-row (matrix-a row-num)
  (let ((len (cl:length matrix-a)))
    (cond
      ((cl:= len 9)
       (m3:get-row matrix-a row-num))
      ((cl:= len 16)
       (m4:get-row matrix-a row-num)))))


;;----------------------------------------------------------------


(defun get-column (matrix-a col-num)
  (let ((len (cl:length matrix-a)))
    (cond
      ((cl:= len 9)
       (m3:get-column matrix-a col-num))
      ((cl:= len 16)
       (m4:get-column matrix-a col-num)))))


;;----------------------------------------------------------------


(defun determinant (matrix-a)
  (let ((len (cl:length matrix-a)))
    (cond
      ((cl:= len 9)
       (m3:determinant matrix-a))
      ((cl:= len 16)
       (m4:determinant matrix-a)))))


;;----------------------------------------------------------------


(defun inverse (matrix-a)
  (let ((len (cl:length matrix-a)))
    (cond
      ((cl:= len 9)
       (error "Inverse for 3x3 matrices is not yet implemented!"))
      ((cl:= len 16)
       (m4:inverse matrix-a)))))


;;----------------------------------------------------------------

(defun affine-inverse (matrix-a)
  (let ((len (cl:length matrix-a)))
    (cond
      ((cl:= len 9)
       (m3:affine-inverse matrix-a))
      ((cl:= len 16)
       (m4:affine-inverse matrix-a)))))

;;----------------------------------------------------------------

(defun transpose (matrix-a)
  (let ((len (cl:length matrix-a)))
    (cond
      ((cl:= len 9)
       (m3:transpose matrix-a))
      ((cl:= len 16)
       (m4:transpose matrix-a)))))


;;----------------------------------------------------------------


(defun trace (matrix-a)
  (let ((len (cl:length matrix-a)))
    (cond
      ((cl:= len 9)
       (m3:trace matrix-a))
      ((cl:= len 16)
       (m4:trace matrix-a)))))


;;----------------------------------------------------------------


(defun negate (matrix-a)
  (let ((len (cl:length matrix-a)))
    (cond
      ((cl:= len 9)
       (m3:negate matrix-a))
      ((cl:= len 16)
       (m4:negate matrix-a)))))


;;----------------------------------------------------------------


(defun * (matrix-a mat-vec-or-scalar)
  (let ((len (cl:length matrix-a)))
    (if (typep mat-vec-or-scalar 'number)
        (cond
          ((cl:= len 9)
           (m3:*s matrix-a mat-vec-or-scalar))
          ((cl:= len 16)
           (m4:*s matrix-a mat-vec-or-scalar)))

        (cond
          ((cl:= len 9)
           (if (< (cl:length mat-vec-or-scalar) 5)
               (m3:*v matrix-a mat-vec-or-scalar)
               (progn
                 (assert (cl:= len (cl:length mat-vec-or-scalar)))
                 (m3:* matrix-a mat-vec-or-scalar))))
          ((cl:= len 16)
           (if (< (cl:length mat-vec-or-scalar) 5)
               (m4:*v matrix-a mat-vec-or-scalar)
               (progn
                 (assert (cl:= len (cl:length mat-vec-or-scalar)))
                 (m4:* matrix-a mat-vec-or-scalar))))))))


;;----------------------------------------------------------------


(defun to-string (mat)
  (assert (typep mat '(or mat3 mat4)))
  (case= (cl:length mat)
    (9 (format nil "(m! ~a ~a ~a ~%     ~a ~a ~a ~%     ~a ~a ~a)~%"
               (m3:melm mat 0 0) (m3:melm mat 0 1) (m3:melm mat 0 2)
               (m3:melm mat 1 0) (m3:melm mat 1 1) (m3:melm mat 1 2)
               (m3:melm mat 2 0) (m3:melm mat 2 1) (m3:melm mat 2 2)))
    (16 (format nil "(m! ~a ~a ~a ~a ~%     ~a ~a ~a ~a ~%     ~a ~a ~a ~a ~%     ~a ~a ~a ~a)~%"
                (m4:melm mat 0 0) (m4:melm mat 0 1) (m4:melm mat 0 2) (m4:melm mat 0 3)
                (m4:melm mat 1 0) (m4:melm mat 1 1) (m4:melm mat 1 2) (m4:melm mat 1 3)
                (m4:melm mat 2 0) (m4:melm mat 2 1) (m4:melm mat 2 2) (m4:melm mat 2 3)
                (m4:melm mat 3 0) (m4:melm mat 3 1) (m4:melm mat 3 2) (m4:melm mat 3 3)))
    (otherwise "<unknown matrix format: print-matrix assumes a 3x3 or 4x4 matrix>")))
