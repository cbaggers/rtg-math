(in-package :rtg-math.vectors)

;;----------------------------------------------------------------

(define-compiler-macro swizzle (&whole form vec pattern)
  (if (keywordp pattern)
      (let* ((name (cl:symbol-name pattern))
             (len (cl:length name))
             (arr (gensym "arr")))
        (if (or (> len 4) (< len 2))
            (error "Vectors: swizzle: Cepl vectors cannot have a length less that 2 or greater than 4")
            `(cl:let ((,arr ,vec))
               (cl:make-array
                ,len :element-type 'single-float :initial-contents
                (list ,@(loop :for char :across name
                           :collect `(aref ,arr ,(or (position char "XYZW")
                                                     (error "Vectors: swizzle: Pattern component was not X, Y, Z or W: ~a" char)))))))))
      form))

(defun swizzle (vec pattern)
  (let* ((name (cl:symbol-name pattern))
         (len (cl:length name))
         (result (cl:make-array (cl:length name) :element-type 'single-float)))
    (if (or (> len 4) (< len 2))
        (error "Vectors: swizzle: Cepl vectors cannot have a length less that 2 or greater than 4")
        (loop :for char :across name :for i :from 0 :do
           (setf (aref result i)
                 (aref vec (or (position char "XYZW")
                               (error "Vectors: swizzle: Pattern component was not X, Y, Z or W: ~a" char))))))
    result))

(defun (setf swizzle) (value vec pattern)
  (let* ((name (cl:symbol-name pattern))
         (len (cl:length name)))
    (if (or (> len 4) (< len 2))
        (error "Vectors: swizzle: Cepl vectors cannot have a length less that 2 or greater than 4")
        (loop :for char :across name :for i :from 0 :do
           (setf (aref vec (position char "XYZW"))
                 (aref value i))))
    vec))

(define-compiler-macro (setf swizzle) (&whole form value vec pattern)
  (labels ((get-pos (char)
             (or (position char "XYZW")
                 (error "Vectors: swizzle: Pattern component was not X, Y, Z or W: ~a" char))))
    (if (keywordp pattern)
        (let* ((name (cl:symbol-name pattern))
               (len (cl:length name))
               (val (gensym "val"))
               (dst (gensym "vec")))
          (if (or (> len 4) (< len 2))
              (error "Vectors: swizzle: Cepl vectors cannot have a length less that 2 or greater than 4")
              `(let ((,dst ,vec)
                     (,val ,value))
                 ,@(loop :for char :across name :for i :from 0
                      :collect `(setf (aref ,dst ,(get-pos char))
                                      (aref ,val ,i)))
                 ,dst)))
        form)))

(defun s~ (vec pattern)
  (swizzle vec pattern))

(define-compiler-macro s~ (vec pattern)
  `(swizzle ,vec ,pattern))

(defun (setf s~) (value vec pattern)
  (setf (swizzle vec pattern) value))

(define-compiler-macro (setf s~) (value vec pattern)
  `(setf (swizzle ,vec ,pattern) ,value))

;;----------------------------------------------------------------

(defmacro dvec (var-list expression &body body)
  (when (or (not (listp var-list))
            (not (every #'symbolp var-list))
            (< (cl:length var-list) 1)
            (> (cl:length var-list) 4))
    (error "dvec: invalid vector destructuring pattern ~s"
           var-list))
  (let ((e (gensym "expression")))
    `(let ((,e ,expression))
       (let ,(loop :for v :in var-list :for i :from 0 :collect
                `(,v (aref ,e ,i)))
         ,@body))))

(defmacro dvec* (var-list-expression-pairs &body body)
  (labels ((group (source n)
             "This takes a  flat list and emit a list of lists, each n long
              containing the elements of the original list"
             (if (= 0 n) (error "zero length"))
             (labels ((rec (source acc)
                        (let ((rest (nthcdr n source)))
                          (if (consp rest)
                              (rec rest (cons (subseq source 0 n)
                                              acc))
                              (nreverse (cons source acc))))))
               (if source
                   (rec source nil)
                   nil))))
    (let ((pairs (append body
                         (group var-list-expression-pairs 2))))
      (labels ((m (x y) `(dvec ,@y ,x)))
        (reduce #'m pairs)))))

;;----------------------------------------------------------------

(defun %make-vector (x y &optional z w)
  "This takes floats and give back a vector, this is just an
   array but it specifies the array type and populates it. "
  (cond (w (v4:make (float x) (float y) (float z) (float w)))
        (z (v3:make (float x) (float y) (float z)))
        (t (v2:make (float x) (float y)))))

;; {TODO} remove this
(defun merge-into-vector (&rest vectors)
  "Takes a list of vectors and combines them into a new vector"
  (warn "merge-into-vector is deprecated, use v! instead")
  (labels ((seqify (x)
             (if (or (listp x) (arrayp x))
                 x
                 (list x))))
    (let ((combined (mapcar #'(lambda (x)
                                (coerce x 'single-float))
                            (apply #'concatenate 'list
                                   (mapcar #'seqify vectors)))))
      (apply #'%make-vector combined))))

;;----------------------------------------------------------------

(defun floatify (vec)
  (make-array (cl:length vec) :element-type 'single-float :initial-contents
              (loop :for i :across vec :collect (float i))))

;;----------------------------------------------------------------

(defun 0p (vec-a)
  "Returns t if the vector is of zero length"
  (let ((vec-a (floatify vec-a)))
    (case= (cl:length vec-a)
      (2 (v2:0p vec-a))
      (3 (v3:0p vec-a))
      (4 (v4:0p vec-a)))))

;;----------------------------------------------------------------

(defun unitp (vec-a)
  (let ((vec-a (floatify vec-a)))
    (case= (cl:length vec-a)
      (2 (v2:unitp vec-a))
      (3 (v3:unitp vec-a))
      (4 (v4:unitp vec-a)))))

;;----------------------------------------------------------------

(defun = (&rest vecs)
  "Returns either t if the vectors are equal.
   Otherwise it returns nil."
  (let* ((vec-a (first vecs))
         (a-len (cl:length vec-a)))
    (loop :for vec :in (cdr vecs) :always
       (and (cl:= (cl:length vec) a-len)
            (loop :for i :below a-len :always
               (cl:= (aref vec i) (aref vec-a i)))))))

;;----------------------------------------------------------------

(defun /= (&rest vecs)
  "Returns either t if the two vectors are equal.
   Otherwise it returns nil."
  (let* ((vec-a (first vecs))
         (a-len (cl:length vec-a)))
    (not (loop :for vec :in (cdr vecs) :always
            (and (cl:= (cl:length vec) a-len)
                 (loop :for i :below a-len :always
                    (cl:= (aref vec i) (aref vec-a i))))))))

;;----------------------------------------------------------------

(defun +s (vec scalar)
  (let ((vec (floatify vec))
        (scalar (float scalar)))
    (case= (cl:length vec)
      (2 (v2:+s vec scalar))
      (3 (v3:+s vec scalar))
      (4 (v4:+s vec scalar)))))

;;----------------------------------------------------------------

(defun -s (vec scalar)
  (let ((vec (floatify vec))
        (scalar (float scalar)))
    (case= (cl:length vec)
      (2 (v2:-s vec scalar))
      (3 (v3:-s vec scalar))
      (4 (v4:-s vec scalar)))))

;;----------------------------------------------------------------

(defun + (&rest vecs)
  (let ((vecs (mapcar #'floatify vecs)))
    (case= (cl:length (first vecs))
      (2 (apply #'v2:+ vecs))
      (3 (apply #'v3:+ vecs))
      (4 (apply #'v4:+ vecs)))))

;;----------------------------------------------------------------

(defun - (&rest vecs)
  (let ((vecs (mapcar #'floatify vecs)))
    (case= (cl:length (first vecs))
      (2 (apply #'v2:- vecs))
      (3 (apply #'v3:- vecs))
      (4 (apply #'v4:- vecs)))))

;;----------------------------------------------------------------

(defun * (vec-a scalar-or-vec)
  "Adds two vectors together and returns the result as a new vector of the same type"
  (let ((vec-a (floatify vec-a)))
    (typecase scalar-or-vec
      (number (let ((num (coerce scalar-or-vec 'single-float)))
                (case= (cl:length vec-a)
                  (2 (v2:*s vec-a num))
                  (3 (v3:*s vec-a num))
                  (4 (v4:*s vec-a num)))))
      (array (case= (cl:length vec-a)
               (2 (v2:* vec-a scalar-or-vec))
               (3 (v3:* vec-a scalar-or-vec))
               (4 (v4:* vec-a scalar-or-vec)))))))

;;----------------------------------------------------------------

(defun / (vec-a scalar-or-vec)
  (let ((vec-a (floatify vec-a)))
    (if (typep scalar-or-vec 'number)
        (let ((scalar (float scalar-or-vec)))
          (case= (cl:length vec-a)
            (2 (v2:/s vec-a scalar))
            (3 (v3:/s vec-a scalar))
            (4 (v4:/s vec-a scalar))))
        (let ((vec-b (floatify scalar-or-vec)))
          (case= (cl:length vec-a)
            (2 (v2:/ vec-a vec-b))
            (3 (v3:/ vec-a vec-b))
            (4 (v4:/ vec-a vec-b)))))))

;;----------------------------------------------------------------

(defun length (vec-a)
  (let ((vec-a (floatify vec-a)))
    (case= (cl:length vec-a)
      (2 (v2:length vec-a))
      (3 (v3:length vec-a))
      (4 (v4:length vec-a)))))

;;----------------------------------------------------------------

(defun length-squared (vec-a)
  (let ((vec-a (floatify vec-a)))
    (case= (cl:length vec-a)
      (2 (v2:length-squared vec-a))
      (3 (v3:length-squared vec-a))
      (4 (v4:length-squared vec-a)))))

;;----------------------------------------------------------------

(defun distance (vec-a vec-b)
  (let ((vec-a (floatify vec-a))
        (vec-b (floatify vec-b)))
    (case= (cl:length vec-a)
      (2 (v2:distance vec-a vec-b))
      (3 (v3:distance vec-a vec-b))
      (4 (v4:distance vec-a vec-b)))))

;;----------------------------------------------------------------

(defun distance-squared (vec-a vec-b)
  (let ((vec-a (floatify vec-a))
        (vec-b (floatify vec-b)))
    (case= (cl:length vec-a)
      (2 (v2:distance-squared vec-a vec-b))
      (3 (v3:distance-squared vec-a vec-b))
      (4 (v4:distance-squared vec-a vec-b)))))

;;----------------------------------------------------------------

(defun dot (vec-a vec-b)
  (let ((vec-a (floatify vec-a))
        (vec-b (floatify vec-b)))
    (case= (cl:length vec-a)
      (2 (v2:dot vec-a vec-b))
      (3 (v3:dot vec-a vec-b))
      (4 (v4:dot vec-a vec-b)))))



;;----------------------------------------------------------------

(defun negate (vec)
  (let ((vec (floatify vec)))
    (case= (cl:length vec)
      (2 (v2:negate vec))
      (3 (v3:negate vec))
      (4 (v4:negate vec)))))

(defun face-foreward (vec-a vec-b)
  (let ((vec-a (floatify vec-a))
        (vec-b (floatify vec-b)))
    (case= (cl:length vec-a)
      (2 (v2:face-foreward vec-a vec-b))
      (3 (v3:face-foreward vec-a vec-b))
      (4 (v4:face-foreward vec-a vec-b)))))

;;----------------------------------------------------------------

(defun absolute-dot (vec-a vec-b)
  "Adds two vectors together and returns the result as a new vector of the same type"
  (let ((vec-a (floatify vec-a))
        (vec-b (floatify vec-b)))
    (case= (cl:length vec-a)
      (2 (v2:absolute-dot vec-a vec-b))
      (3 (v3:absolute-dot vec-a vec-b))
      (4 (v4:absolute-dot vec-a vec-b)))))

;;----------------------------------------------------------------

(defun perp-dot (vec-a vec-b)
  (let ((vec-a (floatify vec-a))
        (vec-b (floatify vec-b)))
    (if (= 2 (length vec-a))
        (v2:perp-dot vec-a vec-b)
        (error "Incorrect vector size"))))

;;----------------------------------------------------------------

(defun normalize (vec-a)
  (let ((vec-a (floatify vec-a)))
    (case= (cl:length vec-a)
      (2 (v2:normalize vec-a))
      (3 (v3:normalize vec-a))
      (4 (v4:normalize vec-a)))))

;;----------------------------------------------------------------

(defun cross (vec-a vec-b)
  (let ((vec-a (floatify vec-a))
        (vec-b (floatify vec-b)))
    (case= (cl:length vec-a)
      (2 (v2:cross vec-a vec-b))
      (3 (v3:cross vec-a vec-b)))))

;;----------------------------------------------------------------

(declaim (inline lerp)
         (ftype (function ((or vec2 vec3 vec4)
                           (or vec2 vec3 vec4)
                           (or (integer) single-float))
                          (or vec2 vec3 vec4))
                lerp))
(defun lerp (vector-a vector-b ammount)
  (declare (type (or vec2 vec3 vec4) vector-a vector-b))
  (case= (cl:length vector-a)
    (2 (v2:lerp vector-a vector-b (float ammount)))
    (3 (v3:lerp vector-a vector-b (float ammount)))
    (4 (v4:lerp vector-a vector-b (float ammount)))
    (otherwise (error "only vectors of size 2-4 are valid"))))

(declaim (inline mix)
         (ftype (function ((or vec2
                               vec3
                               vec4)
                           (or vec2
                               vec3
                               vec4)
                           (or (integer) single-float))
                          (or vec2
                              vec3
                              vec4))
                mix))
(defun mix (vector-a vector-b ammount)
  (declare (type (or vec2 vec3 vec4) vector-a vector-b))
  (lerp vector-a vector-b ammount))

(defun bezier (a1 a2 b1 b2 ammount)
  (lerp (lerp a1 a2 ammount)
        (lerp b1 b2 ammount)
        ammount))
