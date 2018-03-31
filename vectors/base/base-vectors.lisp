(in-package :rtg-math.base-vectors)

(defmacro def-v! (name type)
  (let* ((one-arg (intern (format nil "V!ONE-ARG-~s" name)))
         (name-string (symbol-name name))
         (v2name (intern (format nil "V2!~a" (subseq name-string 2))))
         (v3name (intern (format nil "V3!~a" (subseq name-string 2))))
         (v4name (intern (format nil "V4!~a" (subseq name-string 2)))))
    `(labels ((convert (x) (coerce x ',type)))
       (defun ,name (&rest components)
         (let* ((components
                 (loop :for c :in components
                    :if (listp c) :append
                    (loop :for e :in c :collect (coerce e ',type))
                    :else :if (typep c 'array)
                    :append (loop :for e :across c :collect (coerce e ',type))
                    :else :collect (coerce c ',type)))
                (len (length components)))
           (when (or (> len 4) (< len 2))
             (error "Incorrect number of components for a vector: ~a ~a"
                    len components))
           (make-array (length components)
                       :element-type ',type
                       :initial-contents components)))
       (defun ,one-arg (x)
         (if (listp x)
             (make-array (length x) :element-type ',type
                         :initial-contents (mapcar #'convert x))
             (make-array (length x) :element-type ',type :initial-contents
                         (loop for i across x collect (convert i)))))
       (define-compiler-macro ,name (&whole form &rest components)
         (cond
           ((every #'numberp components)
            (let ((components (loop :for c :in components :collect
                                 (coerce c ',type)))
                  (len (length components)))
              (when (or (> len 4) (< len 2))
                (error "Incorrect number of components for a vector: ~a ~a"
                       len components))
              (list 'make-array len :element-type '',type
                    :initial-contents (list 'quote components))))
           ((= (length components) 1)
            (list ',one-arg ,'(first components)))
           (t form)))
       (declaim )
       (defun ,v2name (x &optional y)
         (if y (,name x y) (,name x x)))
       (defun ,v3name (x &optional y z)
         (if z (,name x y z) (if y (,name x y 0) (,name x x x))))
       (defun ,v4name (x &optional y z w)
         (if w (,name x y z w)
             (if z (,name x y z 0)
                 (if y (,name x y 0 0) (,name x x x 0))))))))

(def-v! v! single-float)
(def-v! v!double double-float)
(def-v! v!int (signed-byte 32))
(def-v! v!uint (unsigned-byte 32))
(def-v! v!ubyte (unsigned-byte 8)) ;; deprecated
(def-v! v!byte (signed-byte 8)) ;; deprecated
(def-v! v!uint8 (unsigned-byte 8))
(def-v! v!int8 (signed-byte 8))
(def-v! v!short (unsigned-byte 16))
(def-v! v!ushort (signed-byte 16))

(defun v!bool (&rest components)
  (let* ((components
          (loop :for c :in components
             :if (typep c 'array)
             :append (loop :for e :across c
                        :collect (coerce e 'boolean))
             :else
             :collect (coerce c 'boolean)))
         (len (length components)))
    (when (or (> len 4) (< len 2))
      (error "incorrect number of components for a vector: ~a ~a" len
             components))
    (make-array (length components) :element-type 'boolean :initial-contents
                components)))

(defun v!bool-one-arg (x)
  (if (listp x)
      (make-array (length x) :element-type 'boolean :initial-contents
                  (mapcar (lambda (_) (not (null _))) x))
      (make-array (length x) :element-type 'boolean :initial-contents
                  (loop :for i :across x :collect (not (null i))))))

(define-compiler-macro v!bool
    (&whole form &rest components)
  (cond
    ((every (lambda (x) (funcall (lambda (y) (typep y 'boolean)) x))
            components)
     (let ((components (loop :for c :in components :collect (not (null c))))
           (len (length components)))
       (when (or (> len 4) (< len 2))
         (error "incorrect number of components for a vector: ~a ~a" len
                components))
       (list 'make-array len :element-type ''boolean :initial-contents
             (list 'quote components))))
    ((= (length components) 1) (list 'v!bool-one-arg (first components)))
    (t form)))

;;----------------------------------------------------------------

(declaim (inline x y z w))
(defun x (vec)
  "Returns the x component of the vector"
  (aref vec 0))
(defun y (vec)
  "Returns the y component of the vector"
  (aref vec 1))
(defun z (vec)
  "Returns the z component of the vector"
  (aref vec 2))
(defun w (vec)
  "Returns the w component of the vector"
  (aref vec 3))

(define-compiler-macro x (vec)
  `(aref ,vec 0))

(define-compiler-macro y (vec)
  `(aref ,vec 1))

(define-compiler-macro z (vec)
  `(aref ,vec 2))

(define-compiler-macro w (vec)
  `(aref ,vec 3))

(defun (setf x) (value vec)
  "Sets the x component of the vector"
  (setf (aref vec 0) (float value)))
(defun (setf y) (value vec)
  "Sets the y component of the vector"
  (setf (aref vec 1) (float value)))
(defun (setf z) (value vec)
  "Sets the z component of the vector"
  (setf (aref vec 2) (float value)))
(defun (setf w) (value vec)
  "Sets the w component of the vector"
  (setf (aref vec 3) (float value)))

(define-compiler-macro (setf x) (value vec)
  `(setf (aref ,vec 0) ,value))

(define-compiler-macro (setf y) (value vec)
  `(setf (aref ,vec 1) ,value))

(define-compiler-macro (setf z) (value vec)
  `(setf (aref ,vec 2) ,value))

(define-compiler-macro (setf w) (value vec)
  `(setf (aref ,vec 3) ,value))

;;----------------------------------------------------------------
