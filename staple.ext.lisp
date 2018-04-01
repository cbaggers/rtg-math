(defun gen-docs ()
  (let* ((things '(staple:symb-function
                   staple:symb-accessor))
         (current (loop :for n :in things :collect (staple:converter n)))
         (ignores (lambda (x y) (declare (ignore x y)))))
    (loop
       :for n :in things
       :for c :in current
       :do (when c (setf (staple:converter n) ignores)))
    (unwind-protect
         (staple:generate :rtg-math)
      (loop
         :for n :in things
         :for c :in current
         :do (when c (setf (staple:converter n) c))))))


;;------------------------------------------------------------

(defvar *documented-packages*
  '(:rtg-math.vector2
    :rtg-math.vector3
    :rtg-math.vector4
    :rtg-math.vector2.non-consing
    :rtg-math.vector3.non-consing
    :rtg-math.vector4.non-consing
    :rtg-math.vectors
    :rtg-math.matrix2
    :rtg-math.matrix3
    :rtg-math.matrix4
    :rtg-math.matrix2.non-consing
    :rtg-math.matrix3.non-consing
    :rtg-math.matrix4.non-consing
    :rtg-math.matrices
    :rtg-math.quaternions
    :rtg-math.quaternions.non-consing
    :rtg-math.polar
    :rtg-math.spherical
    :rtg-math.projection
    :rtg-math.projection.non-consing
    :rtg-math.region.line3
    :rtg-math.region.ray3
    :rtg-math.region.line-segment3
    :rtg-math.region.axis-aligned-box.non-consing
    :rtg-math.region.axis-aligned-box
    :rtg-math.region
    :rtg-math
    :rtg-math.types))

(defmethod staple:system-options append
    ((system (eql (asdf:find-system :rtg-math))))
  (list :name "RTG-MATH"
        :template (asdf:system-relative-pathname
                   :varjo "docs/staple/template.ctml")
        :documentation (asdf:system-relative-pathname
                        :rtg-math "docs/header.md")
        :packages *documented-packages*
        :out (asdf:system-relative-pathname
              :rtg-math "docs/rtg-math-reference.html")
        :if-exists :supersede))

(defmethod staple:render-docstring
    (string (system (eql (asdf:find-system :rtg-math))))
  (typecase string
    (string (staple:render-docstring-markdown string))
    (null (plump:parse "<i>No docstring provided.</i>"))))

;;------------------------------------------------------------

(defclass rtg-func (staple:symb-function)
  ((args :initform nil :initarg :args)))

(defmethod staple:symb-type ((f rtg-func))
  (declare (ignore f))
  "function")

(staple:define-converter rtg-func (symbol package)
  (let ((name (symbol-name symbol))
        (pname (package-name package)))
    (when (and (staple:symbol-function-p symbol)
               (find pname
                     *documented-packages*
                     :test #'string=)
               (eq (nth-value 1 (find-symbol name package))
                   :external))
      (list (make-instance 'rtg-func :symbol symbol)))))

(defmethod staple:symb-arguments ((symb rtg-func))
  (let ((symbol (staple:symb-symbol symb)))
    (or (first (gethash symbol %rtg-math::*signatures*))
        (call-next-method))))
