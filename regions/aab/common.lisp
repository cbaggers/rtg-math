(in-package :%rtg-math.regions.axis-aligned-box.common)

;;------------------------------------------------------------

(defmacro with-aab ((mina maxa) aab &body body)
  (assert (and mina maxa))
  (let ((box (gensym "aab")))
    `(let* ((,box ,aab)
            (,mina (axis-aligned-box-minima ,box))
            (,maxa (axis-aligned-box-maxima ,box)))
       (declare (axis-aligned-box ,box) (vec3 ,mina ,maxa))
       ,@body)))

;;------------------------------------------------------------
