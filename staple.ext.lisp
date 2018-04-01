;;------------------------------------------------------------

(defmethod staple:system-options append
    ((system (eql (asdf:find-system :rtg-math))))
  (list :name "RTG-MATH"
        :template (asdf:system-relative-pathname
                   :varjo "docs/staple/template.ctml")
        :packages '(:rtg-math.vector2
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
                    :rtg-math.types)
        :out (asdf:system-relative-pathname
              :rtg-math "docs/rtg-math-reference.html")
        :if-exists :supersede))

(defmethod staple:render-docstring
    (string (system (eql (asdf:find-system :rtg-math))))
  (typecase string
    (string (staple:render-docstring-markdown string))
    (null (plump:parse "<i>No docstring provided.</i>"))))

;;------------------------------------------------------------
