(in-package :rtg-math.regions.line-segment3)

(defstruct line-segment3
  (end-point0 (v! 0 0 0) :type vec3)
  (offset (v! 0 1 0) :type vec3))

(defun line-segment3-end-point1 (line-seg3)
  (v3:+ (line-segment3-end-point0 line-seg3)
        (line-segment3-offset line-seg3)))

(defun make (end-point0 end-point1)
  (make-line-segment3 :end-point0 end-point0
                      :offset (v:- end-point1 end-point0)))

(defun make-from-point-offset (point-v3 offset-v3)
  (make-line-segment3 :end-point0 point-v3
                      :offset offset-v3))

(defun transform-m3 (matrix3 line3)
  (make-line-segment3
   :end-point0 (m3:*v matrix3 (line-segment3-end-point0 line3))
   :offset (m3:*v matrix3 (line-segment3-offset line3))))

(defun transform-q (quaternion line3)
  (transform-m3 (q:to-mat3 quaternion)
                line3))

(defun direction (line-seg3)
  (line-segment3-offset line-seg3))

(defun length (line3)
  (v3:length (line-segment3-offset line3)))

(defun length-squared (line3)
  (v3:length-squared (line-segment3-offset line3)))
