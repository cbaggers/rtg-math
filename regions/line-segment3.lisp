(in-package :rtg-math.regions.line-segment3)

;; A line segment in ℝ3

(defstruct line-segment3
  (end-point0 (v! 0 0 0) :type vec3)
  (offset (v! 0 1 0) :type vec3))

(defn line-segment3-end-point1 ((line-seg3 line-segment3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (v3:+ (line-segment3-end-point0 line-seg3)
        (line-segment3-offset line-seg3)))

(defn-inline end-point0 ((line-seg3 line-segment3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (line-segment3-end-point0 line-seg3))

(defn-inline end-point1 ((line-seg3 line-segment3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (line-segment3-end-point1 line-seg3))

(defn direction ((line-seg3 line-segment3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (line-segment3-offset line-seg3))

(defn make ((end-point0 vec3) (end-point1 vec3)) line-segment3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make-line-segment3 :end-point0 end-point0
                      :offset (v3:- end-point1 end-point0)))

(defn make-from-point-offset ((point-v3 vec3) (offset-v3 vec3)) line-segment3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make-line-segment3 :end-point0 point-v3
                      :offset offset-v3))

;;----------------------------------------------------------------

(defn transform-m3 ((matrix3 mat3) (line-seg3 line-segment3)) line-segment3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (m3:*v matrix3 (end-point0 line-seg3))
        (m3:*v matrix3 (direction line-seg3))))

(defn transform-q ((quat quaternion) (line-seg3 line-segment3)) line-segment3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (q:rotate (end-point0 line-seg3) quat)
        (q:rotate (direction line-seg3) quat)))

;;----------------------------------------------------------------

(defn length-squared ((line-seg3 line-segment3))
    (single-float 0f0 #.most-positive-single-float)
  (v3:length-squared (line-segment3-offset line-seg3)))

(defn length ((line-seg3 line-segment3))
    (single-float 0f0 #.most-positive-single-float)
  (v3:length (line-segment3-offset line-seg3)))

;;----------------------------------------------------------------
