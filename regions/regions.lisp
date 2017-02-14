(in-package :rtg-math.region)

;;----------------------------------------------------------------

(defgeneric intersects-p (reg-a reg-b))

(defmethod intersects-p ((reg-a axis-aligned-box) (reg-b line3))
  (aab:intersects-with-line3-p reg-a reg-b))

(defmethod intersects-p ((reg-a line3) (reg-b axis-aligned-box))
  (aab:intersects-with-line3-p reg-b reg-a))

(defmethod intersects-p ((reg-a axis-aligned-box) (reg-b ray3))
  (aab:intersects-with-ray3-p reg-a reg-b))

(defmethod intersects-p ((reg-a ray3) (reg-b axis-aligned-box))
  (aab:intersects-with-ray3-p reg-b reg-a))

(defmethod intersects-p ((reg-a axis-aligned-box) (reg-b line-segment3))
  (aab:intersects-with-line-segment-p reg-a reg-b))

(defmethod intersects-p ((reg-a line-segment3) (reg-b axis-aligned-box))
  (aab:intersects-with-line-segment-p reg-b reg-a))

(defmethod intersects-p ((reg-a axis-aligned-box) (reg-b axis-aligned-box))
  (aab:intersects-p reg-a reg-b))

;;----------------------------------------------------------------

(defgeneric distance-squared (reg-a reg-b))

(defmethod distance-squared ((reg-a line-segment3) (reg-b line-segment3))
  (line-seg3:distance-squared-to-line-seg3 reg-a reg-b))

(defmethod distance-squared ((reg-a ray3) (reg-b ray3))
  (ray3:distance-squared-to-ray3 reg-a reg-b))

(defmethod distance-squared ((reg-a line3) (reg-b line3))
  (line3:distance-squared-to-line3 reg-a reg-b))

(defmethod distance-squared ((reg-a ray3) (reg-b line3))
  (ray3:distance-squared-to-line3 reg-a reg-b))

(defmethod distance-squared ((reg-a line3) (reg-b ray3))
  (ray3:distance-squared-to-line3 reg-b reg-a))

(defmethod distance-squared ((reg-a ray3) (reg-b line-segment3))
  (line-seg3:distance-squared-to-ray3 reg-b reg-a))

(defmethod distance-squared ((reg-a line-segment3) (reg-b ray3))
  (line-seg3:distance-squared-to-ray3 reg-a reg-b))

(defmethod distance-squared ((reg-a line3) (reg-b line-segment3))
  (line-seg3:distance-squared-to-line3 reg-b reg-a))

(defmethod distance-squared ((reg-a line-segment3) (reg-b line3))
  (line-seg3:distance-squared-to-line3 reg-a reg-b))

;;----------------------------------------------------------------

(defgeneric distance (reg-a reg-b))

(defmethod distance ((reg-a line-segment3) (reg-b line-segment3))
  (line-seg3:distance-to-line-seg3 reg-a reg-b))

(defmethod distance ((reg-a ray3) (reg-b ray3))
  (ray3:distance-to-ray3 reg-a reg-b))

(defmethod distance ((reg-a line3) (reg-b line3))
  (line3:distance-to-line3 reg-a reg-b))

(defmethod distance ((reg-a ray3) (reg-b line3))
  (ray3:distance-to-line3 reg-a reg-b))

(defmethod distance ((reg-a line3) (reg-b ray3))
  (ray3:distance-to-line3 reg-b reg-a))

(defmethod distance ((reg-a ray3) (reg-b line-segment3))
  (line-seg3:distance-to-ray3 reg-b reg-a))

(defmethod distance ((reg-a line-segment3) (reg-b ray3))
  (line-seg3:distance-to-ray3 reg-a reg-b))

(defmethod distance ((reg-a line3) (reg-b line-segment3))
  (line-seg3:distance-to-line3 reg-b reg-a))

(defmethod distance ((reg-a line-segment3) (reg-b line3))
  (line-seg3:distance-to-line3 reg-a reg-b))

;;----------------------------------------------------------------

(defgeneric closest-points (reg-a reg-b))

(defmethod closest-points ((reg-a line3) (reg-b line3))
  (line3:closest-line-points reg-a reg-b))

(defmethod closest-points ((reg-a ray3) (reg-b ray3))
  (ray3:closest-ray-points reg-a reg-b))

(defmethod closest-points ((reg-a line-segment3) (reg-b line-segment3))
  (line-seg3:closest-line-segment-points reg-a reg-b))

(defmethod closest-points ((reg-a line-segment3) (reg-b ray3))
  (line-seg3:closest-ray-points reg-a reg-b))

(defmethod closest-points ((reg-a ray3) (reg-b line-segment3))
  (line-seg3:closest-ray-points reg-b reg-a))

(defmethod closest-points ((reg-a line-segment3) (reg-b line3))
  (line-seg3:closest-line-points reg-a reg-b))

(defmethod closest-points ((reg-a line3) (reg-b line-segment3))
  (line-seg3:closest-line-points reg-b reg-a))

;;----------------------------------------------------------------

(defgeneric closest-point (reg-a vec3))

(defmethod closest-point ((reg line3) (point-vec3 array))
  (assert (typep point-vec3 'vec3))
  (line3:closest-point reg point-vec3))

(defmethod closest-point ((point-vec3 array) (reg line3))
  (assert (typep point-vec3 'vec3))
  (line3:closest-point reg point-vec3))

(defmethod closest-point ((reg ray3) (point-vec3 array))
  (assert (typep point-vec3 'vec3))
  (ray3:closest-point reg point-vec3))

(defmethod closest-point ((point-vec3 array) (reg ray3))
  (assert (typep point-vec3 'vec3))
  (ray3:closest-point reg point-vec3))

(defmethod closest-point ((reg line-segment3) (point-vec3 array))
  (assert (typep point-vec3 'vec3))
  (line-seg3:closest-point reg point-vec3))

(defmethod closest-point ((point-vec3 array) (reg line-segment3))
  (assert (typep point-vec3 'vec3))
  (line-seg3:closest-point reg point-vec3))

;;----------------------------------------------------------------
