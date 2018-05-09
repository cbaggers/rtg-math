(in-package :rtg-math.projection)

;;------------------------------------------------------------

;; Checked against GLM's perspectiveRH
(defn perspective-radian-fov ((width single-float) (height single-float)
                              (near single-float) (far single-float)
                              (fov single-float))
    mat4

  ;; note: is right handed
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((aspect (/ width height))
         (tan-half-fov (tan (/ fov 2f0)))
         (result (m4:0!)))
    (setf (m4:melm result 0 0) (/ 1f0 (* aspect tan-half-fov))
          (m4:melm result 1 1) (/ 1f0 tan-half-fov)
          (m4:melm result 2 2) (- (/ (+ far near) (- far near)))
          (m4:melm result 2 3) (- (/ (* 2f0 far near) (- far near)))
          (m4:melm result 3 2) -1f0)
    result))


(defn perspective ((width single-float) (height single-float)
                   (near single-float) (far single-float)
                   (fov-degrees single-float))
    mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (perspective-radian-fov width height near far (radians fov-degrees)))

(defn perspective-v2 ((frame-size-v2 vec2)
                      (near single-float) (far single-float)
                      (fov-degrees single-float))
    mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (perspective-radian-fov (x frame-size-v2) (y frame-size-v2) near far
                          (radians fov-degrees)))

(defn perspective-v2-radian-fov ((frame-size-v2 vec2)
                                 (near single-float) (far single-float)
                                 (fov-radians single-float))
    mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (perspective-radian-fov (x frame-size-v2) (y frame-size-v2) near far
                          fov-radians))

;;------------------------------------------------------------
