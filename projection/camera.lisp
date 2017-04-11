(in-package :rtg-math.projection)

;;------------------------------------------------------------

(defn perspective-radian-fov ((width single-float) (height single-float)
                              (near single-float) (far single-float)
                              (fov single-float))
    mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((d (/ 1f0 (tan (* fov 0.5f0))))
         (recip (/ 1f0 (- near far)))
         (ar (/ width height)))
    (m4:make (/ d ar)  0f0  0f0                     0f0
             0f0       d    0f0                     0f0
             0f0       0f0  (* (+ near far) recip)  (* 2 near far recip)
             0f0       0f0  -1f0                    0f0)))

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
                                 (fov-degrees single-float))
    mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (perspective-radian-fov (x frame-size-v2) (y frame-size-v2) near far
                          fov-degrees))

;;------------------------------------------------------------

(defn orthographic ((frame-width single-float) (frame-height single-float)
                    (near single-float) (far single-float))
    mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((left (- (/ frame-width 2.0)))
        (right (/ frame-width 2.0))
        (top (/ frame-height 2.0))
        (bottom (- (/ frame-height 2.0))))
    (m4:make
     (/ 2f0 (- right left)) 0f0                    0f0                   (- (/ (+ right left) (- right left)))
     0f0                    (/ 2f0 (- top bottom)) 0f0                   (- (/ (+ top bottom) (- top bottom)))
     0f0                    0f0                    (/ -2f0 (- far near)) (- (/ (+ far near) (- far near)))
     0f0                    0f0                    0f0                   1f0)))

(defn orthographic-v2 ((frame-size-v2 vec2)
                       (near single-float) (far single-float))
    mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (orthographic (x frame-size-v2) (y frame-size-v2) near far))

;;------------------------------------------------------------
