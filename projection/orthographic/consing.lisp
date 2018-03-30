(in-package :rtg-math.projection)

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
