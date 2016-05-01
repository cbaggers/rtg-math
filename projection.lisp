(in-package :rtg-math.projection)

(defun perspective (frame-width frame-height near far fov)
  (let* ((aspect-ratio (/ frame-width frame-height))
         (near near )
         (far far )
         (fov fov )
         (range (tan (/ fov 2.0)))
         (left (- (* range aspect-ratio)))
         (right (* range aspect-ratio))
         (bottom (- range))
         (top range))
    (m4:make
     (/ (* near 2) (- right left))   0.0                            0.0                                0.0
     0.0                             (/ (* near 2) (- top bottom))  0.0                                0.0
     0.0                             0.0                            (/ (- (+ far near)) (- far near)) -1.0
     0.0                             0.0                            (/ (* 2.0 far near) (- near far))  0.0)))

(defun orthographic (width height near far)
  "Create a raw CFFI orthogonal matrix."
  (rtg-math:m! (/ 2 width) 0.0 0.0 0.0
	       0.0 (/ 2 height) 0.0 0.0
	       0.0 0.0 (/ -2 (- far near)) (- (/ (+ far near) (- far near)))
	       0.0 0.0 0.0 1.0))
