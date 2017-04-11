(in-package :rtg-math.projection)

(defun perspective (frame-width frame-height near far fov)
  (let* ((aspect-ratio (/ frame-width frame-height))
         (near near)
         (far far)
         (fov fov)
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

(defun perspective-v2 (frame-size-v2 near far fov)
  (perspective (x frame-size-v2) (y frame-size-v2) near far fov))

(defun orthographic (frame-width frame-height near far)
  (let ((left (- (/ frame-width 2.0)))
        (right (/ frame-width 2.0))
        (top (/ frame-height 2.0))
        (bottom (- (/ frame-height 2.0)))
        (near near )
        (far far ))
    (m4:make
     (/ 2f0 (- right left)) 0f0                    0f0                   (- (/ (+ right left) (- right left)))
     0f0                    (/ 2f0 (- top bottom)) 0f0                   (- (/ (+ top bottom) (- top bottom)))
     0f0                    0f0                    (/ -2f0 (- far near)) (- (/ (+ far near) (- far near)))
     0f0                    0f0                    0f0                   1f0)))
