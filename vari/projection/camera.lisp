(in-package :rtg-math.projection)

;;------------------------------------------------------------

;; Checked against GLM's perspectiveRH
(vari:v-defun perspective-radian-fov ((width :float) (height :float)
                                      (near :float) (far :float)
                                      (fov :float))
  ;; note: is right handed
  (let* ((aspect (/ width height))
         (tan-half-fov (tan (/ fov 2f0)))
         (result (m4:0!)))
    (setf (m4:melm result 0 0) (/ 1f0 (* aspect tan-half-fov))
          (m4:melm result 1 1) (/ 1f0 tan-half-fov)
          (m4:melm result 2 3) -1f0
          (m4:melm result 2 2) (- (/ (+ far near) (- far near)))
          (m4:melm result 3 2) (- (/ (* 2f0 far near) (- far near))))
    result))


(vari:v-defun perspective ((width :float) (height :float)
                           (near :float) (far :float)
                           (fov-degrees :float))
  (perspective-radian-fov width height near far (radians fov-degrees)))

(vari:v-defun perspective-v2 ((frame-size-v2 :vec2)
                              (near :float) (far :float)
                              (fov-degrees :float))
  (perspective-radian-fov (x frame-size-v2) (y frame-size-v2) near far
                          (radians fov-degrees)))

(vari:v-defun perspective-v2-radian-fov ((frame-size-v2 :vec2)
                                         (near :float) (far :float)
                                         (fov-degrees :float))
  (perspective-radian-fov (x frame-size-v2) (y frame-size-v2) near far
                          fov-degrees))

;;------------------------------------------------------------

(vari:v-defun orthographic ((frame-width :float) (frame-height :float)
                            (near :float) (far :float))
  (let ((left (- (/ frame-width 2.0)))
        (right (/ frame-width 2.0))
        (top (/ frame-height 2.0))
        (bottom (- (/ frame-height 2.0))))
    (m4:make
     (/ 2f0 (- right left)) 0f0                    0f0                   (- (/ (+ right left) (- right left)))
     0f0                    (/ 2f0 (- top bottom)) 0f0                   (- (/ (+ top bottom) (- top bottom)))
     0f0                    0f0                    (/ -2f0 (- far near)) (- (/ (+ far near) (- far near)))
     0f0                    0f0                    0f0                   1f0)))

(vari:v-defun orthographic-v2 ((frame-size-v2 :vec2)
                               (near :float) (far :float))
  (orthographic (x frame-size-v2) (y frame-size-v2) near far))

;;------------------------------------------------------------
